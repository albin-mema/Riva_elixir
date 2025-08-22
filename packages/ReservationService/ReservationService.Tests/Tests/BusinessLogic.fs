module BusinessLogic

open System
open Xunit
open FsUnit.Xunit
open Swensen.Unquote
open ReservationService.Core
open ReservationService.Core.Types
open ReservationService.Core.Configuration

// ---------- Test Configuration Parameters ----------
// Instead of using hardcoded config, create test parameters that would come from Elixir
let private createTestConfigurationParameters () = {
    Currency = { DefaultCurrency = "USD"; SupportedCurrencies = ["USD"; "EUR"] }
    Time = { DefaultTimeZone = "UTC"; SupportedTimeZones = ["UTC"] }
    Retry = { DefaultRetryDelay = 5; MaxRetryAttempts = 3; PaymentDueTime = 24 }
    Money = { DefaultAmount = 100M; DefaultCurrency = "USD"; FallbackCurrencies = ["USD"] }
    Resource = { ResourceConfigurations = Map.empty; DefaultProperties = Map.empty }
    Validation = { AvailableValidationKinds = ["AvailabilityCheck"; "PaymentCheck"]; DefaultValidationTimeout = 300; DefaultRetryBehavior = "Linear" }
    Participant = { AvailableRoles = ["Primary"; "Guest"]; DefaultContactInfo = Map.empty }
    Event = { Source = "TestReservationService" }
    Policy = { AutoApproveThreshold = Some 1000M; MaxParticipants = 10; MinNotice = 2; MaxAdvance = 90; TimeoutPolicy = 48 }
    Message = { ErrorMessages = Map.empty; SuccessMessages = Map.empty; WarningMessages = Map.empty }
}

// ---------- Helper Functions for Test Data Generation ----------
// Updated to not rely on hardcoded configuration
let private createTestReservationId () = ReservationId(Guid.NewGuid())
let private createTestCustomerId () = CustomerId(Guid.NewGuid())
let private createTestResourceId () = ResourceId.Create("test-resource-001") |> Result.defaultWith (fun _ -> failwith "Could not create test resource ID")
let private createTestBusinessId () = BusinessId.Create("test-business-001") |> Result.defaultWith (fun _ -> failwith "Could not create test business ID")
let private createTestServiceId () = ServiceId("test-service-001")
let private createTestRuleId () = RuleId(Guid.NewGuid())
let private createTestCommandId () = CommandId(Guid.NewGuid())
let private createTestValidationId () = ValidationId.Create("test-validation-001") |> Result.defaultWith (fun _ -> failwith "Could not create test validation ID")

let private createTestMoney currency amount = { Amount = amount; Currency = currency }
let private createTestTimeRange start (``end``) = { Start = start; End = ``end`` }
let private createTestZonedTimeRange start (``end``) tz = { Range = createTestTimeRange start ``end``; TimeZoneId = Some tz }

let private createTestParticipant id name role =
    { Id = Some id; Name = Some name; Role = role; ContactInfo = None }

let private createTestResourceDescriptor id type' capacity attributes =
    { Id = id; Type = type'; Capacity = capacity; Attributes = attributes; AvailabilityConstraints = [] }

let private createTestServiceDescriptor id name duration basePrice requiredResources =
    { Id = id; Name = name; Duration = duration; BasePrice = basePrice; Attributes = Map.empty; RequiredResources = requiredResources }

let private createTestValidationState id kind isRequired status deps desc timeout retryPolicy createdAt updatedAt =
    { Id = id; Kind = kind; IsRequired = isRequired; Status = status; Dependencies = deps;
      Description = desc; Timeout = timeout; RetryPolicy = retryPolicy; CreatedAt = createdAt; UpdatedAt = updatedAt }

let private createTestCommandMetadata commandId correlationId userId source =
    { CommandId = commandId; CorrelationId = correlationId; Timestamp = DateTimeOffset.UtcNow; UserId = userId; Source = source }

let private createTestEventMetadata eventId commandId correlationId version =
    { EventId = eventId; CommandId = commandId; CorrelationId = correlationId; Timestamp = DateTimeOffset.UtcNow; Version = version }

let private createTestReservation id request finalTime finalPrice status version validationHistory events =
    { Id = id; Request = request; FinalTime = finalTime; FinalPrice = finalPrice; CreatedAt = request.CreatedAt;
      CreatedBy = request.CreatedBy; Status = status; Version = version; ValidationHistory = validationHistory; Events = events }

// ---------- updateValidationState Tests [<Class>] type UpdateValidationStateTests =
[<Fact>]
let ``updateValidationState should update status when validation ID matches`` () =
    let validationId = createTestValidationId()
    let succeeded = true
    let details = Some "Validation passed"
    let at = DateTimeOffset.UtcNow
    let originalState = createTestValidationState validationId AvailabilityCheck true Pending [] "Check availability" None None DateTimeOffset.UtcNow (DateTimeOffset.UtcNow.AddMinutes(-300.0))
    let expectedState = { originalState with Status = Succeeded(at, details); UpdatedAt = at }
    
    let result = Engine.updateValidationState validationId succeeded details at originalState
    
    test <@ result = expectedState @>

[<Fact>]
let ``updateValidationState should not update status when validation ID doesn't match`` () =
    let validationId = createTestValidationId()
    let otherValidationId = ValidationId "other-validation"
    let succeeded = true
    let details = Some "Validation passed"
    let at = DateTimeOffset.UtcNow
    let originalState = createTestValidationState validationId AvailabilityCheck true Pending [] "Check availability" None None DateTimeOffset.UtcNow (DateTimeOffset.UtcNow.AddMinutes(-300.0))
    
    let result = Engine.updateValidationState otherValidationId succeeded details at originalState
    
    test <@ result = originalState @>

[<Fact>]
let ``updateValidationState should set failed status when succeeded is false`` () =
    let validationId = createTestValidationId()
    let succeeded = false
    let details = Some "Validation failed"
    let at = DateTimeOffset.UtcNow
    let originalState = createTestValidationState validationId PaymentCheck true Pending [] "Payment check" None None DateTimeOffset.UtcNow (DateTimeOffset.UtcNow.AddMinutes(-300.0))
    let expectedState = { originalState with Status = Failed(at, defaultArg details ""); UpdatedAt = at }
    
    let result = Engine.updateValidationState validationId succeeded details at originalState
    
    test <@ match result.Status with | Failed (t, r) when t = at && r = defaultArg details "" -> true | _ -> false @>

[<Fact>]
let ``updateValidationState should use default reason when details is None and succeeded is false`` () =
    let validationId = createTestValidationId()
    let succeeded = false
    let details = None
    let at = DateTimeOffset.UtcNow
    let originalState = createTestValidationState validationId PaymentCheck true Pending [] "Payment check" None None DateTimeOffset.UtcNow (DateTimeOffset.UtcNow.AddMinutes(-300.0))
    let expectedState = { originalState with Status = Failed(at, ""); UpdatedAt = at }
    
    let result = Engine.updateValidationState validationId succeeded details at originalState
    
    test <@ match result.Status with | Failed (t, r) when t = at && r = "" -> true | _ -> false @>

// ---------- getPendingValidations Tests [<Class>] type GetPendingValidationsTests =
[<Fact>]
let ``getPendingValidations should return pending validations`` () =
    let validationId1 = ValidationId "pending-1"
    let validationId2 = ValidationId "pending-2"
    let validationId3 = ValidationId "completed-1"
    let validationId4 = ValidationId "failed-1"
    
    let pendingValidation1 = createTestValidationState validationId1 AvailabilityCheck true Pending [] "Availability check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let pendingValidation2 = createTestValidationState validationId2 PaymentCheck true (InProgress (DateTimeOffset.UtcNow, None)) [] "Payment check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let completedValidation = createTestValidationState validationId3 CustomerProfileCheck true (Succeeded (DateTimeOffset.UtcNow, None)) [] "Customer profile" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let failedValidation = createTestValidationState validationId4 BusinessRuleCheck true (Failed (DateTimeOffset.UtcNow, "Rule failed")) [] "Business rule" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let request = {
        Id = None
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [pendingValidation1; pendingValidation2; completedValidation; failedValidation]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let reservation = createTestReservation (createTestReservationId()) request (request.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    
    let pendingValidations = Engine.getPendingValidations reservation
    
    test <@ List.length pendingValidations = 2 @>
    test <@ List.contains pendingValidation1 pendingValidations @>
    test <@ List.contains pendingValidation2 pendingValidations @>
    test <@ not (List.contains completedValidation pendingValidations) @>
    test <@ not (List.contains failedValidation pendingValidations) @>

[<Fact>]
let ``getPendingValidations should filter out validations with failed dependencies`` () =
    let validationId1 = ValidationId "validation-1"
    let validationId2 = ValidationId "validation-2"
    let dependencyId = ValidationId "dependency-1"
    
    let dependency = createTestValidationState dependencyId AvailabilityCheck true (Failed (DateTimeOffset.UtcNow, "Failed")) [] "Availability check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let validationWithDependency = createTestValidationState validationId1 PaymentCheck true Pending [{ ValidationId = dependencyId; MustSucceed = true }] "Payment check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let independentValidation = createTestValidationState validationId2 CustomerProfileCheck true Pending [] "Customer profile" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let request = {
        Id = None
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [dependency; validationWithDependency; independentValidation]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let reservation = createTestReservation (createTestReservationId()) request (request.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    
    let pendingValidations = Engine.getPendingValidations reservation
    
    test <@ List.length pendingValidations = 1 @>
    test <@ not (List.contains validationWithDependency pendingValidations) @>
    test <@ List.contains independentValidation pendingValidations @>

[<Fact>]
let ``getPendingValidations should include validations with informational dependencies`` () =
    let validationId1 = ValidationId "validation-1"
    let validationId2 = ValidationId "validation-2"
    let dependencyId = ValidationId "dependency-1"
    
    let dependency = createTestValidationState dependencyId AvailabilityCheck true (Failed (DateTimeOffset.UtcNow, "Failed")) [] "Availability check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let validationWithInfoDependency = createTestValidationState validationId1 PaymentCheck true Pending [{ ValidationId = dependencyId; MustSucceed = false }] "Payment check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let independentValidation = createTestValidationState validationId2 CustomerProfileCheck true Pending [] "Customer profile" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let request = {
        Id = None
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [dependency; validationWithInfoDependency; independentValidation]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let reservation = createTestReservation (createTestReservationId()) request (request.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    
    let pendingValidations = Engine.getPendingValidations reservation
    
    test <@ List.length pendingValidations = 2 @>
    test <@ List.contains validationWithInfoDependency pendingValidations @>
    test <@ List.contains independentValidation pendingValidations @>

[<Fact>]
let ``getPendingValidations should return empty list when no pending validations`` () =
    let completedValidation = createTestValidationState (ValidationId "completed-1") AvailabilityCheck true (Succeeded (DateTimeOffset.UtcNow, None)) [] "Availability check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let failedValidation = createTestValidationState (ValidationId "failed-1") PaymentCheck true (Failed (DateTimeOffset.UtcNow, "Failed")) [] "Payment check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let request = {
        Id = None
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [completedValidation; failedValidation]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let reservation = createTestReservation (createTestReservationId()) request (request.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    
    let pendingValidations = Engine.getPendingValidations reservation
    
    test <@ List.isEmpty pendingValidations @>

// ---------- applyEvent Tests [<Class>] type ApplyEventTests =
[<Fact>]
let ``applyEvent should create reservation from ReservationCreated event`` () =
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let request = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = requestedTime
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 1UL
    let event' = ReservationCreated (reservationId, request, metadata)
    
    let initialState = {
        Id = ReservationId Guid.Empty
        Request = {
            Id = None
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = []
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = 0UL
        }
        FinalTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        FinalPrice = Money.Zero "EUR"
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Status = Draft
        Version = 0UL
        ValidationHistory = []
        Events = []
    }
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ newState.Id = reservationId @>
        test <@ newState.Request = request @>
        test <@ newState.FinalTime = requestedTime @>
        test <@ newState.FinalPrice = Money.Zero "EUR" @>
        test <@ newState.Status = Draft @>
        test <@ newState.Version = 1UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should update reservation from ReservationUpdated event`` () =
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = requestedTime
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let updatedRequest = {
        originalRequest with
            SpecialRequests = Some "Window seat preferred"
            Version = 2UL
    }
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 2UL
    let event' = ReservationUpdated (reservationId, updatedRequest, metadata)
    
    let initialState = createTestReservation reservationId originalRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ newState.Request = updatedRequest @>
        test <@ newState.Version = 2UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should start validation from ValidationStarted event`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    let validationState = createTestValidationState validationId AvailabilityCheck true Pending [] "Check availability" None None DateTimeOffset.UtcNow (DateTimeOffset.UtcNow.AddMinutes(-300.0))
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 2UL
    let event' = ValidationStarted (reservationId, validationState, metadata)
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [validationState]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ newState.Request.ValidationStates.Head.Status = InProgress (metadata.Timestamp, None) @>
        test <@ newState.Version = 2UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should complete validation from ValidationCompleted event`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    let completedValidationState = createTestValidationState validationId AvailabilityCheck true (Succeeded (DateTimeOffset.UtcNow, Some "Available")) [] "Check availability" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 3UL
    let event' = ValidationCompleted (reservationId, completedValidationState, metadata)
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [completedValidationState]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 2UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 2UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ newState.Request.ValidationStates.Head = completedValidationState @>
        test <@ List.contains completedValidationState newState.ValidationHistory @>
        test <@ newState.Version = 3UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should set status to PendingPayment from TermsAccepted event`` () =
    let reservationId = createTestReservationId()
    let acceptedBy = "customer@example.com"
    let at = DateTimeOffset.UtcNow
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 4UL
    let event' = TermsAccepted (reservationId, acceptedBy, at, metadata)
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 3UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 3UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ match newState.Status with | PendingPayment (due, _) when due = at.AddHours 24.0 -> true | _ -> false @>
        test <@ newState.Version = 4UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should set status to Confirmed from ReservationConfirmed event`` () =
    let reservationId = createTestReservationId()
    let confirmedBy = Some "manager@business.com"
    let at = DateTimeOffset.UtcNow
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 5UL
    let event' = ReservationConfirmed (reservationId, confirmedBy, at, metadata)
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 4UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 4UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ match newState.Status with | Confirmed (confAt, confBy) when confAt = at && confBy = confirmedBy -> true | _ -> false @>
        test <@ newState.Version = 5UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should set status to Cancelled from ReservationCancelled event`` () =
    let reservationId = createTestReservationId()
    let reason = CustomerCancelled (None, DateTimeOffset.UtcNow, None)
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 6UL
    let event' = ReservationCancelled (reservationId, reason, metadata)
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 5UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 5UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ match newState.Status with | Cancelled r when r = reason -> true | _ -> false @>
        test <@ newState.Version = 6UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should record change request from ReservationChangeRequested event`` () =
    let reservationId = createTestReservationId()
    let newTime = Some (createTestZonedTimeRange (DateTimeOffset.UtcNow.AddHours(2.0)) (DateTimeOffset.UtcNow.AddHours(4.0)) "UTC")
    let reason = "Change of schedule"
    let changeRequest = { NewTime = newTime; NewParticipants = None; NewServices = None; Reason = reason }
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 7UL
    let event' = ReservationChangeRequested (reservationId, changeRequest, metadata)
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 6UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 6UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ newState.Version = 7UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should update final time from ReservationChangeApplied event`` () =
    let reservationId = createTestReservationId()
    let newTime = createTestZonedTimeRange (DateTimeOffset.UtcNow.AddHours(2.0)) (DateTimeOffset.UtcNow.AddHours(4.0)) "UTC"
    let reason = "Change of schedule"
    let changeRequest = { NewTime = Some newTime; NewParticipants = None; NewServices = None; Reason = reason }
    let appliedAt = DateTimeOffset.UtcNow
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 8UL
    let event' = ReservationChangeApplied (reservationId, changeRequest, appliedAt, metadata)
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 7UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 7UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ newState.FinalTime = newTime @>
        test <@ newState.Version = 8UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

[<Fact>]
let ``applyEvent should set status to Cancelled from ReservationTimedOut event`` () =
    let reservationId = createTestReservationId()
    let at = DateTimeOffset.UtcNow
    
    let metadata = createTestEventMetadata (Guid.NewGuid()) (createTestCommandId()) None 9UL
    let event' = ReservationTimedOut (reservationId, at, metadata)
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 8UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 8UL [] []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ match newState.Status with | Cancelled (Timeout timeoutAt) when timeoutAt = at -> true | _ -> false @>
        test <@ newState.Version = 9UL @>
        test <@ List.contains event' newState.Events @>
    | Error messages ->
        failwithf "applyEvent should not return error: %A" messages

// ---------- processPartialValidation Tests [<Class>] type ProcessPartialValidationTests =
[<Fact>]
let ``processPartialValidation should update validation state`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    let succeeded = true
    let details = Some "Validation passed"
    let at = DateTimeOffset.UtcNow
    
    let validationState = createTestValidationState validationId AvailabilityCheck true Pending [] "Check availability" None None DateTimeOffset.UtcNow (DateTimeOffset.UtcNow.AddMinutes(-300.0))
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [validationState]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    
    match Engine.processPartialValidation validationId succeeded details initialState with
    | Ok newState ->
        test <@ newState.Request.ValidationStates.Head.Status = Succeeded(at, details) @>
        test <@ newState.Request.ValidationStates.Head.UpdatedAt = at @>
    | Error messages ->
        failwithf "processPartialValidation should not return error: %A" messages

[<Fact>]
let ``processPartialValidation should not update validation state when ID doesn't match`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    let otherValidationId = ValidationId "other-validation"
    let succeeded = true
    let details = Some "Validation passed"
    
    let validationState = createTestValidationState validationId AvailabilityCheck true Pending [] "Check availability" None None DateTimeOffset.UtcNow (DateTimeOffset.UtcNow.AddMinutes(-300.0))
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [validationState]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    
    match Engine.processPartialValidation otherValidationId succeeded details initialState with
    | Ok newState ->
        test <@ newState.Request.ValidationStates.Head = validationState @>
    | Error messages ->
        failwithf "processPartialValidation should not return error: %A" messages

[<Fact>]
let ``processPartialValidation should set failed status when succeeded is false`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    let succeeded = false
    let details = Some "Validation failed"
    let at = DateTimeOffset.UtcNow
    
    let validationState = createTestValidationState validationId PaymentCheck true Pending [] "Payment check" None None DateTimeOffset.UtcNow (DateTimeOffset.UtcNow.AddMinutes(-300.0))
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [validationState]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let initialState = createTestReservation reservationId originalRequest (originalRequest.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    
    match Engine.processPartialValidation validationId succeeded details initialState with
    | Ok newState ->
        test <@ match newState.Request.ValidationStates.Head.Status with | Failed (t, r) when t = at && r = defaultArg details "" -> true | _ -> false @>
    | Error messages ->
        failwithf "processPartialValidation should not return error: %A" messages

// ---------- createSnapshot Tests [<Class>] type CreateSnapshotTests =
[<Fact>]
let ``createSnapshot should create snapshot with reservation state`` () =
    let reservationId = createTestReservationId()
    let request = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 5UL
    }
    
    let reservation = createTestReservation reservationId request (request.RequestedTime) (Money.Zero "EUR") Confirmed (DateTimeOffset.UtcNow, Some "manager") 5UL [] []
    let timestamp = DateTimeOffset.UtcNow
    let sinceVersion = Some 3UL
    
    let snapshot = Engine.createSnapshot reservation sinceVersion timestamp
    
    test <@ snapshot.ReservationId = reservationId @>
    test <@ snapshot.State = reservation @>
    test <@ snapshot.Version = 5UL @>
    test <@ snapshot.Timestamp = timestamp @>
    test <@ snapshot.EventIds = reservation.Events |> List.choose (fun ev ->
        match ev with
        | ReservationCreated (_,_,m)
        | ReservationUpdated (_,_,m)
        | ValidationStarted (_,_,m)
        | ValidationCompleted (_,_,m)
        | TermsAccepted (_,_,_,m)
        | ReservationConfirmed (_,_,_,m)
        | ReservationCancelled (_,_,m)
        | ReservationChangeRequested (_,_,m)
        | ReservationChangeApplied (_,_,_,m)
        | ReservationTimedOut (_,_,m) -> Some m.EventId
        | _ -> None) @>

[<Fact>]
let ``createSnapshot should include all events when sinceVersion is None`` () =
    let reservationId = createTestReservationId()
    let request = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 3UL
    }
    
    let event1 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 1UL })
    let event2 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 2UL })
    let event3 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 3UL })
    
    let reservation = createTestReservation reservationId request (request.RequestedTime) (Money.Zero "EUR") Confirmed (DateTimeOffset.UtcNow, Some "manager") 3UL [] [event1; event2; event3]
    let timestamp = DateTimeOffset.UtcNow
    let sinceVersion = None
    
    let snapshot = Engine.createSnapshot reservation sinceVersion timestamp
    
    test <@ List.length snapshot.EventIds = 3 @>
    test <@ List.contains event1.EventId snapshot.EventIds @>
    test <@ List.contains event2.EventId snapshot.EventIds @>
    test <@ List.contains event3.EventId snapshot.EventIds @>

[<Fact>]
let ``createSnapshot should include only recent events when sinceVersion is specified`` () =
    let reservationId = createTestReservationId()
    let request = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 5UL
    }
    
    let event1 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 1UL })
    let event2 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 2UL })
    let event3 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 3UL })
    let event4 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 4UL })
    let event5 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 5UL })
    
    let reservation = createTestReservation reservationId request (request.RequestedTime) (Money.Zero "EUR") Confirmed (DateTimeOffset.UtcNow, Some "manager") 5UL [] [event1; event2; event3; event4; event5]
    let timestamp = DateTimeOffset.UtcNow
    let sinceVersion = Some 3UL
    
    let snapshot = Engine.createSnapshot reservation sinceVersion timestamp
    
    test <@ List.length snapshot.EventIds = 3 @>
    test <@ not (List.contains event1.EventId snapshot.EventIds) @>
    test <@ not (List.contains event2.EventId snapshot.EventIds) @>
    test <@ not (List.contains event3.EventId snapshot.EventIds) @>
    test <@ List.contains event4.EventId snapshot.EventIds @>
    test <@ List.contains event5.EventId snapshot.EventIds @>

// ---------- replay Tests [<Class>] type ReplayTests =
[<Fact>]
let ``replay should apply events in order`` () =
    let reservationId = createTestReservationId()
    let request = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 0UL
    }
    
    let initialState = createTestReservation reservationId request (request.RequestedTime) (Money.Zero "EUR") Draft 0UL [] []
    
    let event1 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 1UL })
    let event2 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 2UL })
    let event3 = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 3UL })
    
    let events = [event1; event2; event3]
    
    match Engine.replay initialState events with
    | Ok finalState ->
        test <@ finalState.Version = 3UL @>
        test <@ List.contains event1 finalState.Events @>
        test <@ List.contains event2 finalState.Events @>
        test <@ List.contains event3 finalState.Events @>
        test <@ match finalState.Status with | Confirmed _ -> true | _ -> false @>
    | Error messages ->
        failwithf "replay should not return error: %A" messages

[<Fact>]
let ``replay should handle empty event list`` () =
    let reservationId = createTestReservationId()
    let request = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let initialState = createTestReservation reservationId request (request.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    let events = []
    
    match Engine.replay initialState events with
    | Ok finalState ->
        test <@ finalState = initialState @>
    | Error messages ->
        failwithf "replay should not return error: %A" messages

[<Fact>]
let ``replay should stop on first error`` () =
    let reservationId = createTestReservationId()
    let request = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 0UL
    }
    
    let initialState = createTestReservation reservationId request (request.RequestedTime) (Money.Zero "EUR") Draft 0UL [] []
    
    // Create a valid event
    let validEvent = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 1UL })
    
    // Create an invalid event that will cause version mismatch
    let invalidEvent = ReservationConfirmed (reservationId, Some "manager", DateTimeOffset.UtcNow, { EventId = Guid.NewGuid(); CommandId = createTestCommandId(); CorrelationId = None; Timestamp = DateTimeOffset.UtcNow; Version = 3UL }) // Version jumps from 1 to 3
    
    let events = [validEvent; invalidEvent]
    
    match Engine.replay initialState events with
    | Ok finalState ->
        failwith "replay should return error for version mismatch"
    | Error messages ->
        test <@ not (List.isEmpty messages) @>

// ---------- applyCommand Tests [<Class>] type ApplyCommandTests =
[<Fact>]
let ``applyCommand should create reservation from CreateReservation command when no existing reservation`` () =
    let reservationId = ReservationId(Guid.NewGuid())
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let request = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = requestedTime
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = CreateReservation (request, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None command currentTime with
    | Ok events ->
        test <@ List.length events = 1 @>
        test <@ match events.Head with | ReservationCreated (rid, req, meta) when rid = reservationId && req = request && meta.Version = 1UL -> true | _ -> false @>
    | Error error ->
        failwithf "applyCommand should not return error: %s" error

[<Fact>]
let ``applyCommand should return error when creating reservation that already exists`` () =
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let request = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = requestedTime
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let existingReservation = createTestReservation reservationId request requestedTime (Money.Zero "EUR") Draft 1UL [] []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = CreateReservation (request, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Error error ->
        test <@ error = "Reservation already exists" @>
    | Ok events ->
        failwithf "applyCommand should return error: %A" events

[<Fact>]
let ``applyCommand should update reservation from UpdateReservation command when reservation exists`` () =
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let originalRequest = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = requestedTime
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let updatedRequest = {
        originalRequest with
            SpecialRequests = Some "Window seat preferred"
            Version = 2UL
    }
    
    let existingReservation = createTestReservation reservationId originalRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = UpdateReservation (reservationId, updatedRequest, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Ok events ->
        test <@ List.length events = 1 @>
        test <@ match events.Head with | ReservationUpdated (rid, req, meta) when rid = reservationId && req = updatedRequest && meta.Version = 2UL -> true | _ -> false @>
    | Error error ->
        failwithf "applyCommand should not return error: %s" error

[<Fact>]
let ``applyCommand should return error when updating non-existent reservation`` () =
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let request = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = requestedTime
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 2UL
    }
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = UpdateReservation (reservationId, request, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None command currentTime with
    | Error error ->
        test <@ error = "Reservation not found" @>
    | Ok events ->
        failwithf "applyCommand should return error: %A" events

[<Fact>]
let ``applyCommand should accept terms from AcceptTerms command`` () =
    let reservationId = createTestReservationId()
    let acceptedBy = "customer@example.com"
    let at = DateTimeOffset.UtcNow
    
    let existingReservation = createTestReservation reservationId 
        {
            Id = Some reservationId
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = []
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = 1UL
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        1UL 
        [] 
        []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = AcceptTerms (reservationId, acceptedBy, at, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Ok events ->
        test <@ List.length events = 1 @>
        test <@ match events.Head with | TermsAccepted (rid, by, a, meta) when rid = reservationId && by = acceptedBy && a = at && meta.Version = 2UL -> true | _ -> false @>
    | Error error ->
        failwithf "applyCommand should not return error: %s" error

[<Fact>]
let ``applyCommand should confirm reservation from ConfirmReservation command`` () =
    let reservationId = createTestReservationId()
    let confirmedBy = Some "manager@business.com"
    let at = DateTimeOffset.UtcNow
    
    let existingReservation = createTestReservation reservationId 
        {
            Id = Some reservationId
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = []
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = 2UL
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        2UL 
        [] 
        []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = ConfirmReservation (reservationId, confirmedBy, at, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Ok events ->
        test <@ List.length events = 1 @>
        test <@ match events.Head with | ReservationConfirmed (rid, by, a, meta) when rid = reservationId && by = confirmedBy && a = at && meta.Version = 3UL -> true | _ -> false @>
    | Error error ->
        failwithf "applyCommand should not return error: %s" error

[<Fact>]
let ``applyCommand should cancel reservation from CancelReservation command`` () =
    let reservationId = createTestReservationId()
    let reason = CustomerCancelled (None, DateTimeOffset.UtcNow, None)
    
    let existingReservation = createTestReservation reservationId 
        {
            Id = Some reservationId
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = []
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = 3UL
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        3UL 
        [] 
        []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = CancelReservation (reservationId, reason, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Ok events ->
        test <@ List.length events = 1 @>
        test <@ match events.Head with | ReservationCancelled (rid, r, meta) when rid = reservationId && r = reason && meta.Version = 4UL -> true | _ -> false @>
    | Error error ->
        failwithf "applyCommand should not return error: %s" error

[<Fact>]
let ``applyCommand should request reservation change from RequestReservationChange command`` () =
    let reservationId = createTestReservationId()
    let newTime = Some (createTestZonedTimeRange (DateTimeOffset.UtcNow.AddHours(2.0)) (DateTimeOffset.UtcNow.AddHours(4.0)) "UTC")
    let reason = "Change of schedule"
    let changeRequest = { NewTime = newTime; NewParticipants = None; NewServices = None; Reason = reason }
    
    let existingReservation = createTestReservation reservationId 
        {
            Id = Some reservationId
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = []
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = 4UL
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        4UL 
        [] 
        []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = RequestReservationChange (reservationId, changeRequest, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Ok events ->
        test <@ List.length events = 1 @>
        test <@ match events.Head with | ReservationChangeRequested (rid, change, meta) when rid = reservationId && change = changeRequest && meta.Version = 5UL -> true | _ -> false @>
    | Error error ->
        failwithf "applyCommand should not return error: %s" error

[<Fact>]
let ``applyCommand should start validation from StartValidation command`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    let startedBy = Some "system"
    
    let validationState = createTestValidationState validationId AvailabilityCheck true Pending [] "Check availability" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let existingReservation = createTestReservation reservationId 
        {
            Id = Some reservationId
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = [validationState]
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = 5UL
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        5UL 
        [] 
        []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = StartValidation (reservationId, validationId, startedBy, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Ok events ->
        test <@ List.length events = 1 @>
        test <@ match events.Head with | ValidationStarted (rid, vs, meta) when rid = reservationId && vs.Id = validationId && meta.Version = 6UL -> true | _ -> false @>
    | Error error ->
        failwithf "applyCommand should not return error: %s" error

[<Fact>]
let ``applyCommand should complete validation from CompleteValidation command`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    let succeeded = true
    let details = Some "Validation passed"
    let at = DateTimeOffset.UtcNow
    
    let validationState = createTestValidationState validationId PaymentCheck true Pending [] "Payment check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let existingReservation = createTestReservation reservationId 
        {
            Id = Some reservationId
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = [validationState]
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = 6UL
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        6UL 
        [] 
        []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = CompleteValidation (reservationId, validationId, succeeded, details, at, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Ok events ->
        test <@ List.length events = 1 @>
        test <@ match events.Head with | ValidationCompleted (rid, vs, meta) when rid = reservationId && vs.Id = validationId && meta.Version = 7UL -> true | _ -> false @>
    | Error error ->
        failwithf "applyCommand should not return error: %s" error

[<Fact>]
let ``applyCommand should return error for invalid command/state combination`` () =
    let reservationId = createTestReservationId()
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = ConfirmReservation (reservationId, None, DateTimeOffset.UtcNow, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None command currentTime with
    | Error error ->
        test <@ error = "Invalid command/state" @>
    | Ok events ->
        failwithf "applyCommand should return error: %A" events

// ---------- Property-based tests using Unquote [<Class>] type PropertyTests =
[<Property>]
let ``updateValidationState is idempotent for same validation`` (validationId: string) (succeeded: bool) (details: string option) (at: DateTimeOffset) =
    let validationId' = ValidationId validationId
    let originalState = createTestValidationState validationId' AvailabilityCheck true Pending [] "Check availability" None None (at.AddHours(-1.0)) (at.AddMinutes(-300.0))
    let result1 = Engine.updateValidationState validationId' succeeded details at originalState
    let result2 = Engine.updateValidationState validationId' succeeded details at result1
    test <@ result1 = result2 @>

[<Property>]
let ``getPendingValidations returns subset of all validations`` (validations: ValidationState list) =
    if List.isEmpty validations then () else // Skip empty list to avoid trivial case
    let reservationId = createTestReservationId()
    let request = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = validations
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    let reservation = createTestReservation reservationId request (request.RequestedTime) (Money.Zero "EUR") Draft 1UL [] []
    let pending = Engine.getPendingValidations reservation
    test <@ List.forall (fun v -> List.contains v validations) pending @>

[<Property>]
let ``applyEvent preserves reservation ID`` (reservationId: string) (event': Event) =
    let reservationId' = ReservationId(Guid.Parse reservationId)
    let initialState = createTestReservation reservationId' 
        {
            Id = Some reservationId'
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = []
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = 1UL
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        1UL 
        [] 
        []
    
    match Engine.applyEvent initialState event' with
    | Ok newState ->
        test <@ newState.Id = reservationId' @>
    | Error _ ->
        test <@ true @> // Some events might be invalid for the state, which is OK

[<Property>]
let ``createSnapshot version matches reservation version`` (version: uint64) (timestamp: DateTimeOffset) =
    let reservationId = createTestReservationId()
    let request = {
        Id = Some reservationId
        CustomerId = createTestCustomerId()
        BusinessId = createTestBusinessId()
        Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
        RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = []
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = version
    }
    let reservation = createTestReservation reservationId request (request.RequestedTime) (Money.Zero "EUR") Draft version [] []
    let snapshot = Engine.createSnapshot reservation None timestamp
    test <@ snapshot.Version = version @>

[<Property>]
let ``applyCommand version increments correctly`` (currentVersion: uint64) =
    let reservationId = createTestReservationId()
    let existingReservation = createTestReservation reservationId 
        {
            Id = Some reservationId
            CustomerId = createTestCustomerId()
            BusinessId = createTestBusinessId()
            Resource = createTestResourceDescriptor (createTestResourceId()) Room (Some 10) Map.empty
            RequestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
            Services = []
            Participants = []
            SpecialRequests = None
            Metadata = Map.empty
            ValidationStates = []
            InterimPrice = None
            AdjustedTime = None
            CreatedAt = DateTimeOffset.UtcNow
            CreatedBy = None
            Version = currentVersion
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        currentVersion 
        [] 
        []
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let command = ConfirmReservation (reservationId, None, DateTimeOffset.UtcNow, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) command currentTime with
    | Ok events ->
        test <@ List.exists (fun ev -> match ev with | ReservationConfirmed (_,_,_,meta) when meta.Version = currentVersion + 1UL -> true | _ -> false) events @>
    | Error _ ->
        test <@ false @> // This should not happen for a valid command