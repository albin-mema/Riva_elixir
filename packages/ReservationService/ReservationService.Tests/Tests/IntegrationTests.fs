module IntegrationTests

open System
open Xunit
open FsUnit.Xunit
open Swensen.Unquote
open ReservationService.Core
open ReservationService.Core.Types

// ---------- Helper Functions for Test Data Generation ----------
let private createTestReservationId () = ReservationId(Guid.NewGuid())
let private createTestCustomerId () = CustomerId(Guid.NewGuid())
let private createTestResourceId () = ResourceId("test-resource-123")
let private createTestBusinessId () = BusinessId("test-business-456")
let private createTestServiceId () = ServiceId("test-service-789")
let private createTestRuleId () = RuleId(Guid.NewGuid())
let private createTestCommandId () = CommandId(Guid.NewGuid())
let private createTestValidationId () = ValidationId("validation-123")

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

// ---------- Complete Reservation Lifecycle Tests [<Class>] type ReservationLifecycleTests =
[<Fact>]
let ``Complete reservation lifecycle from creation to confirmation`` () =
    // Step 1: Create reservation
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let initialRequest = {
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
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        test <@ List.length createEvents = 1 @>
        test <@ match createEvents.Head with | ReservationCreated (rid, req, meta) when rid = reservationId && req = initialRequest && meta.Version = 1UL -> true | _ -> false @>
        
        // Step 2: Apply create event to get initial state
        let initialState = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            test <@ stateAfterCreate.Status = Draft @>
            test <@ stateAfterCreate.Version = 1UL @>
            
            // Step 3: Accept terms
            let acceptedBy = "customer@example.com"
            let acceptedAt = DateTimeOffset.UtcNow
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let acceptTermsCommand = AcceptTerms (reservationId, acceptedBy, acceptedAt, metadata2)
            
            match Engine.applyCommand (Some stateAfterCreate) acceptTermsCommand currentTime with
            | Ok acceptEvents ->
                test <@ List.length acceptEvents = 1 @>
                test <@ match acceptEvents.Head with | TermsAccepted (rid, by, at, meta) when rid = reservationId && by = acceptedBy && at = acceptedAt && meta.Version = 2UL -> true | _ -> false @>
                
                // Step 4: Apply terms accepted event
                match Engine.applyEvent stateAfterCreate acceptEvents.Head with
                | Ok stateAfterTerms ->
                    test <@ match stateAfterTerms.Status with | PendingPayment (due, _) when due = acceptedAt.AddHours 24.0 -> true | _ -> false @>
                    test <@ stateAfterTerms.Version = 2UL @>
                    
                    // Step 5: Confirm reservation
                    let confirmedBy = Some "manager@business.com"
                    let confirmedAt = DateTimeOffset.UtcNow
                    let commandId3 = createTestCommandId()
                    let metadata3 = createTestCommandMetadata commandId3 None None "test"
                    let confirmCommand = ConfirmReservation (reservationId, confirmedBy, confirmedAt, metadata3)
                    
                    match Engine.applyCommand (Some stateAfterTerms) confirmCommand currentTime with
                    | Ok confirmEvents ->
                        test <@ List.length confirmEvents = 1 @>
                        test <@ match confirmEvents.Head with | ReservationConfirmed (rid, by, at, meta) when rid = reservationId && by = confirmedBy && at = confirmedAt && meta.Version = 3UL -> true | _ -> false @>
                        
                        // Step 6: Apply confirmed event
                        match Engine.applyEvent stateAfterTerms confirmEvents.Head with
                        | Ok finalState ->
                            test <@ match finalState.Status with | Confirmed (confAt, confBy) when confAt = confirmedAt && confBy = confirmedBy -> true | _ -> false @>
                            test <@ finalState.Version = 3UL @>
                            test <@ finalState.Id = reservationId @>
                            test <@ finalState.Request = initialRequest @>
                        | Error messages ->
                            failwithf "Failed to apply confirmation event: %A" messages
                    | Error error ->
                        failwithf "Failed to confirm reservation: %s" error
                | Error messages ->
                    failwithf "Failed to apply terms accepted event: %A" messages
            | Error error ->
                failwithf "Failed to accept terms: %s" error
        | Error messages ->
            failwithf "Failed to apply create event: %A" messages
    | Error error ->
        failwithf "Failed to create reservation: %s" error

[<Fact>]
let ``Reservation lifecycle with validation workflow`` () =
    // Step 1: Create reservation with validations
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let availabilityValidation = createTestValidationState (ValidationId "availability-1") AvailabilityCheck true Pending [] "Check availability" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let paymentValidation = createTestValidationState (ValidationId "payment-1") PaymentCheck true Pending [] "Payment check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let customerProfileValidation = createTestValidationState (ValidationId "profile-1") CustomerProfileCheck true Pending [] "Customer profile" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let initialRequest = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = requestedTime
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [availabilityValidation; paymentValidation; customerProfileValidation]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        // Step 2: Apply create event to get initial state
        let initialState = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            test <@ stateAfterCreate.Status = Draft @>
            test <@ stateAfterCreate.Version = 1UL @>
            
            // Step 3: Start availability validation
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let startValidationCommand = StartValidation (reservationId, availabilityValidation.Id, None, metadata2)
            
            match Engine.applyCommand (Some stateAfterCreate) startValidationCommand currentTime with
            | Ok startEvents ->
                test <@ List.length startEvents = 1 @>
                test <@ match startEvents.Head with | ValidationStarted (rid, vs, meta) when rid = reservationId && vs.Id = availabilityValidation.Id && meta.Version = 2UL -> true | _ -> false @>
                
                // Step 4: Apply validation started event
                match Engine.applyEvent stateAfterCreate startEvents.Head with
                | Ok stateAfterStart ->
                    test <@ stateAfterStart.Version = 2UL @>
                    
                    // Step 5: Complete availability validation successfully
                    let completedAvailabilityValidation = createTestValidationState availabilityValidation.Id availabilityValidation.Kind availabilityValidation.IsRequired (Succeeded (DateTimeOffset.UtcNow, Some "Available")) availabilityValidation.Dependencies availabilityValidation.Description availabilityValidation.Timeout availabilityValidation.RetryPolicy availabilityValidation.CreatedAt DateTimeOffset.UtcNow
                    let commandId3 = createTestCommandId()
                    let metadata3 = createTestCommandMetadata commandId3 None None "test"
                    let completeValidationCommand = CompleteValidation (reservationId, availabilityValidation.Id, true, Some "Available", DateTimeOffset.UtcNow, metadata3)
                    
                    match Engine.applyCommand (Some stateAfterStart) completeValidationCommand currentTime with
                    | Ok completeEvents ->
                        test <@ List.length completeEvents = 1 @>
                        test <@ match completeEvents.Head with | ValidationCompleted (rid, vs, meta) when rid = reservationId && vs.Id = availabilityValidation.Id && meta.Version = 3UL -> true | _ -> false @>
                        
                        // Step 6: Apply validation completed event
                        match Engine.applyEvent stateAfterStart completeEvents.Head with
                        | Ok stateAfterComplete ->
                            test <@ stateAfterComplete.Version = 3UL @>
                            test <@ List.contains completedAvailabilityValidation stateAfterComplete.ValidationHistory @>
                            
                            // Step 7: Start payment validation
                            let commandId4 = createTestCommandId()
                            let metadata4 = createTestCommandMetadata commandId4 None None "test"
                            let startPaymentCommand = StartValidation (reservationId, paymentValidation.Id, None, metadata4)
                            
                            match Engine.applyCommand (Some stateAfterComplete) startPaymentCommand currentTime with
                            | Ok paymentStartEvents ->
                                test <@ List.length paymentStartEvents = 1 @>
                                
                                // Step 8: Complete payment validation successfully
                                let completedPaymentValidation = createTestValidationState paymentValidation.Id paymentValidation.Kind paymentValidation.IsRequired (Succeeded (DateTimeOffset.UtcNow, Some "Payment processed")) paymentValidation.Dependencies paymentValidation.Description paymentValidation.Timeout paymentValidation.RetryPolicy paymentValidation.CreatedAt DateTimeOffset.UtcNow
                                let commandId5 = createTestCommandId()
                                let metadata5 = createTestCommandMetadata commandId5 None None "test"
                                let completePaymentCommand = CompleteValidation (reservationId, paymentValidation.Id, true, Some "Payment processed", DateTimeOffset.UtcNow, metadata5)
                                
                                match Engine.applyCommand (Some stateAfterComplete) completePaymentCommand currentTime with
                                | Ok paymentCompleteEvents ->
                                    test <@ List.length paymentCompleteEvents = 1 @>
                                    
                                    // Step 9: Apply payment validation completed event
                                    match Engine.applyEvent stateAfterComplete paymentCompleteEvents.Head with
                                    | Ok stateAfterPayment ->
                                        test <@ stateAfterPayment.Version = 4UL @>
                                        test <@ List.contains completedPaymentValidation stateAfterPayment.ValidationHistory @>
                                        
                                        // Step 10: Start customer profile validation
                                        let commandId6 = createTestCommandId()
                                        let metadata6 = createTestCommandMetadata commandId6 None None "test"
                                        let startProfileCommand = StartValidation (reservationId, customerProfileValidation.Id, None, metadata6)
                                        
                                        match Engine.applyCommand (Some stateAfterPayment) startProfileCommand currentTime with
                                        | Ok profileStartEvents ->
                                            test <@ List.length profileStartEvents = 1 @>
                                            
                                            // Step 11: Complete customer profile validation successfully
                                            let completedProfileValidation = createTestValidationState customerProfileValidation.Id customerProfileValidation.Kind customerProfileValidation.IsRequired (Succeeded (DateTimeOffset.UtcNow, Some "Profile verified")) customerProfileValidation.Dependencies customerProfileValidation.Description customerProfileValidation.Timeout customerProfileValidation.RetryPolicy customerProfileValidation.CreatedAt DateTimeOffset.UtcNow
                                            let commandId7 = createTestCommandId()
                                            let metadata7 = createTestCommandMetadata commandId7 None None "test"
                                            let completeProfileCommand = CompleteValidation (reservationId, customerProfileValidation.Id, true, Some "Profile verified", DateTimeOffset.UtcNow, metadata7)
                                            
                                            match Engine.applyCommand (Some stateAfterPayment) completeProfileCommand currentTime with
                                            | Ok profileCompleteEvents ->
                                                test <@ List.length profileCompleteEvents = 1 @>
                                                
                                                // Step 12: Apply profile validation completed event
                                                match Engine.applyEvent stateAfterPayment profileCompleteEvents.Head with
                                                | Ok stateAfterProfile ->
                                                    test <@ stateAfterProfile.Version = 5UL @>
                                                    test <@ List.contains completedProfileValidation stateAfterProfile.ValidationHistory @>
                                                    
                                                    // Step 13: Accept terms
                                                    let acceptedBy = "customer@example.com"
                                                    let acceptedAt = DateTimeOffset.UtcNow
                                                    let commandId8 = createTestCommandId()
                                                    let metadata8 = createTestCommandMetadata commandId8 None None "test"
                                                    let acceptTermsCommand = AcceptTerms (reservationId, acceptedBy, acceptedAt, metadata8)
                                                    
                                                    match Engine.applyCommand (Some stateAfterProfile) acceptTermsCommand currentTime with
                                                    | Ok acceptEvents ->
                                                        test <@ List.length acceptEvents = 1 @>
                                                        
                                                        // Step 14: Apply terms accepted event
                                                        match Engine.applyEvent stateAfterProfile acceptEvents.Head with
                                                        | Ok stateAfterTerms ->
                                                            test <@ match stateAfterTerms.Status with | PendingPayment (due, _) when due = acceptedAt.AddHours 24.0 -> true | _ -> false @>
                                                            test <@ stateAfterTerms.Version = 6UL @>
                                                            
                                                            // Step 15: Confirm reservation
                                                            let confirmedBy = Some "manager@business.com"
                                                            let confirmedAt = DateTimeOffset.UtcNow
                                                            let commandId9 = createTestCommandId()
                                                            let metadata9 = createTestCommandMetadata commandId9 None None "test"
                                                            let confirmCommand = ConfirmReservation (reservationId, confirmedBy, confirmedAt, metadata9)
                                                            
                                                            match Engine.applyCommand (Some stateAfterTerms) confirmCommand currentTime with
                                                            | Ok confirmEvents ->
                                                                test <@ List.length confirmEvents = 1 @>
                                                                
                                                                // Step 16: Apply confirmed event
                                                                match Engine.applyEvent stateAfterTerms confirmEvents.Head with
                                                                | Ok finalState ->
                                                                    test <@ match finalState.Status with | Confirmed (confAt, confBy) when confAt = confirmedAt && confBy = confirmedBy -> true | _ -> false @>
                                                                    test <@ finalState.Version = 7UL @>
                                                                    test <@ finalState.Id = reservationId @>
                                                                    test <@ finalState.Request = initialRequest @>
                                                                    test <@ List.length finalState.ValidationHistory = 3 @>
                                                                | Error messages ->
                                                                    failwithf "Failed to apply confirmation event: %A" messages
                                                            | Error error ->
                                                                failwithf "Failed to confirm reservation: %s" error
                                                        | Error messages ->
                                                            failwithf "Failed to apply terms accepted event: %A" messages
                                                    | Error error ->
                                                        failwithf "Failed to accept terms: %s" error
                                                | Error messages ->
                                                    failwithf "Failed to apply profile validation completed event: %A" messages
                                            | Error error ->
                                                failwithf "Failed to complete customer profile validation: %s" error
                                        | Error error ->
                                            failwithf "Failed to start customer profile validation: %s" error
                                    | Error messages ->
                                        failwithf "Failed to apply payment validation completed event: %A" messages
                                | Error error ->
                                    failwithf "Failed to complete payment validation: %s" error
                            | Error error ->
                                failwithf "Failed to start payment validation: %s" error
                        | Error messages ->
                            failwithf "Failed to apply availability validation completed event: %A" messages
                    | Error error ->
                        failwithf "Failed to complete availability validation: %s" error
                | Error messages ->
                    failwithf "Failed to apply validation started event: %A" messages
            | Error error ->
                failwithf "Failed to start availability validation: %s" error
        | Error messages ->
            failwithf "Failed to apply create event: %A" messages
    | Error error ->
        failwithf "Failed to create reservation: %s" error

[<Fact>]
let ``Reservation lifecycle with failed validation`` () =
    // Step 1: Create reservation with validations
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let availabilityValidation = createTestValidationState (ValidationId "availability-1") AvailabilityCheck true Pending [] "Check availability" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    let paymentValidation = createTestValidationState (ValidationId "payment-1") PaymentCheck true Pending [] "Payment check" None None DateTimeOffset.UtcNow DateTimeOffset.UtcNow
    
    let initialRequest = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = requestedTime
        Services = []
        Participants = []
        SpecialRequests = None
        Metadata = Map.empty
        ValidationStates = [availabilityValidation; paymentValidation]
        InterimPrice = None
        AdjustedTime = None
        CreatedAt = DateTimeOffset.UtcNow
        CreatedBy = None
        Version = 1UL
    }
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        // Step 2: Apply create event to get initial state
        let initialState = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            test <@ stateAfterCreate.Status = Draft @>
            test <@ stateAfterCreate.Version = 1UL @>
            
            // Step 3: Start availability validation
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let startValidationCommand = StartValidation (reservationId, availabilityValidation.Id, None, metadata2)
            
            match Engine.applyCommand (Some stateAfterCreate) startValidationCommand currentTime with
            | Ok startEvents ->
                // Step 4: Apply validation started event
                match Engine.applyEvent stateAfterCreate startEvents.Head with
                | Ok stateAfterStart ->
                    // Step 5: Complete availability validation with failure
                    let failedAvailabilityValidation = createTestValidationState availabilityValidation.Id availabilityValidation.Kind availabilityValidation.IsRequired (Failed (DateTimeOffset.UtcNow, "Resource not available")) availabilityValidation.Dependencies availabilityValidation.Description availabilityValidation.Timeout availabilityValidation.RetryPolicy availabilityValidation.CreatedAt DateTimeOffset.UtcNow
                    let commandId3 = createTestCommandId()
                    let metadata3 = createTestCommandMetadata commandId3 None None "test"
                    let completeValidationCommand = CompleteValidation (reservationId, availabilityValidation.Id, false, Some "Resource not available", DateTimeOffset.UtcNow, metadata3)
                    
                    match Engine.applyCommand (Some stateAfterStart) completeValidationCommand currentTime with
                    | Ok completeEvents ->
                        // Step 6: Apply validation completed event
                        match Engine.applyEvent stateAfterStart completeEvents.Head with
                        | Ok stateAfterComplete ->
                            test <@ stateAfterComplete.Version = 3UL @>
                            test <@ List.contains failedAvailabilityValidation stateAfterComplete.ValidationHistory @>
                            
                            // Step 7: Check pending validations - should only include payment validation since availability failed
                            let pendingValidations = Engine.getPendingValidations stateAfterComplete
                            test <@ List.length pendingValidations = 1 @>
                            test <@ pendingValidations.Head.Id = paymentValidation.Id @>
                            
                            // Step 8: Try to accept terms - should fail because required validation failed
                            let acceptedBy = "customer@example.com"
                            let acceptedAt = DateTimeOffset.UtcNow
                            let commandId4 = createTestCommandId()
                            let metadata4 = createTestCommandMetadata commandId4 None None "test"
                            let acceptTermsCommand = AcceptTerms (reservationId, acceptedBy, acceptedAt, metadata4)
                            
                            match Engine.applyCommand (Some stateAfterComplete) acceptTermsCommand currentTime with
                            | Error error ->
                                test <@ error = "Invalid command/state" @> // Should fail because reservation is not in a state that accepts terms
                            | Ok events ->
                                failwithf "Accept terms should have failed: %A" events
                        | Error messages ->
                            failwithf "Failed to apply validation completed event: %A" messages
                    | Error error ->
                        failwithf "Failed to complete validation: %s" error
                | Error messages ->
                    failwithf "Failed to apply validation started event: %A" messages
            | Error error ->
                failwithf "Failed to start validation: %s" error
        | Error messages ->
            failwithf "Failed to apply create event: %A" messages
    | Error error ->
        failwithf "Failed to create reservation: %s" error

[<Fact>]
let ``Reservation lifecycle with change request`` () =
    // Step 1: Create and confirm reservation
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let originalTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    let newTime = createTestZonedTimeRange (DateTimeOffset.UtcNow.AddHours(2.0)) (DateTimeOffset.UtcNow.AddHours(4.0)) "UTC"
    
    let initialRequest = {
        Id = Some reservationId
        CustomerId = customerId
        BusinessId = businessId
        Resource = createTestResourceDescriptor resourceId Room (Some 10) Map.empty
        RequestedTime = originalTime
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
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        let initialState = createTestReservation reservationId initialRequest originalTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            // Accept terms
            let acceptedBy = "customer@example.com"
            let acceptedAt = DateTimeOffset.UtcNow
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let acceptTermsCommand = AcceptTerms (reservationId, acceptedBy, acceptedAt, metadata2)
            
            match Engine.applyCommand (Some stateAfterCreate) acceptTermsCommand currentTime with
            | Ok acceptEvents ->
                match Engine.applyEvent stateAfterCreate acceptEvents.Head with
                | Ok stateAfterTerms ->
                    // Confirm reservation
                    let confirmedBy = Some "manager@business.com"
                    let confirmedAt = DateTimeOffset.UtcNow
                    let commandId3 = createTestCommandId()
                    let metadata3 = createTestCommandMetadata commandId3 None None "test"
                    let confirmCommand = ConfirmReservation (reservationId, confirmedBy, confirmedAt, metadata3)
                    
                    match Engine.applyCommand (Some stateAfterTerms) confirmCommand currentTime with
                    | Ok confirmEvents ->
                        match Engine.applyEvent stateAfterTerms confirmEvents.Head with
                        | Ok confirmedState ->
                            test <@ confirmedState.Status = Confirmed (confirmedAt, confirmedBy) @>
                            test <@ confirmedState.Version = 3UL @>
                            
                            // Step 2: Request change
                            let reason = "Change of schedule"
                            let changeRequest = { NewTime = Some newTime; NewParticipants = None; NewServices = None; Reason = reason }
                            let commandId4 = createTestCommandId()
                            let metadata4 = createTestCommandMetadata commandId4 None None "test"
                            let changeRequestCommand = RequestReservationChange (reservationId, changeRequest, metadata4)
                            
                            match Engine.applyCommand (Some confirmedState) changeRequestCommand currentTime with
                            | Ok changeRequestEvents ->
                                test <@ List.length changeRequestEvents = 1 @>
                                test <@ match changeRequestEvents.Head with | ReservationChangeRequested (rid, change, meta) when rid = reservationId && change = changeRequest && meta.Version = 4UL -> true | _ -> false @>
                                
                                // Step 3: Apply change request event
                                match Engine.applyEvent confirmedState changeRequestEvents.Head with
                                | Ok stateAfterChangeRequest ->
                                    test <@ stateAfterChangeRequest.Version = 4UL @>
                                    test <@ List.contains changeRequestEvents.Head stateAfterChangeRequest.Events @>
                                    
                                    // Step 4: Apply change
                                    let commandId5 = createTestCommandId()
                                    let metadata5 = createTestCommandMetadata commandId5 None None "test"
                                    let changeAppliedCommand = ReservationChangeApplied (reservationId, changeRequest, DateTimeOffset.UtcNow, metadata5)
                                    
                                    match Engine.applyCommand (Some stateAfterChangeRequest) changeAppliedCommand currentTime with
                                    | Ok changeAppliedEvents ->
                                        test <@ List.length changeAppliedEvents = 1 @>
                                        test <@ match changeAppliedEvents.Head with | ReservationChangeApplied (rid, change, appliedAt, meta) when rid = reservationId && change = changeRequest && meta.Version = 5UL -> true | _ -> false @>
                                        
                                        // Step 5: Apply change applied event
                                        match Engine.applyEvent stateAfterChangeRequest changeAppliedEvents.Head with
                                        | Ok finalState ->
                                            test <@ finalState.FinalTime = newTime @>
                                            test <@ finalState.Version = 5UL @>
                                            test <@ finalState.Status = Confirmed (confirmedAt, confirmedBy) @> // Status should remain confirmed
                                        | Error messages ->
                                            failwithf "Failed to apply change applied event: %A" messages
                                    | Error error ->
                                        failwithf "Failed to apply change: %s" error
                                | Error messages ->
                                    failwithf "Failed to apply change request event: %A" messages
                            | Error error ->
                                failwithf "Failed to request change: %s" error
                        | Error messages ->
                            failwithf "Failed to apply confirmation event: %A" messages
                    | Error error ->
                        failwithf "Failed to confirm reservation: %s" error
                | Error messages ->
                    failwithf "Failed to apply terms accepted event: %A" messages
            | Error error ->
                failwithf "Failed to accept terms: %s" error
        | Error messages ->
            failwithf "Failed to apply create event: %A" messages
    | Error error ->
        failwithf "Failed to create reservation: %s" error

[<Fact>]
let ``Reservation lifecycle with cancellation`` () =
    // Step 1: Create and confirm reservation
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let initialRequest = {
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
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        let initialState = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            // Accept terms
            let acceptedBy = "customer@example.com"
            let acceptedAt = DateTimeOffset.UtcNow
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let acceptTermsCommand = AcceptTerms (reservationId, acceptedBy, acceptedAt, metadata2)
            
            match Engine.applyCommand (Some stateAfterCreate) acceptTermsCommand currentTime with
            | Ok acceptEvents ->
                match Engine.applyEvent stateAfterCreate acceptEvents.Head with
                | Ok stateAfterTerms ->
                    // Confirm reservation
                    let confirmedBy = Some "manager@business.com"
                    let confirmedAt = DateTimeOffset.UtcNow
                    let commandId3 = createTestCommandId()
                    let metadata3 = createTestCommandMetadata commandId3 None None "test"
                    let confirmCommand = ConfirmReservation (reservationId, confirmedBy, confirmedAt, metadata3)
                    
                    match Engine.applyCommand (Some stateAfterTerms) confirmCommand currentTime with
                    | Ok confirmEvents ->
                        match Engine.applyEvent stateAfterTerms confirmEvents.Head with
                        | Ok confirmedState ->
                            test <@ confirmedState.Status = Confirmed (confirmedAt, confirmedBy) @>
                            test <@ confirmedState.Version = 3UL @>
                            
                            // Step 2: Cancel reservation by customer
                            let cancellationReason = CustomerCancelled (Some "customer@example.com", DateTimeOffset.UtcNow, Some "No longer needed")
                            let commandId4 = createTestCommandId()
                            let metadata4 = createTestCommandMetadata commandId4 None None "test"
                            let cancelCommand = CancelReservation (reservationId, cancellationReason, metadata4)
                            
                            match Engine.applyCommand (Some confirmedState) cancelCommand currentTime with
                            | Ok cancelEvents ->
                                test <@ List.length cancelEvents = 1 @>
                                test <@ match cancelEvents.Head with | ReservationCancelled (rid, reason, meta) when rid = reservationId && reason = cancellationReason && meta.Version = 4UL -> true | _ -> false @>
                                
                                // Step 3: Apply cancellation event
                                match Engine.applyEvent confirmedState cancelEvents.Head with
                                | Ok cancelledState ->
                                    test <@ match cancelledState.Status with | Cancelled reason' when reason' = cancellationReason -> true | _ -> false @>
                                    test <@ cancelledState.Version = 4UL @>
                                    
                                    // Step 4: Try to perform operations on cancelled reservation - should fail
                                    let newCommand = ConfirmReservation (reservationId, None, DateTimeOffset.UtcNow, metadata4)
                                    match Engine.applyCommand (Some cancelledState) newCommand currentTime with
                                    | Error error ->
                                        test <@ error = "Invalid command/state" @> // Should fail because reservation is cancelled
                                    | Ok events ->
                                        failwithf "Operation on cancelled reservation should have failed: %A" events
                                | Error messages ->
                                    failwithf "Failed to apply cancellation event: %A" messages
                            | Error error ->
                                failwithf "Failed to cancel reservation: %s" error
                        | Error messages ->
                            failwithf "Failed to apply confirmation event: %A" messages
                    | Error error ->
                        failwithf "Failed to confirm reservation: %s" error
                | Error messages ->
                    failwithf "Failed to apply terms accepted event: %A" messages
            | Error error ->
                failwithf "Failed to accept terms: %s" error
        | Error messages ->
            failwithf "Failed to apply create event: %A" messages
    | Error error ->
        failwithf "Failed to create reservation: %s" error

[<Fact>]
let ``Reservation lifecycle with timeout`` () =
    // Step 1: Create reservation in draft state
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let initialRequest = {
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
        CreatedAt = DateTimeOffset.UtcNow.AddHours(-2.0) // Created 2 hours ago
        CreatedBy = None
        Version = 1UL
    }
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        let initialState = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            test <@ stateAfterCreate.Status = Draft @>
            test <@ stateAfterCreate.Version = 1UL @>
            
            // Step 2: Simulate timeout by creating timeout event
            let timeoutAt = DateTimeOffset.UtcNow
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let timeoutEvent = ReservationTimedOut (reservationId, timeoutAt, metadata2)
            
            // Step 3: Apply timeout event
            match Engine.applyEvent stateAfterCreate timeoutEvent with
            | Ok timedOutState ->
                test <@ match timedOutState.Status with | Cancelled (Timeout timeoutAt') when timeoutAt' = timeoutAt -> true | _ -> false @>
                test <@ timedOutState.Version = 2UL @>
                
                // Step 4: Try to perform operations on timed out reservation - should fail
                let newCommand = ConfirmReservation (reservationId, None, DateTimeOffset.UtcNow, metadata2)
                match Engine.applyCommand (Some timedOutState) newCommand currentTime with
                | Error error ->
                    test <@ error = "Invalid command/state" @> // Should fail because reservation is cancelled
                | Ok events ->
                    failwithf "Operation on timed out reservation should have failed: %A" events
            | Error messages ->
                failwithf "Failed to apply timeout event: %A" messages
    | Error error ->
        failwithf "Failed to create reservation: %s" error

// ---------- Error Scenario Tests [<Class>] type ErrorScenarioTests =
[<Fact>]
let ``Error scenario: Update non-existent reservation`` () =
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let updateRequest = {
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
    let updateCommand = UpdateReservation (reservationId, updateRequest, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None updateCommand currentTime with
    | Error error ->
        test <@ error = "Reservation not found" @>
    | Ok events ->
        failwithf "Update non-existent reservation should have failed: %A" events

[<Fact>]
let ``Error scenario: Confirm non-existent reservation`` () =
    let reservationId = createTestReservationId()
    let confirmedBy = Some "manager@business.com"
    let at = DateTimeOffset.UtcNow
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let confirmCommand = ConfirmReservation (reservationId, confirmedBy, at, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None confirmCommand currentTime with
    | Error error ->
        test <@ error = "Reservation not found" @>
    | Ok events ->
        failwithf "Confirm non-existent reservation should have failed: %A" events

[<Fact>]
let ``Error scenario: Cancel non-existent reservation`` () =
    let reservationId = createTestReservationId()
    let reason = CustomerCancelled (None, DateTimeOffset.UtcNow, None)
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let cancelCommand = CancelReservation (reservationId, reason, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None cancelCommand currentTime with
    | Error error ->
        test <@ error = "Reservation not found" @>
    | Ok events ->
        failwithf "Cancel non-existent reservation should have failed: %A" events

[<Fact>]
let ``Error scenario: Start validation on non-existent reservation`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let startValidationCommand = StartValidation (reservationId, validationId, None, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None startValidationCommand currentTime with
    | Error error ->
        test <@ error = "Reservation not found" @>
    | Ok events ->
        failwithf "Start validation on non-existent reservation should have failed: %A" events

[<Fact>]
let ``Error scenario: Complete validation on non-existent reservation`` () =
    let reservationId = createTestReservationId()
    let validationId = createTestValidationId()
    let succeeded = true
    let details = Some "Validation passed"
    let at = DateTimeOffset.UtcNow
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let completeValidationCommand = CompleteValidation (reservationId, validationId, succeeded, details, at, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None completeValidationCommand currentTime with
    | Error error ->
        test <@ error = "Reservation not found" @>
    | Ok events ->
        failwithf "Complete validation on non-existent reservation should have failed: %A" events

[<Fact>]
let ``Error scenario: Version mismatch in update`` () =
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
    
    let existingReservation = createTestReservation reservationId originalRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
    
    let updatedRequest = {
        originalRequest with
            SpecialRequests = Some "Window seat preferred"
            Version = 3UL // Version jumps from 1 to 3, skipping 2
    }
    
    let commandId = createTestCommandId()
    let metadata = createTestCommandMetadata commandId None None "test"
    let updateCommand = UpdateReservation (reservationId, updatedRequest, metadata)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand (Some existingReservation) updateCommand currentTime with
    | Error error ->
        test <@ error = "Invalid command/state" @> // Should fail due to version mismatch
    | Ok events ->
        failwithf "Update with version mismatch should have failed: %A" events

[<Fact>]
let ``Error scenario: Invalid state transition`` () =
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let initialRequest = {
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
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        let initialState = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            test <@ stateAfterCreate.Status = Draft @>
            
            // Try to confirm reservation without accepting terms first
            let confirmedBy = Some "manager@business.com"
            let confirmedAt = DateTimeOffset.UtcNow
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let confirmCommand = ConfirmReservation (reservationId, confirmedBy, confirmedAt, metadata2)
            
            match Engine.applyCommand (Some stateAfterCreate) confirmCommand currentTime with
            | Error error ->
                test <@ error = "Invalid command/state" @> // Should fail because reservation is in Draft state, not ready for confirmation
            | Ok events ->
                failwithf "Confirm without accepting terms should have failed: %A" events
    | Error error ->
        failwithf "Failed to create reservation: %s" error

[<Fact>]
let ``Error scenario: Duplicate reservation creation`` () =
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let initialRequest = {
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
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand1 = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    // First creation should succeed
    match Engine.applyCommand None createCommand1 currentTime with
    | Ok createEvents1 ->
        test <@ List.length createEvents1 = 1 @>
        
        // Second creation with same ID should fail
        let commandId2 = createTestCommandId()
        let metadata2 = createTestCommandMetadata commandId2 None None "test"
        let createCommand2 = CreateReservation (initialRequest, metadata2)
        
        match Engine.applyCommand None createCommand2 currentTime with
        | Error error ->
            test <@ error = "Reservation already exists" @>
        | Ok events ->
            failwithf "Duplicate reservation creation should have failed: %A" events
    | Error error ->
        failwithf "First reservation creation failed: %s" error

// ---------- Event Sourcing and Replay Tests [<Class>] type EventSourcingTests =
[<Fact>]
let ``Event sourcing: Create snapshot and replay events`` () =
    // Step 1: Create a reservation with several events
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let initialRequest = {
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
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        let initialState = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            // Accept terms
            let acceptedBy = "customer@example.com"
            let acceptedAt = DateTimeOffset.UtcNow
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let acceptTermsCommand = AcceptTerms (reservationId, acceptedBy, acceptedAt, metadata2)
            
            match Engine.applyCommand (Some stateAfterCreate) acceptTermsCommand currentTime with
            | Ok acceptEvents ->
                match Engine.applyEvent stateAfterCreate acceptEvents.Head with
                | Ok stateAfterTerms ->
                    // Confirm reservation
                    let confirmedBy = Some "manager@business.com"
                    let confirmedAt = DateTimeOffset.UtcNow
                    let commandId3 = createTestCommandId()
                    let metadata3 = createTestCommandMetadata commandId3 None None "test"
                    let confirmCommand = ConfirmReservation (reservationId, confirmedBy, confirmedAt, metadata3)
                    
                    match Engine.applyCommand (Some stateAfterTerms) confirmCommand currentTime with
                    | Ok confirmEvents ->
                        match Engine.applyEvent stateAfterTerms confirmEvents.Head with
                        | Ok confirmedState ->
                            test <@ confirmedState.Version = 3UL @>
                            test <@ List.length confirmedState.Events = 3 @>
                            
                            // Step 2: Create snapshot
                            let timestamp = DateTimeOffset.UtcNow
                            let snapshot = Engine.createSnapshot confirmedState None timestamp
                            
                            test <@ snapshot.ReservationId = reservationId @>
                            test <@ snapshot.State = confirmedState @>
                            test <@ snapshot.Version = 3UL @>
                            test <@ snapshot.Timestamp = timestamp @>
                            test <@ List.length snapshot.EventIds = 3 @>
                            
                            // Step 3: Create empty state and replay events from snapshot
                            let emptyState = {
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
                            
                            let eventsToReplay = snapshot.State.Events
                            match Engine.replay emptyState eventsToReplay with
                            | Ok replayedState ->
                                test <@ replayedState.Id = reservationId @>
                                test <@ replayedState.Version = 3UL @>
                                test <@ replayedState.Status = Confirmed (confirmedAt, confirmedBy) @>
                                test <@ replayedState.Request = initialRequest @>
                                test <@ replayedState.FinalTime = requestedTime @>
                                test <@ replayedState.FinalPrice = Money.Zero "EUR" @>
                                test <@ List.length replayedState.Events = 3 @>
                            | Error messages ->
                                failwithf "Failed to replay events: %A" messages
                        | Error messages ->
                            failwithf "Failed to apply confirmation event: %A" messages
                    | Error error ->
                        failwithf "Failed to confirm reservation: %s" error
                | Error messages ->
                    failwithf "Failed to apply terms accepted event: %A" messages
            | Error error ->
                failwithf "Failed to accept terms: %s" error
        | Error messages ->
            failwithf "Failed to apply create event: %A" messages
    | Error error ->
        failwithf "Failed to create reservation: %s" error

[<Fact>]
let ``Event sourcing: Partial replay with sinceVersion`` () =
    // Step 1: Create a reservation with several events
    let reservationId = createTestReservationId()
    let customerId = createTestCustomerId()
    let businessId = createTestBusinessId()
    let resourceId = createTestResourceId()
    let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
    
    let initialRequest = {
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
    
    let commandId1 = createTestCommandId()
    let metadata1 = createTestCommandMetadata commandId1 None None "test"
    let createCommand = CreateReservation (initialRequest, metadata1)
    let currentTime = DateTimeOffset.UtcNow
    
    match Engine.applyCommand None createCommand currentTime with
    | Ok createEvents ->
        let initialState = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
        match Engine.applyEvent initialState createEvents.Head with
        | Ok stateAfterCreate ->
            // Accept terms
            let acceptedBy = "customer@example.com"
            let acceptedAt = DateTimeOffset.UtcNow
            let commandId2 = createTestCommandId()
            let metadata2 = createTestCommandMetadata commandId2 None None "test"
            let acceptTermsCommand = AcceptTerms (reservationId, acceptedBy, acceptedAt, metadata2)
            
            match Engine.applyCommand (Some stateAfterCreate) acceptTermsCommand currentTime with
            | Ok acceptEvents ->
                match Engine.applyEvent stateAfterCreate acceptEvents.Head with
                | Ok stateAfterTerms ->
                    // Confirm reservation
                    let confirmedBy = Some "manager@business.com"
                    let confirmedAt = DateTimeOffset.UtcNow
                    let commandId3 = createTestCommandId()
                    let metadata3 = createTestCommandMetadata commandId3 None None "test"
                    let confirmCommand = ConfirmReservation (reservationId, confirmedBy, confirmedAt, metadata3)
                    
                    match Engine.applyCommand (Some stateAfterTerms) confirmCommand currentTime with
                    | Ok confirmEvents ->
                        match Engine.applyEvent stateAfterTerms confirmEvents.Head with
                        | Ok confirmedState ->
                            test <@ confirmedState.Version = 3UL @>
                            test <@ List.length confirmedState.Events = 3 @>
                            
                            // Step 2: Create snapshot with sinceVersion = 2
                            let timestamp = DateTimeOffset.UtcNow
                            let snapshot = Engine.createSnapshot confirmedState (Some 2UL) timestamp
                            
                            test <@ List.length snapshot.EventIds = 2 @> // Should only include events from version 2 onwards
                            
                            // Step 3: Create state at version 1 and replay partial events
                            let stateAtVersion1 = createTestReservation reservationId initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
                            
                            let eventsToReplay = snapshot.State.Events
                            match Engine.replay stateAtVersion1 eventsToReplay with
                            | Ok replayedState ->
                                test <@ replayedState.Id = reservationId @>
                                test <@ replayedState.Version = 3UL @>
                                test <@ replayedState.Status = Confirmed (confirmedAt, confirmedBy) @>
                                test <@ List.length replayedState.Events = 2 @> // Should only have the replayed events
                            | Error messages ->
                                failwithf "Failed to replay partial events: %A" messages
                        | Error messages ->
                            failwithf "Failed to apply confirmation event: %A" messages
                    | Error error ->
                        failwithf "Failed to confirm reservation: %s" error
                | Error messages ->
                    failwithf "Failed to apply terms accepted event: %A" messages
            | Error error ->
                failwithf "Failed to accept terms: %s" error
        | Error messages ->
            failwithf "Failed to apply create event: %A" messages
    | Error error ->
        failwithf "Failed to create reservation: %s" error

// ---------- Property-based tests using Unquote [<Class>] type PropertyTests =
[<Property>]
let ``Complete lifecycle preserves reservation ID throughout`` (reservationId: string) =
    let reservationId' = ReservationId(Guid.Parse reservationId)
    let testCompleteLifecycle () =
        // Simplified version of complete lifecycle test
        let customerId = createTestCustomerId()
        let businessId = createTestBusinessId()
        let resourceId = createTestResourceId()
        let requestedTime = createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC"
        
        let initialRequest = {
            Id = Some reservationId'
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
        
        let commandId1 = createTestCommandId()
        let metadata1 = createTestCommandMetadata commandId1 None None "test"
        let createCommand = CreateReservation (initialRequest, metadata1)
        let currentTime = DateTimeOffset.UtcNow
        
        match Engine.applyCommand None createCommand currentTime with
        | Ok createEvents ->
            let initialState = createTestReservation reservationId' initialRequest requestedTime (Money.Zero "EUR") Draft 1UL [] []
            match Engine.applyEvent initialState createEvents.Head with
            | Ok stateAfterCreate ->
                test <@ stateAfterCreate.Id = reservationId' @>
                true
            | Error _ -> false
        | Error _ -> false
    
    test <@ testCompleteLifecycle () @>

[<Property>]
let ``Event replay is deterministic`` (events: Event list) =
    if List.isEmpty events then () else // Skip empty list to avoid trivial case
    let reservationId = createTestReservationId()
    let initialState = createTestReservation reservationId 
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
    
    let result1 = Engine.replay initialState events
    let result2 = Engine.replay initialState events
    
    match result1, result2 with
    | Ok state1, Ok state2 -> test <@ state1 = state2 @>
    | Error _, Error _ -> test <@ true @> // Both failed with same errors
    | _, _ -> test <@ false @> // One succeeded, one failed

[<Property>]
let ``Snapshot creation is idempotent`` (reservation: Reservation) (timestamp: DateTimeOffset) =
    let snapshot1 = Engine.createSnapshot reservation None timestamp
    let snapshot2 = Engine.createSnapshot reservation None timestamp
    test <@ snapshot1 = snapshot2 @>

[<Property>]
let ``Command application is deterministic`` (command: Command) (currentTime: DateTimeOffset) =
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
            Version = 1UL
        } 
        (createTestZonedTimeRange (DateTimeOffset.UtcNow) (DateTimeOffset.UtcNow.AddHours(2.0)) "UTC") 
        (Money.Zero "EUR") 
        Draft 
        1UL 
        [] 
        []
    
    let result1 = Engine.applyCommand (Some existingReservation) command currentTime
    let result2 = Engine.applyCommand (Some existingReservation) command currentTime
    
    match result1, result2 with
    | Ok events1, Ok events2 -> test <@ events1 = events2 @>
    | Error error1, Error error2 -> test <@ error1 = error2 @>
    | _, _ -> test <@ false @> // Inconsistent results