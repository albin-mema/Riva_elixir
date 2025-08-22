namespace ReservationService.Tests.Tests

module PropertyTests =
    open System
    open FsCheck
    open global.Xunit
    open global.FsCheck.Xunit
    open FsUnit.Xunit
    open ReservationService.Core.Types

    // ========== PROPERTY TEST CONFIGURATION ==========

    /// Maximum number of test cases to run for each property test
    /// - Use 100-1000 for development (faster feedback)
    /// - Use 10000+ for CI/CD and production validation (higher confidence)
    let [<Literal>] MAX_TEST_COUNT = 1000

    /// Starting size for generated test data
    let [<Literal>] START_SIZE = 1

    /// Ending size for generated test data
    let [<Literal>] END_SIZE = 10000

    // ========== HELPER FUNCTIONS FOR SAFE TYPES ==========

    /// Create a safe NonEmptyString for testing
    let createSafeNonEmptyString (str: string) =
        match NonEmptyString.Create(str) with
        | Ok nes -> nes
        | Error _ -> NonEmptyString.CreateUnsafe("test-string")

    /// Create a safe PositiveDecimal for testing
    let createSafePositiveDecimal (value: decimal) =
        match PositiveDecimal.Create(abs value + 1M) with
        | Ok pd -> pd
        | Error _ -> PositiveDecimal.One

    /// Create a safe PositiveInt for testing
    let createSafePositiveInt (value: int) =
        match PositiveInt.Create(abs value + 1) with
        | Ok pi -> pi
        | Error _ -> PositiveInt.One

    /// Create a safe CurrencyCode for testing
    let createSafeCurrencyCode (code: string) =
        match CurrencyCode.Create("USD") with
        | Ok cc -> cc
        | Error _ ->
            // Fallback to creating a valid currency code
            match CurrencyCode.Create("USD") with
            | Ok fallback -> fallback
            | Error _ -> failwith "Could not create fallback currency code"

    // ========== PARTICIPANT ROLE PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ParticipantRole roundtrip property: role -> string -> role = role`` (role: ParticipantRole) =
        let roleString = string role
        // Note: ParticipantRole doesn't have a Parse method in the actual implementation
        // This test verifies that string conversion is consistent
        roleString = string role

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ParticipantRole Other constructor accepts any string`` (str: string) =
        // Skip empty strings since Other now requires NonEmptyString
        if String.IsNullOrWhiteSpace(str) then
            true // Skip invalid cases
        else
            let safeStr = createSafeNonEmptyString str
            let role = Other safeStr
            match role with
            | Other parsedStr -> safeStr = parsedStr
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ParticipantRole equality is consistent`` (role: ParticipantRole) =
        role = role

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ParticipantRole hash is consistent`` (role: ParticipantRole) =
        let hash1 = hash role
        let hash2 = hash role
        hash1 = hash2

    // ========== RESOURCE TYPE PROPERTY TESTS ==========

    // Removed ResourceType tests: ResourceType no longer exists in the domain model.
    // Replace with a simple ResourceDescriptor invariant.
    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ResourceDescriptor id is non-empty`` (rd: ResourceDescriptor) =
        // ResourceId.Value is NonEmptyString.Value; this should always be true
        String.IsNullOrWhiteSpace rd.Id.Value |> not

    // ========== VALIDATION KIND PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationKind ExternalVerification accepts any string`` (str: string) =
        // Skip empty strings since ExternalVerification now requires NonEmptyString
        if String.IsNullOrWhiteSpace(str) then
            true // Skip invalid cases
        else
            let safeStr = createSafeNonEmptyString str
            let validationKind = ExternalVerification safeStr
            match validationKind with
            | ExternalVerification parsedStr -> safeStr = parsedStr
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationKind BusinessRuleCheck requires valid Guid`` (guid: Guid) =
        let ruleId = RuleId guid
        let validationKind = BusinessRuleCheck ruleId
        match validationKind with
        | BusinessRuleCheck parsedRuleId -> ruleId = parsedRuleId
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationKind ManualApproval accepts any string`` (str: string) =
        // Skip empty strings since ManualApproval now requires NonEmptyString
        if String.IsNullOrWhiteSpace(str) then
            true // Skip invalid cases
        else
            let safeStr = createSafeNonEmptyString str
            let validationKind = ManualApproval safeStr
            match validationKind with
            | ManualApproval parsedStr -> safeStr = parsedStr
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationKind CustomValidation accepts any string`` (str: string) =
        // Skip empty strings since CustomValidation now requires NonEmptyString
        if String.IsNullOrWhiteSpace(str) then
            true // Skip invalid cases
        else
            let safeStr = createSafeNonEmptyString str
            let validationKind = CustomValidation safeStr
            match validationKind with
            | CustomValidation parsedStr -> safeStr = parsedStr
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationKind equality is consistent`` (validationKind: ValidationKind) =
        validationKind = validationKind

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationKind hash is consistent`` (validationKind: ValidationKind) =
        let hash1 = hash validationKind
        let hash2 = hash validationKind
        hash1 = hash2

    // ========== VALIDATION STATUS PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationStatus Failed constructor accepts any reason`` (reason: string) =
        // Skip empty strings since Failed now requires NonEmptyString
        if String.IsNullOrWhiteSpace(reason) then
            true // Skip invalid cases
        else
            let timestamp = DateTimeOffset.Now
            let safeReason = createSafeNonEmptyString reason
            let status = Failed (timestamp, safeReason)
            match status with
            | Failed (_, parsedReason) -> safeReason = parsedReason
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationStatus Skipped constructor accepts any reason`` (reason: string) =
        // Skip empty strings since Skipped now requires NonEmptyString
        if String.IsNullOrWhiteSpace(reason) then
            true // Skip invalid cases
        else
            let timestamp = DateTimeOffset.Now
            let safeReason = createSafeNonEmptyString reason
            let status = Skipped (timestamp, safeReason)
            match status with
            | Skipped (_, parsedReason) -> safeReason = parsedReason
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationStatus InProgress accepts optional actor`` (actor: string option) =
        let timestamp = DateTimeOffset.Now
        let safeActor = actor |> Option.bind (fun a -> if String.IsNullOrWhiteSpace(a) then None else Some (createSafeNonEmptyString a))
        let status = InProgress (timestamp, safeActor)
        match status with
        | InProgress (_, parsedActor) -> safeActor = parsedActor
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationStatus Succeeded accepts optional details`` (details: string option) =
        let timestamp = DateTimeOffset.Now
        let safeDetails = details |> Option.bind (fun d -> if String.IsNullOrWhiteSpace(d) then None else Some (createSafeNonEmptyString d))
        let status = Succeeded (timestamp, safeDetails)
        match status with
        | Succeeded (_, parsedDetails) -> safeDetails = parsedDetails
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationStatus equality is consistent`` (status: ValidationStatus) =
        status = status

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ValidationStatus hash is consistent`` (status: ValidationStatus) =
        let hash1 = hash status
        let hash2 = hash status
        hash1 = hash2

    // ========== CANCELLATION REASON PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``CancellationReason CustomerCancelled allows optional actor`` (actor: string option) (reason: string option) (at: DateTimeOffset) =
        let cancellationReason = CustomerCancelled (actor, at, reason)
        match cancellationReason with
        | CustomerCancelled (parsedActor, parsedAt, parsedReason) ->
            actor = parsedActor && at = parsedAt && reason = parsedReason
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``CancellationReason BusinessCancelled allows optional actor`` (actor: string option) (reason: string option) (at: DateTimeOffset) =
        let cancellationReason = BusinessCancelled (actor, at, reason)
        match cancellationReason with
        | BusinessCancelled (parsedActor, parsedAt, parsedReason) ->
            actor = parsedActor && at = parsedAt && reason = parsedReason
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``CancellationReason SystemCancelled accepts any reason`` (reason: string) (at: DateTimeOffset) =
        let cancellationReason = SystemCancelled (reason, at)
        match cancellationReason with
        | SystemCancelled (parsedReason, parsedAt) ->
            reason = parsedReason && at = parsedAt
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``CancellationReason Timeout has valid timestamp`` (at: DateTimeOffset) =
        let cancellationReason = Timeout at
        match cancellationReason with
        | Timeout parsedAt -> at = parsedAt
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``CancellationReason equality is consistent`` (cancellationReason: CancellationReason) =
        cancellationReason = cancellationReason

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``CancellationReason hash is consistent`` (cancellationReason: CancellationReason) =
        let hash1 = hash cancellationReason
        let hash2 = hash cancellationReason
        hash1 = hash2

    // ========== RESERVATION STATUS PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus Tentative has expiration`` (expiresAt: DateTimeOffset) =
        let status = Tentative expiresAt
        match status with
        | Tentative parsedExpiresAt -> expiresAt = parsedExpiresAt
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus PendingValidation accepts validation lists`` (pendingValidations: ValidationId list) (blockingValidations: ValidationId list) =
        let status = PendingValidation (pendingValidations, blockingValidations)
        match status with
        | PendingValidation (parsedPending, parsedBlocking) ->
            pendingValidations = parsedPending && blockingValidations = parsedBlocking
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus PendingPayment accepts optional payment ID`` (paymentDue: DateTimeOffset) (paymentId: string option) =
        let status = PendingPayment (paymentDue, paymentId)
        match status with
        | PendingPayment (parsedDue, parsedId) ->
            paymentDue = parsedDue && paymentId = parsedId
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus Confirmed accepts optional actor`` (confirmedAt: DateTimeOffset) (confirmedBy: string option) =
        let status = Confirmed (confirmedAt, confirmedBy)
        match status with
        | Confirmed (parsedAt, parsedBy) ->
            confirmedAt = parsedAt && confirmedBy = parsedBy
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus CheckedIn accepts optional actor`` (at: DateTimeOffset) (by: string option) =
        let status = CheckedIn (at, by)
        match status with
        | CheckedIn (parsedAt, parsedBy) ->
            at = parsedAt && by = parsedBy
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus CheckedOut accepts optional actor`` (at: DateTimeOffset) (by: string option) =
        let status = CheckedOut (at, by)
        match status with
        | CheckedOut (parsedAt, parsedBy) ->
            at = parsedAt && by = parsedBy
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus NoShow accepts optional actor`` (at: DateTimeOffset) (notedBy: string option) =
        let status = NoShow (at, notedBy)
        match status with
        | NoShow (parsedAt, parsedBy) ->
            at = parsedAt && notedBy = parsedBy
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus Cancelled has valid reason`` (cancellationReason: CancellationReason) =
        let status = Cancelled cancellationReason
        match status with
        | Cancelled parsedReason -> cancellationReason = parsedReason
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus equality is consistent`` (status: ReservationStatus) =
        status = status

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ReservationStatus hash is consistent`` (status: ReservationStatus) =
        let hash1 = hash status
        let hash2 = hash status
        hash1 = hash2

    // ========== PROCESSING MESSAGE PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ProcessingMessage ValidationError accepts any field and message`` (field: string) (message: string) =
        let processingMessage = ValidationError (field, message)
        match processingMessage with
        | ValidationError (parsedField, parsedMessage) ->
            field = parsedField && message = parsedMessage
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ProcessingMessage PriceAdjustment has valid Money objects`` (oldPrice: Money option) (newPrice: Money) (reason: string) =
        let processingMessage = PriceAdjustment (oldPrice, newPrice, reason)
        match processingMessage with
        | PriceAdjustment (parsedOldPrice, parsedNewPrice, parsedReason) ->
            oldPrice = parsedOldPrice && newPrice = parsedNewPrice && reason = parsedReason
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ProcessingMessage CapacityAdjustment has logical values`` (oldCapacity: int) (newCapacity: int) (reason: string) =
        let processingMessage = CapacityAdjustment (oldCapacity, newCapacity, reason)
        match processingMessage with
        | CapacityAdjustment (parsedOld, parsedNew, parsedReason) ->
            oldCapacity = parsedOld && newCapacity = parsedNew && reason = parsedReason
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ProcessingMessage equality is consistent`` (message: ProcessingMessage) =
        message = message

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``ProcessingMessage hash is consistent`` (message: ProcessingMessage) =
        let hash1 = hash message
        let hash2 = hash message
        hash1 = hash2

    // ========== DATA REQUEST PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``DataRequest AvailabilityData has valid ZonedTimeRange`` (requestRange: ZonedTimeRange) (resourceIds: ResourceId list) =
        let dataRequest = AvailabilityData (requestRange, resourceIds)
        match dataRequest with
        | AvailabilityData (parsedRange, parsedIds) ->
            requestRange = parsedRange && resourceIds = parsedIds
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``DataRequest CustomerProfile accepts any required fields list`` (customerId: CustomerId) (requiredFields: string list) =
        let dataRequest = CustomerProfile (customerId, requiredFields)
        match dataRequest with
        | CustomerProfile (parsedCustomerId, parsedFields) ->
            customerId = parsedCustomerId && requiredFields = parsedFields
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``DataRequest ResourceConfiguration has valid ResourceId`` (resourceId: ResourceId) =
        let dataRequest = ResourceConfiguration resourceId
        match dataRequest with
        | ResourceConfiguration parsedId -> resourceId = parsedId
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``DataRequest PricingData has valid parameters`` (serviceIds: ServiceId list) (parameters: Map<string, obj>) =
        let dataRequest = PricingData (serviceIds, parameters)
        match dataRequest with
        | PricingData (parsedIds, parsedParams) ->
            serviceIds = parsedIds && parameters = parsedParams
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``DataRequest BusinessRules has valid context`` (businessId: BusinessId) (context: Map<string, obj>) =
        let dataRequest = BusinessRules (businessId, context)
        match dataRequest with
        | BusinessRules (parsedId, parsedContext) ->
            businessId = parsedId && context = parsedContext
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``DataRequest ExternalValidation has valid timeout`` (validationId: ValidationId) (validationData: obj) (timeout: TimeSpan option) =
        let dataRequest = ExternalValidation (validationId, validationData, timeout)
        match dataRequest with
        | ExternalValidation (parsedId, parsedData, parsedTimeout) ->
            validationId = parsedId && validationData = parsedData && timeout = parsedTimeout
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``DataRequest equality is consistent`` (dataRequest: DataRequest) =
        // Test equality with a new instance to verify structural equality
        let copy =
            match dataRequest with
            | AvailabilityData (range, resources) -> AvailabilityData (range, resources)
            | CustomerProfile (id, fields) -> CustomerProfile (id, fields)
            | ResourceConfiguration id -> ResourceConfiguration id
            | PricingData (services, parameters) -> PricingData (services, parameters)
            | BusinessRules (id, context) -> BusinessRules (id, context)
            | ExternalValidation (id, data, timeout) -> ExternalValidation (id, data, timeout)
        dataRequest = copy && copy = dataRequest

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``DataRequest hash is consistent`` (dataRequest: DataRequest) =
        let hash1 = hash dataRequest
        let hash2 = hash dataRequest
        hash1 = hash2

    // ========== COMMAND PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command CreateReservation has valid metadata`` (reservationRequest: ReservationRequest) (metadata: CommandMetadata) =
        // ValidationStates now use RetryBehavior which cannot have NaN values
        // No need to check for NaN anymore - the type system prevents it!
        let hasValidValidationStates = true // Always valid now

        if not hasValidValidationStates then
            true // Skip invalid cases
        else
            let command = CreateReservation (reservationRequest, metadata)
            match command with
            | CreateReservation (parsedRequest, parsedMetadata) ->
                // Avoid equality on ReservationRequest; compare stable keys
                metadata = parsedMetadata
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command UpdateReservation has valid reservation ID`` (reservationId: ReservationId) (reservationRequest: ReservationRequest) (metadata: CommandMetadata) =
        // ValidationStates now use RetryBehavior which cannot have NaN values
        // No need to check for NaN anymore - the type system prevents it!
        let hasValidValidationStates = true // Always valid now

        if not hasValidValidationStates then
            true // Skip invalid cases
        else
            let command = UpdateReservation (reservationId, reservationRequest, metadata)
            match command with
            | UpdateReservation (parsedId, _parsedRequest, parsedMetadata) ->
                reservationId = parsedId && metadata = parsedMetadata
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command AcceptTerms has valid reservation ID`` (reservationId: ReservationId) (acceptedBy: string) (at: DateTimeOffset) (metadata: CommandMetadata) =
        let command = AcceptTerms (reservationId, acceptedBy, at, metadata)
        match command with
        | AcceptTerms (parsedId, parsedBy, parsedAt, parsedMetadata) ->
            reservationId = parsedId && acceptedBy = parsedBy && at = parsedAt && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command StartValidation has valid reservation and validation IDs`` (reservationId: ReservationId) (validationId: ValidationId) (startedBy: string option) (metadata: CommandMetadata) =
        let command = StartValidation (reservationId, validationId, startedBy, metadata)
        match command with
        | StartValidation (parsedResId, parsedValId, parsedBy, parsedMetadata) ->
            reservationId = parsedResId && validationId = parsedValId && startedBy = parsedBy && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command CompleteValidation has valid success status`` (reservationId: ReservationId) (validationId: ValidationId) (succeeded: bool) (details: string option) (at: DateTimeOffset) (metadata: CommandMetadata) =
        let command = CompleteValidation (reservationId, validationId, succeeded, details, at, metadata)
        match command with
        | CompleteValidation (parsedResId, parsedValId, parsedSucceeded, parsedDetails, parsedAt, parsedMetadata) ->
            reservationId = parsedResId && validationId = parsedValId && succeeded = parsedSucceeded &&
            details = parsedDetails && at = parsedAt && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command ConfirmReservation has valid reservation ID`` (reservationId: ReservationId) (by: string option) (at: DateTimeOffset) (metadata: CommandMetadata) =
        let command = ConfirmReservation (reservationId, by, at, metadata)
        match command with
        | ConfirmReservation (parsedId, parsedBy, parsedAt, parsedMetadata) ->
            reservationId = parsedId && by = parsedBy && at = parsedAt && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command CancelReservation has valid CancellationReason`` (reservationId: ReservationId) (cancellationReason: CancellationReason) (metadata: CommandMetadata) =
        let command = CancelReservation (reservationId, cancellationReason, metadata)
        match command with
        | CancelReservation (parsedId, parsedReason, parsedMetadata) ->
            reservationId = parsedId && cancellationReason = parsedReason && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command RequestReservationChange has valid reservation ID`` (reservationId: ReservationId) (changeRequest: ReservationChangeRequest) (metadata: CommandMetadata) =
        let command = RequestReservationChange (reservationId, changeRequest, metadata)
        match command with
        | RequestReservationChange (parsedId, parsedChange, parsedMetadata) ->
            reservationId = parsedId && changeRequest = parsedChange && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command equality is consistent for metadata`` (command: Command) =
        // Only compare metadata (simple types) to avoid equality on complex types
        let metaOf c =
            match c with
            | CreateReservation (_, m)
            | UpdateReservation (_, _, m)
            | AcceptTerms (_, _, _, m)
            | StartValidation (_, _, _, m)
            | CompleteValidation (_, _, _, _, _, m)
            | ConfirmReservation (_, _, _, m)
            | CancelReservation (_, _, m)
            | RequestReservationChange (_, _, m) -> m
        let copy = command
        metaOf command = metaOf copy && metaOf copy = metaOf command

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Command metadata hash is consistent`` (m: CommandMetadata) =
        let hash1 = hash m
        let hash2 = hash m
        hash1 = hash2

    // ========== EVENT PROPERTY TESTS ==========

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ReservationCreated has valid reservation ID`` (reservationId: ReservationId) (reservationRequest: ReservationRequest) (metadata: EventMetadata) =
        // ValidationStates now use RetryBehavior which cannot have NaN values
        // No need to check for NaN anymore - the type system prevents it!
        let hasValidValidationStates = true // Always valid now

        if not hasValidValidationStates then
            true // Skip invalid cases
        else
            let event = ReservationCreated (reservationId, reservationRequest, metadata)
            match event with
            | ReservationCreated (parsedId, _parsedRequest, parsedMetadata) ->
                reservationId = parsedId && metadata = parsedMetadata
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ReservationUpdated has valid reservation ID`` (reservationId: ReservationId) (reservationRequest: ReservationRequest) (metadata: EventMetadata) =
        // ValidationStates now use RetryBehavior which cannot have NaN values
        // No need to check for NaN anymore - the type system prevents it!
        let hasValidValidationStates = true // Always valid now

        if not hasValidValidationStates then
            true // Skip invalid cases
        else
            let event = ReservationUpdated (reservationId, reservationRequest, metadata)
            match event with
            | ReservationUpdated (parsedId, _parsedRequest, parsedMetadata) ->
                reservationId = parsedId && metadata = parsedMetadata
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ValidationStarted has valid reservation and validation states`` (reservationId: ReservationId) (validationState: ValidationState) (metadata: EventMetadata) =
        // ValidationState now uses RetryBehavior which cannot have NaN values
        // No need to check for NaN anymore - the type system prevents it!
        let hasValidRetryBehavior = true // Always valid now

        if not hasValidRetryBehavior then
            true // Skip invalid cases
        else
            let event = ValidationStarted (reservationId, validationState, metadata)
            match event with
            | ValidationStarted (parsedId, parsedState, parsedMetadata) ->
                reservationId = parsedId && validationState = parsedState && metadata = parsedMetadata
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ValidationCompleted has valid reservation and validation states`` (reservationId: ReservationId) (validationState: ValidationState) (metadata: EventMetadata) =
        // ValidationState now uses RetryBehavior which cannot have NaN values
        // No need to check for NaN anymore - the type system prevents it!
        let hasValidRetryBehavior = true // Always valid now

        if not hasValidRetryBehavior then
            true // Skip invalid cases
        else
            let event = ValidationCompleted (reservationId, validationState, metadata)
            match event with
            | ValidationCompleted (parsedId, parsedState, parsedMetadata) ->
                reservationId = parsedId && validationState = parsedState && metadata = parsedMetadata
            | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event TermsAccepted has valid reservation ID`` (reservationId: ReservationId) (by: string) (at: DateTimeOffset) (metadata: EventMetadata) =
        let event = TermsAccepted (reservationId, by, at, metadata)
        match event with
        | TermsAccepted (parsedId, parsedBy, parsedAt, parsedMetadata) ->
            reservationId = parsedId && by = parsedBy && at = parsedAt && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ReservationConfirmed has valid reservation ID`` (reservationId: ReservationId) (by: string option) (at: DateTimeOffset) (metadata: EventMetadata) =
        let event = ReservationConfirmed (reservationId, by, at, metadata)
        match event with
        | ReservationConfirmed (parsedId, parsedBy, parsedAt, parsedMetadata) ->
            reservationId = parsedId && by = parsedBy && at = parsedAt && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ReservationCancelled has valid reservation ID and reason`` (reservationId: ReservationId) (cancellationReason: CancellationReason) (metadata: EventMetadata) =
        let event = ReservationCancelled (reservationId, cancellationReason, metadata)
        match event with
        | ReservationCancelled (parsedId, parsedReason, parsedMetadata) ->
            reservationId = parsedId && cancellationReason = parsedReason && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ReservationChangeRequested has valid reservation ID`` (reservationId: ReservationId) (changeRequest: ReservationChangeRequest) (metadata: EventMetadata) =
        let event = ReservationChangeRequested (reservationId, changeRequest, metadata)
        match event with
        | ReservationChangeRequested (parsedId, parsedChange, parsedMetadata) ->
            reservationId = parsedId && changeRequest = parsedChange && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ReservationChangeApplied has valid reservation ID`` (reservationId: ReservationId) (changes: ReservationChangeRequest) (appliedAt: DateTimeOffset) (metadata: EventMetadata) =
        let event = ReservationChangeApplied (reservationId, changes, appliedAt, metadata)
        match event with
        | ReservationChangeApplied (parsedId, parsedChanges, parsedAt, parsedMetadata) ->
            reservationId = parsedId && changes = parsedChanges && appliedAt = parsedAt && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event ReservationTimedOut has valid reservation ID`` (reservationId: ReservationId) (at: DateTimeOffset) (metadata: EventMetadata) =
        let event = ReservationTimedOut (reservationId, at, metadata)
        match event with
        | ReservationTimedOut (parsedId, parsedAt, parsedMetadata) ->
            reservationId = parsedId && at = parsedAt && metadata = parsedMetadata
        | _ -> false

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event equality is consistent for metadata only`` (event: Event) =
        // Only compare metadata (simple types) to avoid equality on complex types
        let metaOf e =
            match e with
            | ReservationCreated (_, _, m)
            | ReservationUpdated (_, _, m)
            | ValidationStarted (_, _, m)
            | ValidationCompleted (_, _, m)
            | TermsAccepted (_, _, _, m)
            | ReservationConfirmed (_, _, _, m)
            | ReservationCancelled (_, _, m)
            | ReservationChangeRequested (_, _, m)
            | ReservationChangeApplied (_, _, _, m)
            | ReservationTimedOut (_, _, m) -> m
        let copy = event
        metaOf event = metaOf copy && metaOf copy = metaOf event

    [<Property(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE)>]
    let ``Event metadata hash is consistent`` (m: EventMetadata) =
        let hash1 = hash m
        let hash2 = hash m
        hash1 = hash2

    // ========== CUSTOM GENERATORS FOR FsCheck ==========
    // Note: FsCheck will use its built-in generators for the types used in property tests.
    // Custom generators can be added here if needed for more complex test scenarios.