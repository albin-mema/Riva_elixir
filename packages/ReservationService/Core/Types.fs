namespace ReservationService.Core.Types

open System

// ========== CONSTRAINED PRIMITIVE TYPES ==========

/// A non-empty string that cannot be null or whitespace
[<Struct>]
type NonEmptyString = private NonEmptyString of string
with
    member this.Value = let (NonEmptyString v) = this in v

    static member Create(value: string) =
        if String.IsNullOrWhiteSpace(value) then
            Error "String cannot be null, empty, or whitespace"
        else
            Ok (NonEmptyString value)

    static member CreateUnsafe(value: string) = NonEmptyString value

/// A positive decimal that cannot be negative or zero
[<Struct>]
type PositiveDecimal = private PositiveDecimal of decimal
with
    member this.Value = let (PositiveDecimal v) = this in v

    static member Create(value: decimal) =
        if value <= 0M then
            Error "Amount must be positive"
        else
            Ok (PositiveDecimal value)

    static member One = PositiveDecimal 1M

/// A positive integer that cannot be zero or negative
[<Struct>]
type PositiveInt = private PositiveInt of int
with
    member this.Value = let (PositiveInt v) = this in v

    static member Create(value: int) =
        if value <= 0 then
            Error "Value must be positive"
        else
            Ok (PositiveInt value)

    static member One = PositiveInt 1

/// A non-negative integer that cannot be negative
[<Struct>]
type NonNegativeInt = private NonNegativeInt of int
with
    member this.Value = let (NonNegativeInt v) = this in v

    static member Create(value: int) =
        if value < 0 then
            Error "Value cannot be negative"
        else
            Ok (NonNegativeInt value)

    static member Zero = NonNegativeInt 0


// ========== GENERIC CONSTRAINED TYPES (FSCheck-friendly) ==========

/// A non-negative TimeSpan (>= 0)
[<Struct>]
type NonNegativeTimeSpan = private NonNegativeTimeSpan of TimeSpan
with
    member this.Value = let (NonNegativeTimeSpan v) = this in v
    static member Create(value: TimeSpan) =
        if value < TimeSpan.Zero then Error "Duration cannot be negative" else Ok (NonNegativeTimeSpan value)
    static member Zero = NonNegativeTimeSpan TimeSpan.Zero

/// A strictly positive TimeSpan (> 0)
[<Struct>]
type PositiveTimeSpan = private PositiveTimeSpan of TimeSpan
with
    member this.Value = let (PositiveTimeSpan v) = this in v
    static member Create(value: TimeSpan) =
        if value <= TimeSpan.Zero then Error "Duration must be positive" else Ok (PositiveTimeSpan value)

/// A bounded integer such that min <= value <= max
type BoundedInt = private BoundedInt of min:int * max:int * value:int
with
    member this.Min = let (BoundedInt (mn, _, _)) = this in mn
    member this.Max = let (BoundedInt (_, mx, _)) = this in mx
    member this.Value = let (BoundedInt (_, _, v)) = this in v
    static member Create(min:int, max:int, value:int) =
        if max < min then Error "Max must be >= min"
        elif value < min || value > max then Error (sprintf "Value %d out of bounds [%d, %d]" value min max)
        else Ok (BoundedInt (min, max, value))

/// A bounded decimal such that min <= value <= max
type BoundedDecimal = private BoundedDecimal of min:decimal * max:decimal * value:decimal
with
    member this.Min = let (BoundedDecimal (mn, _, _)) = this in mn
    member this.Max = let (BoundedDecimal (_, mx, _)) = this in mx
    member this.Value = let (BoundedDecimal (_, _, v)) = this in v
    static member Create(min:decimal, max:decimal, value:decimal) =
        if max < min then Error "Max must be >= min"
        elif value < min || value > max then Error (sprintf "Value %M out of bounds [%M, %M]" value min max)
        else Ok (BoundedDecimal (min, max, value))

/// A list that is guaranteed to be non-empty
type NonEmptyList<'T> = private NonEmptyList of 'T list
with
    member this.Value = let (NonEmptyList v) = this in v
    static member Create(items: 'T list) =
        match items with
        | [] -> Error "List cannot be empty"
        | _ -> Ok (NonEmptyList items)
    static member CreateUnsafe(items: 'T list) = NonEmptyList items

/// An array that is guaranteed to be non-empty
type NonEmptyArray<'T> = private NonEmptyArray of 'T array
with
    member this.Value = let (NonEmptyArray v) = this in v
    static member Create(items: 'T array) =
        if isNull (box items) then Error "Array cannot be null"
        elif items.Length = 0 then Error "Array cannot be empty"
        else Ok (NonEmptyArray items)
    static member CreateUnsafe(items: 'T array) = NonEmptyArray items

/// A positive float that cannot be NaN, Infinity, or negative
[<Struct>]
type PositiveFloat = private PositiveFloat of float
with
    member this.Value = let (PositiveFloat v) = this in v

    static member Create(value: float) =
        if Double.IsNaN(value) then
            Error "Value cannot be NaN"
        elif Double.IsInfinity(value) then
            Error "Value cannot be Infinity"
        elif value <= 0.0 then
            Error "Value must be positive"
        else
            Ok (PositiveFloat value)

    static member Two = PositiveFloat 2.0

/// A valid ISO 4217 currency code
[<Struct>]
type CurrencyCode = private CurrencyCode of string
with
    member this.Value = let (CurrencyCode v) = this in v

    static member Create(code: string) =
        if String.IsNullOrWhiteSpace(code) then
            Error "Currency code cannot be empty"
        elif code.Length <> 3 then
            Error "Currency code must be exactly 3 characters"
        elif not (code |> Seq.forall Char.IsUpper) then
            Error "Currency code must be uppercase"
        else
            Ok (CurrencyCode code)

// ========== COMPUTATION EXPRESSION FOR RESULT ==========

module ResultBuilder =
    type ResultBuilder() =
        member _.Bind(x, f) = Result.bind f x
        member _.Return(x) = Ok x
        member _.ReturnFrom(x) = x

    let result = ResultBuilder()

// ---------- Core Identifiers ----------
[<Struct>] type ReservationId  = ReservationId of Guid
[<Struct>] type CustomerId     = CustomerId of Guid
[<Struct>] type ServiceId      = ServiceId of string
[<Struct>] type RuleId         = RuleId of Guid
[<Struct>] type CommandId      = CommandId of Guid
[<Struct>] type CorrelationId  = CorrelationId of Guid

/// Resource ID that cannot be empty
[<Struct>]
type ResourceId = private ResourceId of NonEmptyString
with
    member this.Value = let (ResourceId v) = this in v.Value

    static member Create(value: string) =
        NonEmptyString.Create(value) |> Result.map ResourceId

/// Business ID that cannot be empty
[<Struct>]
type BusinessId = private BusinessId of NonEmptyString
with
    member this.Value = let (BusinessId v) = this in v.Value

    static member Create(value: string) =
        NonEmptyString.Create(value) |> Result.map BusinessId

/// Validation ID that cannot be empty
[<Struct>]
type ValidationId = private ValidationId of NonEmptyString
with
    member this.Value = let (ValidationId v) = this in v.Value

    static member Create(value: string) =
        NonEmptyString.Create(value) |> Result.map ValidationId

// ---------- Improved Value Objects ----------
/// Money is a generic amount and currency string provided by the caller.
type Money = private {
    Amount: PositiveDecimal
    Currency: string
}
with
    member this.AmountValue = this.Amount.Value
    member this.CurrencyValue = this.Currency

    static member Create(amount: decimal, currency: string) =
        ResultBuilder.result {
            let! validAmount = PositiveDecimal.Create(amount)
            if String.IsNullOrWhiteSpace(currency) then
                return! Error "Currency must be provided by caller"
            else
                return { Amount = validAmount; Currency = currency }
        }

/// Time range that cannot have end before start
type TimeRange = private {
    Start: DateTimeOffset
    End: DateTimeOffset
}
with
    member this.Duration = this.End - this.Start

    static member Create(start: DateTimeOffset, endTime: DateTimeOffset) =
        if endTime < start then
            Error "End time must be on or after start time"
        else
            Ok { Start = start; End = endTime }

/// Zoned time range with valid time zone
type ZonedTimeRange = private {
    Range: TimeRange
    TimeZoneId: NonEmptyString option
}
with
    static member Create(range: TimeRange, timeZoneId: string option) =
        match timeZoneId with
        | None -> Ok { Range = range; TimeZoneId = None }
        | Some tz ->
            NonEmptyString.Create(tz)
            |> Result.map (fun validTz -> { Range = range; TimeZoneId = Some validTz })

// ---------- Improved Participant Types ----------
type ParticipantRole =
    | Primary
    | Guest
    | Staff
    | Other of NonEmptyString

/// Participant that must have either ID or name (or both)
type Participant =
    | IdentifiedParticipant of id: Guid * name: NonEmptyString option * role: ParticipantRole * contact: Map<string, string>
    | NamedParticipant of name: NonEmptyString * role: ParticipantRole * contact: Map<string, string>
with
    static member CreateWithId(id: Guid, name: string option, role: ParticipantRole, contact: Map<string, string>) =
        match name with
        | None -> Ok (IdentifiedParticipant (id, None, role, contact))
        | Some n ->
            NonEmptyString.Create(n)
            |> Result.map (fun validName -> IdentifiedParticipant (id, Some validName, role, contact))

    static member CreateWithName(name: string, role: ParticipantRole, contact: Map<string, string>) =
        NonEmptyString.Create(name)
        |> Result.map (fun validName -> NamedParticipant (validName, role, contact))

// ---------- Improved Resource & Service Descriptors ----------

/// Resource capacity that makes sense for the resource type
type ResourceCapacity =
    | UnlimitedCapacity
    | FixedCapacity of PositiveInt
    | VariableCapacity of min: NonNegativeInt * max: PositiveInt

/// Configuration for a resource type that defines required properties and validation rules
type ResourceConfig = {
    /// Name of the resource type (e.g., "Room", "Equipment")
    Name: NonEmptyString
    /// Set of required property names that must be present in Attributes
    RequiredProperties: Set<NonEmptyString>
    /// Optional, pure validation function (FSCheck-friendly) that checks Attributes
    Validate: Option<Map<NonEmptyString, NonEmptyString> -> Result<unit, string list>>
}

/// Definition of a resource type using its configuration
type ResourceDefinition = {
    /// The configuration that defines this resource type
    Config: ResourceConfig
}

/// Helper functions for creating resource configurations from parameters
module ResourceConfigHelpers =
    /// Create a resource configuration from parameters passed by the caller
    let createConfig name requiredProps validateOpt =
        match NonEmptyString.Create(name) with
        | Ok validName ->
            let validProps =
                requiredProps
                |> List.choose (fun prop ->
                    match NonEmptyString.Create(prop) with
                    | Ok validProp -> Some validProp
                    | Error _ -> None)
                |> Set.ofList
            Ok {
                Name = validName
                RequiredProperties = validProps
                Validate = validateOpt
            }
        | Error msg -> Error msg

    /// Create a simple validation function that checks for required properties
    let createRequiredPropsValidator requiredProps =
        fun (attrs: Map<NonEmptyString, NonEmptyString>) ->
            let missingProps =
                requiredProps
                |> List.filter (fun prop ->
                    match NonEmptyString.Create(prop) with
                    | Ok validProp -> not (attrs.ContainsKey validProp)
                    | Error _ -> true)

            if List.isEmpty missingProps then
                Ok ()
            else
                Error (missingProps |> List.map (fun prop -> sprintf "Missing required property: %s" prop))

type ResourceDescriptor = {
    Id: ResourceId
    Definition: ResourceDefinition
    Attributes: Map<NonEmptyString, NonEmptyString>
    AvailabilityConstraints: NonEmptyString list
}

type ServiceDescriptor = {
    Id: ServiceId
    Name: NonEmptyString
    Duration: TimeSpan option
    BasePrice: Money option
    Attributes: Map<NonEmptyString, NonEmptyString>
    RequiredResources: ResourceId list
}

// ---------- Improved Validation Lifecycle with Dependencies ----------

/// Retry behavior that cannot have invalid configurations
type RetryBehavior =
    | NoRetry
    | RetryOnce
    | LinearBackoff of attempts: PositiveInt * delaySeconds: PositiveInt
    | ExponentialBackoff of attempts: PositiveInt * baseDelaySeconds: PositiveInt * maxDelaySeconds: PositiveInt

/// Timeout that makes business sense
type ValidationTimeout =
    | NoTimeout
    | TimeoutAfter of duration: TimeSpan
    | TimeoutAt of absoluteTime: DateTimeOffset

type ValidationKind =
    | AvailabilityCheck
    | CustomerProfileCheck
    | PaymentCheck
    | ExternalVerification of serviceName: NonEmptyString
    | BusinessRuleCheck of RuleId
    | ManualApproval of approvalRole: NonEmptyString
    | CustomValidation of validationName: NonEmptyString

type ValidationStatus =
    | Pending
    | InProgress of startedAt: DateTimeOffset * actor: NonEmptyString option
    | Succeeded of at: DateTimeOffset * details: NonEmptyString option
    | Failed of at: DateTimeOffset * reason: NonEmptyString
    | Skipped of at: DateTimeOffset * reason: NonEmptyString

type ValidationDependency = {
    ValidationId: ValidationId
    MustSucceed: bool // If false, this dependency is informational only
}

/// Validation state that cannot be in impossible states
type ValidationState = {
    Id: ValidationId
    Kind: ValidationKind
    IsRequired: bool
    Status: ValidationStatus
    Dependencies: ValidationDependency list
    Description: NonEmptyString
    Timeout: ValidationTimeout
    RetryBehavior: RetryBehavior
    CreatedAt: DateTimeOffset
    UpdatedAt: DateTimeOffset
}

// ---------- Processing Messages ----------
type ProcessingMessage =
    | TimeAdjustmentRequired of original: ZonedTimeRange * suggested: ZonedTimeRange * reason: string
    | AdditionalDataRequired of DataRequest
    | ValidationError of field: string * message: string
    | BusinessRuleApplied of ruleId: RuleId * effect: string
    | ValidationPending of ValidationId * description: string
    | ValidationCompleted of ValidationId * result: bool * details: string option
    | PriceAdjustment of oldPrice: Money option * newPrice: Money * reason: string
    | CapacityAdjustment of oldCapacity: int * newCapacity: int * reason: string

and DataRequest =
    | AvailabilityData of requestRange: ZonedTimeRange * resourceIds: ResourceId list
    | CustomerProfile of CustomerId * requiredFields: string list
    | ResourceConfiguration of ResourceId
    | PricingData of ServiceId list * parameters: Map<string, obj>
    | BusinessRules of BusinessId * context: Map<string, obj>
    | ExternalValidation of ValidationId * validationData: obj * timeout: TimeSpan option

// ---------- Reservation Request and States ----------
type ReservationRequest = {
    Id: ReservationId option
    CustomerId: CustomerId
    BusinessId: BusinessId
    Resource: ResourceDescriptor
    RequestedTime: ZonedTimeRange
    Services: ServiceDescriptor list
    Participants: Participant list
    SpecialRequests: string option
    Metadata: Map<string, string>
    ValidationStates: ValidationState list
    InterimPrice: Money option
    AdjustedTime: ZonedTimeRange option
    CreatedAt: DateTimeOffset
    CreatedBy: string option
    Version: uint64
}

type ValidReservation = {
    Request: ReservationRequest
    ValidatedTime: ZonedTimeRange
    Price: Money
    TermsAccepted: bool
    TermsAcceptedAt: DateTimeOffset option
    CompletedValidations: ValidationState list
    ValidatedAt: DateTimeOffset
}

// ---------- Reservation Status with Enhanced State Tracking ----------
type CancellationReason =
    | CustomerCancelled of by: string option * at: DateTimeOffset * reason: string option
    | BusinessCancelled of by: string option * at: DateTimeOffset * reason: string option
    | SystemCancelled of reason: string * at: DateTimeOffset
    | Timeout of at: DateTimeOffset

type ReservationStatus =
    | Draft
    | Tentative of expiresAt: DateTimeOffset
    | PendingValidation of pendingValidations: ValidationId list * blockingValidations: ValidationId list
    | PendingPayment of paymentDue: DateTimeOffset * paymentId: string option
    | Confirmed of confirmedAt: DateTimeOffset * confirmedBy: string option
    | CheckedIn of DateTimeOffset * by: string option
    | CheckedOut of DateTimeOffset * by: string option
    | NoShow of DateTimeOffset * notedBy: string option
    | Cancelled of CancellationReason

// ---------- Event Sourcing Implementation ----------
type CommandMetadata = {
    CommandId: CommandId
    CorrelationId: CorrelationId option
    Timestamp: DateTimeOffset
    UserId: string option
    Source: string
}

type EventMetadata = {
    EventId: Guid
    CommandId: CommandId
    CorrelationId: CorrelationId option
    Timestamp: DateTimeOffset
    Version: uint64
}

type Command =
    | CreateReservation of ReservationRequest * metadata: CommandMetadata
    | UpdateReservation of ReservationId * ReservationRequest * metadata: CommandMetadata
    | AcceptTerms of ReservationId * acceptedBy: string * at: DateTimeOffset * metadata: CommandMetadata
    | StartValidation of ReservationId * ValidationId * startedBy: string option * metadata: CommandMetadata
    | CompleteValidation of ReservationId * ValidationId * succeeded: bool * details: string option * at: DateTimeOffset * metadata: CommandMetadata
    | ConfirmReservation of ReservationId * by: string option * at: DateTimeOffset * metadata: CommandMetadata
    | CancelReservation of ReservationId * CancellationReason * metadata: CommandMetadata
    | RequestReservationChange of ReservationId * changeRequest: ReservationChangeRequest * metadata: CommandMetadata

and ReservationChangeRequest = {
    NewTime: ZonedTimeRange option
    NewParticipants: Participant list option
    NewServices: ServiceDescriptor list option
    Reason: string
}

type Event =
    | ReservationCreated of ReservationId * ReservationRequest * metadata: EventMetadata
    | ReservationUpdated of ReservationId * ReservationRequest * metadata: EventMetadata
    | ValidationStarted of ReservationId * ValidationState * metadata: EventMetadata
    | ValidationCompleted of ReservationId * ValidationState * metadata: EventMetadata
    | TermsAccepted of ReservationId * by: string * at: DateTimeOffset * metadata: EventMetadata
    | ReservationConfirmed of ReservationId * by: string option * at: DateTimeOffset * metadata: EventMetadata
    | ReservationCancelled of ReservationId * CancellationReason * metadata: EventMetadata
    | ReservationChangeRequested of ReservationId * ReservationChangeRequest * metadata: EventMetadata
    | ReservationChangeApplied of ReservationId * changes: ReservationChangeRequest * appliedAt: DateTimeOffset * metadata: EventMetadata
    | ReservationTimedOut of ReservationId * at: DateTimeOffset * metadata: EventMetadata

// ---------- Final Reservation Type ----------
type Reservation = {
    Id: ReservationId
    Request: ReservationRequest
    FinalTime: ZonedTimeRange
    FinalPrice: Money
    CreatedAt: DateTimeOffset
    CreatedBy: string option
    Status: ReservationStatus
    Version: uint64
    ValidationHistory: ValidationState list
    Events: Event list // Recent events since last snapshot
}

// ---------- Snapshot Types for Event Sourcing ----------
type ReservationSnapshot = {
    ReservationId: ReservationId
    State: Reservation
    Version: uint64
    Timestamp: DateTimeOffset
    EventIds: Guid list // Last N events included in this snapshot
}

// ---------- Function Signatures with Enhanced Error Handling ----------
type ProcessPartialValidation =
    ValidationId -> bool -> string option -> Reservation -> Result<Reservation, ProcessingMessage list>

type GetPendingValidations =
    Reservation -> ValidationState list

type UpdateValidationState =
    ValidationId -> bool -> string option -> DateTimeOffset -> ValidationState -> ValidationState

type ApplyEventToReservation =
    Reservation -> Event -> Result<Reservation, ProcessingMessage list>

type CreateReservationSnapshot =
    Reservation -> uint64 option -> DateTimeOffset -> ReservationSnapshot

type ReplayEventsToReservation =
    Reservation -> Event list -> Result<Reservation, ProcessingMessage list>

// ---------- Query Types for Read Model ----------
type ReservationQuery = {
    ReservationId: ReservationId option
    CustomerId: CustomerId option
    BusinessId: BusinessId option
    Status: ReservationStatus option
    DateRange: TimeRange option
    IncludeValidationDetails: bool
}

type ValidationQuery = {
    ValidationId: ValidationId option
    ReservationId: ReservationId option
    Kind: ValidationKind option
    Status: ValidationStatus option
}

// ---------- Result Types for API Responses ----------
type QueryResult<'T> = {
    Data: 'T list
    TotalCount: int
    Page: int
    PageSize: int
}

type CommandResult = {
    Success: bool
    ReservationId: ReservationId option
    NewVersion: uint64 option
    Events: Event list
    Messages: ProcessingMessage list
    RequiredValidations: ValidationId list
}

// ---------- Policy and Rule Types ----------
type BusinessRule = {
    Id: RuleId
    Name: string
    Description: string
    Condition: ReservationRequest -> bool
    Action: ReservationRequest -> Result<ReservationRequest, ProcessingMessage list>
    Priority: int
}

type ReservationPolicy = {
    Id: RuleId
    BusinessId: BusinessId
    RequiredValidations: ValidationKind list
    ValidationDependencies: (ValidationKind * ValidationKind list) list
    AutoApproveThreshold: PositiveDecimal option
    MaxParticipants: PositiveInt
    MinNotice: TimeSpan
    MaxAdvance: TimeSpan
    TimeoutPolicy: TimeSpan
    RetryBehaviors: Map<ValidationKind, RetryBehavior>
}

// ========== HELPER MODULES FOR WORKING WITH SAFE TYPES ==========

module SafeTypes =

    let createMoney amount currency =
        Money.Create(amount, currency)

    let createTimeRange start endTime =
        TimeRange.Create(start, endTime)

    let createZonedTimeRange range timeZoneId =
        ZonedTimeRange.Create(range, timeZoneId)

    let createResourceId value =
        ResourceId.Create(value)

    let createBusinessId value =
        BusinessId.Create(value)

    let createValidationId value =
        ValidationId.Create(value)

    let createNonEmptyString value =
        NonEmptyString.Create(value)

    let createPositiveDecimal value =
        PositiveDecimal.Create(value)

    let createPositiveInt value =
        PositiveInt.Create(value)

    let createCurrencyCode code =
        CurrencyCode.Create(code)

module RetryBehavior =

    /// Create a linear backoff retry behavior with validation
    let createLinearBackoff attempts delaySeconds =
        ResultBuilder.result {
            let! validAttempts = PositiveInt.Create(attempts)
            let! validDelay = PositiveInt.Create(delaySeconds)
            return LinearBackoff (validAttempts, validDelay)
        }

    /// Create exponential backoff with validation
    let createExponentialBackoff attempts baseDelaySeconds maxDelaySeconds =
        ResultBuilder.result {
            let! validAttempts = PositiveInt.Create(attempts)
            let! validBaseDelay = PositiveInt.Create(baseDelaySeconds)
            let! validMaxDelay = PositiveInt.Create(maxDelaySeconds)
            return ExponentialBackoff (validAttempts, validBaseDelay, validMaxDelay)
        }

    /// Calculate the next retry delay
    let calculateNextDelay behavior attemptNumber (defaultDelay: float) =
        match behavior with
        | NoRetry -> None
        | RetryOnce when attemptNumber <= 1 -> Some (TimeSpan.FromSeconds defaultDelay)
        | RetryOnce -> None
        | LinearBackoff (maxAttempts, delaySeconds) when attemptNumber <= maxAttempts.Value ->
            Some (TimeSpan.FromSeconds(float (attemptNumber * delaySeconds.Value)))
        | ExponentialBackoff (maxAttempts, baseDelay, maxDelay) when attemptNumber <= maxAttempts.Value ->
            let delay = Math.Pow(2.0, float (attemptNumber - 1)) * float baseDelay.Value
            let cappedDelay = min delay (float maxDelay.Value)
            Some (TimeSpan.FromSeconds(cappedDelay))
        | _ -> None

module ValidationTimeout =

    /// Create a timeout with validation
    let createTimeout duration =
        if duration <= TimeSpan.Zero then
            Error "Timeout duration must be positive"
        else
            Ok (TimeoutAfter duration)

    /// Check if a validation has timed out
    let hasTimedOut timeoutBehavior currentTime startTime =
        match timeoutBehavior with
        | NoTimeout -> false
        | TimeoutAfter duration -> (currentTime - startTime) >= duration
        | TimeoutAt absoluteTime -> currentTime >= absoluteTime