namespace ReservationService.Core.ImprovedTypes

open System
open ReservationService.Core.Configuration

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

// ========== IMPROVED CORE IDENTIFIERS ==========

/// Resource ID that cannot be empty
[<Struct>]
type SafeResourceId = private SafeResourceId of NonEmptyString
with
    member this.Value = let (SafeResourceId v) = this in v.Value

    static member Create(value: string) =
        NonEmptyString.Create(value) |> Result.map SafeResourceId

/// Business ID that cannot be empty
[<Struct>]
type SafeBusinessId = private SafeBusinessId of NonEmptyString
with
    member this.Value = let (SafeBusinessId v) = this in v.Value

    static member Create(value: string) =
        NonEmptyString.Create(value) |> Result.map SafeBusinessId

/// Validation ID that cannot be empty
[<Struct>]
type SafeValidationId = private SafeValidationId of NonEmptyString
with
    member this.Value = let (SafeValidationId v) = this in v.Value

    static member Create(value: string) =
        NonEmptyString.Create(value) |> Result.map SafeValidationId

// ========== IMPROVED VALUE OBJECTS ==========

/// Money that cannot have negative amounts or invalid currency
type SafeMoney = private {
    Amount: PositiveDecimal
    Currency: CurrencyCode
}
with
    member this.AmountValue = this.Amount.Value
    member this.CurrencyValue = this.Currency.Value

    static member Create(amount: decimal, currency: string) =
        result {
            let! validAmount = PositiveDecimal.Create(amount)
            let! validCurrency = CurrencyCode.Create(currency)
            return { Amount = validAmount; Currency = validCurrency }
        }

/// Time range that cannot have end before start
type ValidTimeRange = private {
    Start: DateTimeOffset
    End: DateTimeOffset
}
with
    member this.Duration = this.End - this.Start

    static member Create(start: DateTimeOffset, endTime: DateTimeOffset) =
        if endTime <= start then
            Error "End time must be after start time"
        else
            Ok { Start = start; End = endTime }

/// Zoned time range with valid time zone
type SafeZonedTimeRange = private {
    Range: ValidTimeRange
    TimeZoneId: NonEmptyString option
}
with
    static member Create(range: ValidTimeRange, timeZoneId: string option) =
        match timeZoneId with
        | None -> Ok { Range = range; TimeZoneId = None }
        | Some tz ->
            NonEmptyString.Create(tz)
            |> Result.map (fun validTz -> { Range = range; TimeZoneId = Some validTz })

// ========== HELPER FUNCTIONS ==========

module SafeRetryPolicy =
    
    /// Convert from the old unsafe RetryPolicy
    let fromUnsafe (policy: RetryPolicy) : Result<SafeRetryPolicy, string> =
        SafeRetryPolicy.Create(policy.MaxAttempts, policy.BackoffFactor, policy.MaxBackoff)
    
    /// Convert to the old unsafe RetryPolicy (for backward compatibility)
    let toUnsafe (policy: SafeRetryPolicy) : RetryPolicy = {
        MaxAttempts = policy.MaxAttempts.Value
        BackoffFactor = policy.BackoffFactor.Value
        MaxBackoff = policy.MaxBackoff
    }

module RetryConfiguration =
    
    /// Calculate the delay for a given attempt
    let calculateDelay (config: RetryConfiguration) (attemptNumber: int) (retryConfig: RetryConfig) : TimeSpan option =
        match config with
        | NoRetry -> None
        | SimpleRetry maxAttempts ->
            if attemptNumber <= maxAttempts.Value then
                Some (TimeSpan.FromSeconds(float retryConfig.DefaultRetryDelay))
            else
                None
        | BackoffRetry (maxAttempts, strategy, maxBackoff) ->
            if attemptNumber <= maxAttempts.Value then
                let delay =
                    match strategy with
                    | Linear factor ->
                        TimeSpan.FromSeconds(float attemptNumber * factor.Value)
                    | Exponential baseValue ->
                        TimeSpan.FromSeconds(Math.Pow(baseValue.Value, float attemptNumber))
                    | Fixed delay -> delay
                    | Custom calculate -> calculate attemptNumber
                
                Some (min delay maxBackoff)
            else
                None

// ========== COMPUTATION EXPRESSION FOR RESULT ==========

type ResultBuilder() =
    member _.Bind(x, f) = Result.bind f x
    member _.Return(x) = Ok x
    member _.ReturnFrom(x) = x

let result = ResultBuilder()

// ========== EVEN BETTER: DOMAIN-SPECIFIC APPROACH ==========

/// Instead of generic "retry policy", model the actual business concepts
type ValidationRetryBehavior =
    | FailImmediately
    | RetryOnce
    | RetryWithLinearBackoff of attempts: int * delaySeconds: int
    | RetryWithExponentialBackoff of attempts: int * baseDelaySeconds: int
    | CustomRetryLogic of name: string * calculator: (int -> TimeSpan option)

/// Timeout behavior that's explicit about what it means
type ValidationTimeout =
    | NoTimeout
    | TimeoutAfter of duration: TimeSpan
    | TimeoutAt of absoluteTime: DateTimeOffset

/// A validation state that cannot have invalid configurations
type RobustValidationState = {
    Id: ValidationId
    Kind: ValidationKind
    IsRequired: bool
    Status: ValidationStatus
    Dependencies: ValidationDependency list
    Description: string
    TimeoutBehavior: ValidationTimeout
    RetryBehavior: ValidationRetryBehavior
    CreatedAt: DateTimeOffset
    UpdatedAt: DateTimeOffset
}

// ========== SMART CONSTRUCTORS ==========

module ValidationRetryBehavior =

    /// Create a linear backoff retry behavior with validation
    let createLinearBackoff attempts delaySeconds =
        if attempts <= 0 then
            Error "Attempts must be positive"
        elif delaySeconds <= 0 then
            Error "Delay must be positive"
        else
            Ok (RetryWithLinearBackoff (attempts, delaySeconds))

    /// Create exponential backoff with validation
    let createExponentialBackoff attempts baseDelaySeconds =
        if attempts <= 0 then
            Error "Attempts must be positive"
        elif baseDelaySeconds <= 0 then
            Error "Base delay must be positive"
        else
            Ok (RetryWithExponentialBackoff (attempts, baseDelaySeconds))

    /// Calculate the next retry delay
    let calculateNextDelay behavior attemptNumber (retryConfig: RetryConfig) =
        match behavior with
        | FailImmediately -> None
        | RetryOnce when attemptNumber <= 1 -> Some (TimeSpan.FromSeconds(float retryConfig.DefaultRetryDelay))
        | RetryOnce -> None
        | RetryWithLinearBackoff (maxAttempts, delaySeconds) when attemptNumber <= maxAttempts ->
            Some (TimeSpan.FromSeconds(float (attemptNumber * delaySeconds)))
        | RetryWithExponentialBackoff (maxAttempts, baseDelay) when attemptNumber <= maxAttempts ->
            let delay = Math.Pow(2.0, float (attemptNumber - 1)) * float baseDelay
            Some (TimeSpan.FromSeconds(delay))
        | CustomRetryLogic (_, calculator) -> calculator attemptNumber
        | _ -> None

module ValidationTimeout =

    /// Create a timeout with validation
    let createTimeout duration =
        if duration <= TimeSpan.Zero then
            Error "Timeout duration must be positive"
        else
            Ok (TimeoutAfter duration)

    /// Check if a validation has timed out
    let hasTimedOut timeoutBehavior currentTime =
        match timeoutBehavior with
        | NoTimeout -> false
        | TimeoutAfter duration -> false // Need start time to calculate this
        | TimeoutAt absoluteTime -> currentTime >= absoluteTime

// ========== MIGRATION HELPERS ==========

module Migration =

    /// Convert old RetryPolicy to new ValidationRetryBehavior
    let convertRetryPolicy (oldPolicy: RetryPolicy option) : ValidationRetryBehavior =
        match oldPolicy with
        | None -> FailImmediately
        | Some policy when policy.MaxAttempts <= 0 -> FailImmediately
        | Some policy when policy.MaxAttempts = 1 -> RetryOnce
        | Some policy when Double.IsNaN(policy.BackoffFactor) || policy.BackoffFactor <= 0.0 ->
            RetryOnce // Fallback for invalid data
        | Some policy when policy.BackoffFactor = 1.0 ->
            RetryWithLinearBackoff (policy.MaxAttempts, 1)
        | Some policy ->
            RetryWithExponentialBackoff (policy.MaxAttempts, 1)

    /// Convert old ValidationState to new RobustValidationState
    let convertValidationState (oldState: ValidationState) : RobustValidationState = {
        Id = oldState.Id
        Kind = oldState.Kind
        IsRequired = oldState.IsRequired
        Status = oldState.Status
        Dependencies = oldState.Dependencies
        Description = oldState.Description
        TimeoutBehavior =
            match oldState.Timeout with
            | None -> NoTimeout
            | Some duration -> TimeoutAfter duration
        RetryBehavior = convertRetryPolicy oldState.RetryPolicy
        CreatedAt = oldState.CreatedAt
        UpdatedAt = oldState.UpdatedAt
    }
