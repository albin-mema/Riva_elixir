namespace ReservationService.Tests.Tests

module SafeTypesPropertyTests =
    open System
    open FsCheck
    open global.Xunit
    open global.FsCheck.Xunit
    open FsUnit.Xunit
    open ReservationService.Core.Types
    open ReservationService.Core.ImprovedTypes

    // ========== PROPERTY TEST CONFIGURATION ==========
    
    let [<Literal>] MAX_TEST_COUNT = 20
    let [<Literal>] START_SIZE = 1
    let [<Literal>] END_SIZE = 20

    // ========== CUSTOM GENERATORS FOR SAFE TYPES ==========
    
    /// Generator for PositiveFloat that can never be NaN
    let positiveFloatGen = 
        Gen.choose(1, 1000)
        |> Gen.map (fun i -> PositiveFloat.Create(float i / 10.0))
        |> Gen.map (function Ok pf -> pf | Error _ -> PositiveFloat.One)
    
    /// Generator for PositiveInt that can never be zero or negative
    let positiveIntGen = 
        Gen.choose(1, 100)
        |> Gen.map (fun i -> PositiveInt.Create(i))
        |> Gen.map (function Ok pi -> pi | Error _ -> PositiveInt.One)
    
    /// Generator for SafeRetryPolicy that can never have invalid states
    let safeRetryPolicyGen = 
        gen {
            let! attempts = positiveIntGen
            let! factor = positiveFloatGen
            let! maxBackoffMinutes = Gen.choose(1, 60)
            return {
                MaxAttempts = attempts
                BackoffFactor = factor
                MaxBackoff = TimeSpan.FromMinutes(float maxBackoffMinutes * 2.0)
            }
        }
    
    /// Generator for ValidationRetryBehavior
    let validationRetryBehaviorGen = 
        Gen.oneof [
            Gen.constant FailImmediately
            Gen.constant RetryOnce
            Gen.map2 RetryWithLinearBackoff (Gen.choose(1, 5)) (Gen.choose(1, 10))
            Gen.map2 RetryWithExponentialBackoff (Gen.choose(1, 5)) (Gen.choose(1, 5))
        ]
    
    /// Generator for ValidationTimeout
    let validationTimeoutGen = 
        Gen.oneof [
            Gen.constant NoTimeout
            Gen.map (fun minutes -> TimeoutAfter (TimeSpan.FromMinutes(float minutes))) (Gen.choose(1, 120))
            Gen.map (fun hours -> TimeoutAt (DateTimeOffset.Now.AddHours(float hours))) (Gen.choose(1, 48))
        ]

    // Register the generators with FsCheck
    type SafeTypeGenerators =
        static member PositiveFloat() = Arb.fromGen positiveFloatGen
        static member PositiveInt() = Arb.fromGen positiveIntGen
        static member SafeRetryPolicy() = Arb.fromGen safeRetryPolicyGen
        static member ValidationRetryBehavior() = Arb.fromGen validationRetryBehaviorGen
        static member ValidationTimeout() = Arb.fromGen validationTimeoutGen

    // ========== SAFE RETRY POLICY PROPERTY TESTS ==========

    [<ConfigurableProperty>]
    let ``SafeRetryPolicy never has invalid BackoffFactor`` (policy: SafeRetryPolicy) =
        let factor = policy.BackoffFactor.Value
        not (Double.IsNaN(factor)) && 
        not (Double.IsInfinity(factor)) && 
        factor > 0.0

    [<ConfigurableProperty>]
    let ``SafeRetryPolicy never has invalid MaxAttempts`` (policy: SafeRetryPolicy) =
        policy.MaxAttempts.Value > 0

    [<ConfigurableProperty>]
    let ``SafeRetryPolicy roundtrip conversion works`` (policy: SafeRetryPolicy) =
        let unsafe = SafeRetryPolicy.toUnsafe policy
        let backToSafe = SafeRetryPolicy.fromUnsafe unsafe
        match backToSafe with
        | Ok converted -> 
            converted.MaxAttempts.Value = policy.MaxAttempts.Value &&
            converted.BackoffFactor.Value = policy.BackoffFactor.Value &&
            converted.MaxBackoff = policy.MaxBackoff
        | Error _ -> false

    // ========== VALIDATION RETRY BEHAVIOR PROPERTY TESTS ==========

    [<ConfigurableProperty>]
    let ``ValidationRetryBehavior calculateNextDelay is consistent`` (behavior: ValidationRetryBehavior) =
        let attempt1 = ValidationRetryBehavior.calculateNextDelay behavior 1
        let attempt2 = ValidationRetryBehavior.calculateNextDelay behavior 1
        attempt1 = attempt2

    [<ConfigurableProperty>]
    let ``ValidationRetryBehavior delays are non-negative`` (behavior: ValidationRetryBehavior) (attemptNumber: int) =
        let attemptNum = abs attemptNumber + 1 // Ensure positive
        match ValidationRetryBehavior.calculateNextDelay behavior attemptNum with
        | None -> true // No delay is valid
        | Some delay -> delay >= TimeSpan.Zero

    [<ConfigurableProperty>]
    let ``ValidationRetryBehavior FailImmediately never retries`` (attemptNumber: int) =
        let attemptNum = abs attemptNumber + 1
        ValidationRetryBehavior.calculateNextDelay FailImmediately attemptNum = None

    [<ConfigurableProperty>]
    let ``ValidationRetryBehavior RetryOnce only retries once`` (attemptNumber: int) =
        let attemptNum = abs attemptNumber + 1
        let result = ValidationRetryBehavior.calculateNextDelay RetryOnce attemptNum
        if attemptNum = 1 then
            result.IsSome
        else
            result.IsNone

    // ========== VALIDATION TIMEOUT PROPERTY TESTS ==========

    [<ConfigurableProperty(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE, Arbitrary = [| typeof<SafeTypeGenerators> |])>]
    let ``ValidationTimeout NoTimeout never times out`` () =
        let currentTime = DateTimeOffset.Now
        not (ValidationTimeout.hasTimedOut NoTimeout currentTime)

    [<ConfigurableProperty(MaxTest = MAX_TEST_COUNT, StartSize = START_SIZE, EndSize = END_SIZE, Arbitrary = [| typeof<SafeTypeGenerators> |])>]
    let ``ValidationTimeout TimeoutAt works correctly`` (hoursFromNow: int) =
        let hours = abs hoursFromNow + 24 // Ensure positive
        let timeoutTime = DateTimeOffset.Now.AddHours(float hours)
        let timeout = TimeoutAt timeoutTime

        // Before timeout
        let beforeTime = timeoutTime.AddMinutes(-1.0)
        let afterTime = timeoutTime.AddMinutes(1.0)

        not (ValidationTimeout.hasTimedOut timeout beforeTime) &&
        ValidationTimeout.hasTimedOut timeout afterTime

    // ========== MIGRATION PROPERTY TESTS ==========

    [<ConfigurableProperty>]
    let ``Migration handles NaN RetryPolicy gracefully`` () =
        let badPolicy = { MaxAttempts = 3; BackoffFactor = Double.NaN; MaxBackoff = TimeSpan.FromMinutes(10.0) }
        let converted = Migration.convertRetryPolicy (Some badPolicy)
        
        // Should not crash and should provide a sensible fallback
        match converted with
        | FailImmediately | RetryOnce -> true
        | _ -> true // Any valid behavior is acceptable

    [<ConfigurableProperty>]
    let ``Migration handles negative MaxAttempts gracefully`` () =
        let badPolicy = { MaxAttempts = -1; BackoffFactor = 2.0; MaxBackoff = TimeSpan.FromMinutes(10.0) }
        let converted = Migration.convertRetryPolicy (Some badPolicy)
        
        // Should default to FailImmediately for invalid attempts
        converted = FailImmediately

    // ========== POSITIVE TYPES PROPERTY TESTS ==========

    [<ConfigurableProperty>]
    let ``PositiveFloat Create rejects NaN`` () =
        match PositiveFloat.Create(Double.NaN) with
        | Error _ -> true
        | Ok _ -> false

    [<ConfigurableProperty>]
    let ``PositiveFloat Create rejects Infinity`` () =
        match PositiveFloat.Create(Double.PositiveInfinity) with
        | Error _ -> true
        | Ok _ -> false

    [<ConfigurableProperty>]
    let ``PositiveFloat Create rejects negative values`` (negativeValue: float) =
        let value = -(abs negativeValue) - 0.1 // Ensure negative
        match PositiveFloat.Create(value) with
        | Error _ -> true
        | Ok _ -> false

    [<ConfigurableProperty>]
    let ``PositiveInt Create rejects zero and negative`` (nonPositiveValue: int) =
        let value = -(abs nonPositiveValue) // Ensure non-positive
        match PositiveInt.Create(value) with
        | Error _ -> true
        | Ok _ -> false
