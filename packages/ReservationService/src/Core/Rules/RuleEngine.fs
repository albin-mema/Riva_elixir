namespace ReservationService.Core

open System
open System.Text.Json
open System.Collections.Generic
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleDomain

/// <summary>
/// Core rule interpretation engine that processes rules against reservation data.
/// Follows the ports and adapters pattern with pure functional core.
/// </summary>
module RuleEngine =

    // ========== PORTS (DEPENDENCY INJECTION VIA FUNCTIONS) ==========

    /// <summary>
    /// Port for getting current time. Injected to maintain determinism.
    /// </summary>
    type TimeProvider = unit -> DateTimeOffset

    /// <summary>
    /// Port for resolving time zones. Injected to avoid external dependencies.
    /// </summary>
    type TimeZoneResolver = string -> TimeZoneInfo option

    /// <summary>
    /// Port for emitting trace entries. Injected to separate logging concerns.
    /// </summary>
    type TraceEmitter = string -> unit

    /// <summary>
    /// Port for string comparison operations. Injected for configurability.
    /// </summary>
    type StringComparer = StringComparison -> string -> string -> bool

    /// <summary>
    /// Collection of all ports (dependencies) needed by the rule engine.
    /// This follows the style guide's recommendation for dependency injection via functions.
    /// </summary>
    type RuleEnginePorts = {
        /// <summary>Function to get current time</summary>
        TimeNow: TimeProvider
        /// <summary>Function to resolve time zone information</summary>
        ResolveTimeZone: TimeZoneResolver
        /// <summary>Function to emit trace information</summary>
        EmitTrace: TraceEmitter
        /// <summary>Function to compare strings</summary>
        CompareStrings: StringComparer
    }

    /// Default ports used by the engine when none are provided explicitly
    let private defaultPorts : RuleEnginePorts =
        {
            TimeNow = (fun () -> DateTimeOffset.UtcNow)
            ResolveTimeZone = (fun tz -> try Some (TimeZoneInfo.FindSystemTimeZoneById tz) with _ -> None)
            EmitTrace = (fun _ -> ())
            CompareStrings = (fun comparison a b -> String.Equals(a, b, comparison))
        }


    // ========== RULE EVALUATION RESULTS ==========

    /// <summary>
    /// Result of evaluating a single rule.
    /// Contains all information about the rule execution.
    /// </summary>
    type RuleEvaluationResult = {
        /// <summary>Unique identifier of the rule</summary>
        RuleId: Types.RuleId
        /// <summary>Human-readable name of the rule</summary>
        RuleName: string
        /// <summary>Whether the rule condition matched</summary>
        Matched: bool
        /// <summary>Actions to execute if the rule matched</summary>
        Actions: RuleTypes.RuleAction[]
        /// <summary>Time taken to execute the rule</summary>
        ExecutionTime: TimeSpan
        /// <summary>Error message if rule execution failed</summary>
        Error: string option
        /// <summary>Trace information for debugging</summary>
        Trace: string[] option
    }

    /// <summary>
    /// Result of evaluating all rules in a rule set.
    /// Organizes results by rule category for easier processing.
    /// </summary>
    type RuleSetEvaluationResult = {
        /// <summary>Results from validation rules</summary>
        ValidationResults: RuleEvaluationResult[]
        /// <summary>Results from business rules</summary>
        BusinessRuleResults: RuleEvaluationResult[]
        /// <summary>Results from conflict detection rules</summary>
        ConflictResults: RuleEvaluationResult[]
        /// <summary>Results from pricing rules</summary>
        PricingResults: RuleEvaluationResult[]
        /// <summary>Results from approval rules</summary>
        ApprovalResults: RuleEvaluationResult[]
        /// <summary>Total time taken to evaluate all rules</summary>
        TotalExecutionTime: TimeSpan
        /// <summary>Whether any rules had execution errors</summary>
        HasErrors: bool
        /// <summary>Whether any rules produced critical severity actions</summary>
        HasCriticalErrors: bool
    }

    // ========== PURE CORE FUNCTIONS ==========

    /// <summary>
    /// Extract a value from a JSON document using a field path.
    /// This is a pure function that doesn't depend on external state.
    /// </summary>
    /// <param name="fieldPath">Dot-separated path to the field</param>
    /// <param name="jsonData">JSON document to extract from</param>
    /// <returns>Extracted value as RuleValue option</returns>
    let extractFieldValue (fieldPath: RuleDomain.FieldPath) (jsonData: JsonDocument) : RuleDomain.RuleValue option =
        let rec traverse (element: JsonElement) (parts: string list) =
            match parts with
            | [] -> Some element
            | part :: rest ->
                match element.ValueKind with
                | JsonValueKind.Object ->
                    match element.TryGetProperty(part) with
                    | true, nextElement -> traverse nextElement rest
                    | false, _ -> None
                | JsonValueKind.Array ->
                    match System.Int32.TryParse(part) with
                    | true, index when index >= 0 && index < element.GetArrayLength() ->
                        traverse (element[index]) rest
                    | _ -> None
                | _ -> None

        try
            let parts = fieldPath.Split('.') |> Array.toList
            traverse jsonData.RootElement parts
            |> Option.map SimpleValueOps.ofJsonElement
        with
        | _ -> None

    // ========== CONDITION EVALUATION ==========

    /// <summary>
    /// Compare two values using the specified operator.
    /// Uses injected ports for time zone resolution and string comparison.
    /// This function is pure except for the injected dependencies.
    /// </summary>
    /// <param name="ports">Injected dependencies</param>
    /// <param name="context">Rule execution context</param>
    /// <param name="operator">Comparison operator to use</param>
    /// <param name="leftValue">Left side value</param>
    /// <param name="rightValue">Right side value</param>
    /// <returns>True if comparison succeeds, false otherwise</returns>
    let compareValuesInternal (ports: RuleEnginePorts) (context: RuleExecutionContext) (operator: ComparisonOperator) (leftValue: RuleValue option) (rightValue: RuleValue option) : bool =
        let comparison = if context.CaseInsensitive then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal

        let tzOpt =
            match context.TargetTimeZoneId with
            | Some tzId -> ports.ResolveTimeZone tzId
            | None -> None

        let toUtc (dto: DateTimeOffset) : DateTimeOffset =
            match tzOpt with
            | Some tz -> TimeZoneInfo.ConvertTime(dto, tz).ToUniversalTime()
            | None -> dto.ToUniversalTime()

        let eq l r =
            match l, r with
            | SString a, SString b -> ports.CompareStrings comparison a b
            | SInt a, SInt b -> a = b
            | SDecimal a, SDecimal b -> a = b
            | SInt a, SDecimal b -> (decimal a) = b
            | SDecimal a, SInt b -> a = (decimal b)
            | SBool a, SBool b -> a = b
            | SDateTimeOffset a, SDateTimeOffset b -> toUtc a = toUtc b
            | SNull, SNull -> true
            | _ -> false

        let lt l r =
            match l, r with
            | SInt a, SInt b -> a < b
            | SDecimal a, SDecimal b -> a < b
            | SInt a, SDecimal b -> (decimal a) < b
            | SDecimal a, SInt b -> a < (decimal b)
            | SDateTimeOffset a, SDateTimeOffset b -> toUtc a < toUtc b
            | _ -> false

        let le l r = lt l r || eq l r

        let gt l r =
            match l, r with
            | SInt a, SInt b -> a > b
            | SDecimal a, SDecimal b -> a > b
            | SInt a, SDecimal b -> (decimal a) > b
            | SDecimal a, SInt b -> a > (decimal b)
            | SDateTimeOffset a, SDateTimeOffset b -> toUtc a > toUtc b
            | _ -> false

        let ge l r = gt l r || eq l r

        // Membership only supported against arrays; CSV membership in strings is not supported
        let memberOf (container: RuleValue) (item: RuleValue) =
            match container with
            | SArray arr -> arr |> Array.exists (fun v -> eq v item)
            | _ -> false
        let result =
            match operator, leftValue, rightValue with
            | Equal, Some left, Some right -> eq left right
            | NotEqual, Some left, Some right -> not (eq left right)
            | LessThan, Some left, Some right -> lt left right
            | LessThanOrEqual, Some left, Some right -> le left right
            | GreaterThan, Some left, Some right -> gt left right
            | GreaterThanOrEqual, Some left, Some right -> ge left right
            | Contains, Some (SString a), Some (SString b) -> a.Contains(b, comparison)
            | Contains, Some (SArray arr), Some v -> arr |> Array.exists (fun x -> eq x v)
            | StartsWith, Some (SString a), Some (SString b) -> a.StartsWith(b, comparison)
            | EndsWith, Some (SString a), Some (SString b) -> a.EndsWith(b, comparison)
            | In, Some item, Some container -> memberOf container item
            | NotIn, Some item, Some container -> memberOf container item |> not
            | IsNull, None, _ -> true
            | IsNull, Some _, _ -> false
            | IsNotNull, None, _ -> false
            | IsNotNull, Some _, _ -> true
            | _ -> false

        result

    /// Back-compat: simplified compareValues that uses default ports
    let compareValues (context: RuleExecutionContext) (operator: ComparisonOperator) (leftValue: RuleValue option) (rightValue: RuleValue option) : bool =
        compareValuesInternal defaultPorts context operator leftValue rightValue

    /// <summary>
    /// Evaluate a single rule condition against JSON data using injected ports.
    /// </summary>
    /// <param name="ports">Injected dependencies</param>
    /// <param name="condition">The condition to evaluate</param>
    /// <param name="jsonData">JSON data to evaluate against</param>
    /// <param name="context">Rule execution context</param>
    /// <returns>True if condition matches, false otherwise</returns>
    let evaluateCondition (ports: RuleEnginePorts) (condition: RuleCondition) (jsonData: JsonDocument) (context: RuleExecutionContext) : bool =
        let leftValue = extractFieldValue condition.Field jsonData

        let rightValue =
            match condition.ReferenceField with
            | Some refField -> extractFieldValue refField jsonData
            | None -> condition.Value

        compareValuesInternal ports context condition.Operator leftValue rightValue

    /// <summary>
    /// Evaluate a complex condition recursively using injected ports.
    /// Handles both simple conditions and composite conditions with logical operators.
    /// </summary>
    /// <param name="ports">Injected dependencies</param>
    /// <param name="condition">The complex condition to evaluate</param>
    /// <param name="jsonData">JSON data to evaluate against</param>
    /// <param name="context">Rule execution context</param>
    /// <returns>True if condition matches, false otherwise</returns>
    let rec evaluateComplexCondition (ports: RuleEnginePorts) (condition: ComplexCondition) (jsonData: JsonDocument) (context: RuleExecutionContext) : bool =
        match condition with
        | Simple ruleCondition ->
            evaluateCondition ports ruleCondition jsonData context

        | Composite (logicalOp, conditions) ->


            match logicalOp, conditions with
            | And, [] -> true
            | And, conditions ->
                conditions
                |> List.map (fun c -> evaluateComplexCondition ports c jsonData context)
                |> List.forall id

            | Or, [] -> false
            | Or, conditions ->
                conditions
                |> List.map (fun c -> evaluateComplexCondition ports c jsonData context)
                |> List.exists id

            | Not, [condition] ->
                not (evaluateComplexCondition ports condition jsonData context)

            | Not, _ -> false

    // ========== RULE EVALUATION ==========

    /// <summary>
    /// Evaluate a single rule against JSON data using injected ports.
    /// Uses the injected time provider for deterministic timing.
    /// </summary>
    /// <param name="ports">Injected dependencies</param>
    /// <param name="rule">The rule to evaluate</param>
    /// <param name="jsonData">JSON data to evaluate against</param>
    /// <param name="context">Rule execution context</param>
    /// <returns>Result of rule evaluation</returns>
    let evaluateRule (ports: RuleEnginePorts) (rule: IRule) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult =
        let startTime = ports.TimeNow()

        try
            let matched =
                match rule.Condition with
                | Some condition -> evaluateComplexCondition ports condition jsonData context
                | None -> true // Rules without conditions always match

            let actions = if matched then rule.Actions else [||]

            let endTime = ports.TimeNow()
            let executionTime = endTime - startTime

            {
                RuleId = rule.Id
                RuleName = rule.Name
                Matched = matched
                Actions = actions
                ExecutionTime = executionTime
                Error = None
                Trace = None
            }
        with
        | ex ->
            let endTime = ports.TimeNow()
            let executionTime = endTime - startTime

            {
                RuleId = rule.Id
                RuleName = rule.Name
                Matched = false
                Actions = [||]
                ExecutionTime = executionTime
                Error = Some ex.Message
                Trace = None
            }

    // ========== RULE SET EVALUATION ==========

    /// Evaluate all validation rules with enhanced error handling
    let evaluateValidationRules (ports: RuleEnginePorts) (rules: ValidationRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule ports r.Rule jsonData context)

    /// Evaluate all business rules
    let evaluateBusinessRules (ports: RuleEnginePorts) (rules: BusinessRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule ports r.Rule jsonData context)

    /// Evaluate all conflict rules
    let evaluateConflictRules (ports: RuleEnginePorts) (rules: ConflictRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule ports r.Rule jsonData context)

    /// Evaluate all pricing rules
    let evaluatePricingRules (ports: RuleEnginePorts) (rules: PricingRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule ports r.Rule jsonData context)

    /// Evaluate all approval rules
    let evaluateApprovalRules (ports: RuleEnginePorts) (rules: ApprovalRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule ports r.Rule jsonData context)

    // ========== MAIN INTERPRETATION FUNCTION ==========

    /// Interpret all rules with custom ports (adapters provide effects)
    let interpretWith (ports: RuleEnginePorts) (ruleSet: RuleSet) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleSetEvaluationResult =
        let startTime = ports.TimeNow()
        let validationResults = evaluateValidationRules ports ruleSet.ValidationRules jsonData context
        let businessRuleResults = evaluateBusinessRules ports ruleSet.BusinessRules jsonData context
        let conflictResults = evaluateConflictRules ports ruleSet.ConflictRules jsonData context
        let pricingResults = evaluatePricingRules ports ruleSet.PricingRules jsonData context
        let approvalResults = evaluateApprovalRules ports ruleSet.ApprovalRules jsonData context
        let endTime = ports.TimeNow()
        let totalExecutionTime = endTime - startTime
        let allResults = [| validationResults; businessRuleResults; conflictResults; pricingResults; approvalResults |] |> Array.concat
        let hasErrors = allResults |> Array.exists (fun r -> r.Error.IsSome)
        let hasCriticalErrors = allResults |> Array.exists (fun r -> r.Actions |> Array.exists (fun a -> a.Severity = Critical))
        {
            ValidationResults = validationResults
            BusinessRuleResults = businessRuleResults
            ConflictResults = conflictResults
            PricingResults = pricingResults
            ApprovalResults = approvalResults
            TotalExecutionTime = totalExecutionTime
            HasErrors = hasErrors
            HasCriticalErrors = hasCriticalErrors
        }

    /// Main function to interpret all rules against reservation data
    let interpretRules (ruleSet: RuleSet) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleSetEvaluationResult =
        interpretWith defaultPorts ruleSet jsonData context
