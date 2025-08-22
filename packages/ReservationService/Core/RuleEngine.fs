namespace ReservationService.Core

open System
open System.Text.Json
open ReservationService.Core.RuleTypes
open ReservationService.Core

/// Core rule interpretation engine that processes rules against reservation data
module RuleEngine =

    // ========== RULE EVALUATION RESULTS ==========

    /// Result of evaluating a single rule
    type RuleEvaluationResult = {
        RuleId: string
        RuleName: string
        Matched: bool
        Actions: RuleAction[]
        ExecutionTime: TimeSpan
        Error: string option
        Trace: string[] option
    }

    /// Result of evaluating all rules
    type RuleSetEvaluationResult = {
        ValidationResults: RuleEvaluationResult[]
        BusinessRuleResults: RuleEvaluationResult[]
        ConflictResults: RuleEvaluationResult[]
        PricingResults: RuleEvaluationResult[]
        ApprovalResults: RuleEvaluationResult[]
        TotalExecutionTime: TimeSpan
        HasErrors: bool
        HasCriticalErrors: bool
    }

    // ========== FIELD VALUE EXTRACTION ==========

    /// Extract a value from a JSON document using a field path
    let extractFieldValue (fieldPath: FieldPath) (jsonData: JsonDocument) : RuleValue option =
        try
            let parts = fieldPath.Split('.')
            let mutable current = jsonData.RootElement
            let mutable ok = true
            for part in parts do
                if ok then
                    let mutable tmp = Unchecked.defaultof<JsonElement>
                    if current.ValueKind = JsonValueKind.Object && current.TryGetProperty(part, &tmp) then
                        current <- tmp
                    else
                        ok <- false
            if ok then Some (SimpleValueOps.ofJsonElement current) else None
        with
        | _ -> None

    // ========== CONDITION EVALUATION ==========

    /// Compare two values using the specified operator
    let compareValues (operator: ComparisonOperator) (leftValue: RuleValue option) (rightValue: RuleValue option) : bool =
        let eq l r = match l, r with
                      | SString a, SString b -> a = b
                      | SInt a, SInt b -> a = b
                      | SDecimal a, SDecimal b -> a = b
                      | SBool a, SBool b -> a = b
                      | SDateTime a, SDateTime b -> a = b
                      | SNull, SNull -> true
                      | _ -> false
        match leftValue, rightValue with
        | Some left, Some right ->
            let lt l r =
                match l, r with
                | SInt a, SInt b -> a < b
                | SDecimal a, SDecimal b -> a < b
                | SDateTime a, SDateTime b -> a < b
                | _ -> false
            let le l r = lt l r || eq l r
            let gt l r =
                match l, r with
                | SInt a, SInt b -> a > b
                | SDecimal a, SDecimal b -> a > b
                | SDateTime a, SDateTime b -> a > b
                | _ -> false
            let ge l r = gt l r || eq l r
            match operator with
            | Equals -> eq left right
            | NotEquals -> not (eq left right)
            | LessThan -> lt left right
            | LessThanOrEqual -> le left right
            | GreaterThan -> gt left right
            | GreaterThanOrEqual -> ge left right
            | Contains ->
                match left, right with
                | SString a, SString b -> a.Contains(b)
                | SArray arr, v -> arr |> Array.exists ((=) v)
                | _ -> false
            | NotContains ->
                match left, right with
                | SString a, SString b -> not (a.Contains(b))
                | SArray arr, v -> arr |> Array.exists ((=) v) |> not
                | _ -> false
            | IsNull -> false
            | IsNotNull -> true
            | Between
            | NotBetween
            | In
            | NotIn
            | Matches -> false // not implemented generically
        | None, _ when operator = IsNull -> true
        | None, _ when operator = IsNotNull -> false
        | Some _, _ when operator = IsNull -> false
        | Some _, _ when operator = IsNotNull -> true
        | _ -> false

    /// Evaluate a single rule condition against JSON data
    let evaluateCondition (condition: RuleCondition) (jsonData: JsonDocument) (context: RuleExecutionContext) : bool =
        let leftValue = extractFieldValue condition.Field jsonData
        
        let rightValue = 
            match condition.ReferenceField with
            | Some refField -> extractFieldValue refField jsonData
            | None -> condition.Value
        
        compareValues condition.Operator leftValue rightValue

    /// Evaluate a complex condition (with logical operators)
    let rec evaluateComplexCondition (condition: ComplexCondition) (jsonData: JsonDocument) (context: RuleExecutionContext) : bool =
        match condition with
        | Simple ruleCondition -> evaluateCondition ruleCondition jsonData context
        | Composite (logicalOp, conditions) ->
            match logicalOp, conditions with
            | And, conditions -> conditions |> List.forall (fun c -> evaluateComplexCondition c jsonData context)
            | Or, conditions -> conditions |> List.exists (fun c -> evaluateComplexCondition c jsonData context)
            | Not, [condition] -> not (evaluateComplexCondition condition jsonData context)
            | Not, _ -> false // Invalid Not condition with multiple operands
            | _, [] -> true // Empty condition list defaults to true

    // ========== RULE EVALUATION ==========

    /// Evaluate a single rule against JSON data
    let evaluateRule (rule: IRule) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult =
        let startTime = DateTime.UtcNow
        
        try
            let matched = 
                match rule.Condition with
                | Some condition -> evaluateComplexCondition condition jsonData context
                | None -> true // Rules without conditions always match
            
            let actions = if matched then rule.Actions else [||]
            
            let endTime = DateTime.UtcNow
            let executionTime = endTime - startTime
            
            {
                RuleId = rule.Id
                RuleName = rule.Name
                Matched = matched
                Actions = actions
                ExecutionTime = executionTime
                Error = None
                Trace = if context.TraceEnabled then Some [| sprintf "Rule %s: %s" rule.Id (if matched then "MATCHED" else "NO MATCH") |] else None
            }
        with
        | ex ->
            let endTime = DateTime.UtcNow
            let executionTime = endTime - startTime

            {
                RuleId = rule.Id
                RuleName = rule.Name
                Matched = false
                Actions = [||]
                ExecutionTime = executionTime
                Error = Some ex.Message
                Trace = if context.TraceEnabled then Some [| sprintf "Rule %s: ERROR - %s" rule.Id ex.Message |] else None
            }

    // ========== RULE SET EVALUATION ==========

    /// Evaluate all validation rules
    let evaluateValidationRules (rules: ValidationRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule r.Rule jsonData context)

    /// Evaluate all business rules
    let evaluateBusinessRules (rules: BusinessRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule r.Rule jsonData context)

    /// Evaluate all conflict rules
    let evaluateConflictRules (rules: ConflictRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule r.Rule jsonData context)

    /// Evaluate all pricing rules
    let evaluatePricingRules (rules: PricingRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule r.Rule jsonData context)

    /// Evaluate all approval rules
    let evaluateApprovalRules (rules: ApprovalRule[]) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleEvaluationResult[] =
        rules
        |> Array.filter (fun r -> r.Rule.Enabled)
        |> Array.sortByDescending (fun r -> r.Rule.Priority)
        |> Array.map (fun r -> evaluateRule r.Rule jsonData context)

    // ========== MAIN INTERPRETATION FUNCTION ==========

    /// Main function to interpret all rules against reservation data
    let interpretRules (ruleSet: RuleSet) (jsonData: JsonDocument) (context: RuleExecutionContext) : RuleSetEvaluationResult =
        let startTime = DateTime.UtcNow
        
        let validationResults = evaluateValidationRules ruleSet.ValidationRules jsonData context
        let businessRuleResults = evaluateBusinessRules ruleSet.BusinessRules jsonData context
        let conflictResults = evaluateConflictRules ruleSet.ConflictRules jsonData context
        let pricingResults = evaluatePricingRules ruleSet.PricingRules jsonData context
        let approvalResults = evaluateApprovalRules ruleSet.ApprovalRules jsonData context
        
        let endTime = DateTime.UtcNow
        let totalExecutionTime = endTime - startTime
        
        let allResults = [| validationResults; businessRuleResults; conflictResults; pricingResults; approvalResults |] |> Array.concat
        
        let hasErrors = allResults |> Array.exists (fun r -> r.Error.IsSome)
        let hasCriticalErrors = 
            allResults
            |> Array.exists (fun r ->
                r.Actions |> Array.exists (fun a -> a.Severity = RuleSeverity.Critical))
        
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
