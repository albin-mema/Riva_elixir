namespace ReservationService.Core.Dsl

open System
open System.Text.Json
open System.Text.Json.Serialization
open ReservationService.Core.RuleDomain
open ReservationService.Core.RuleTypes
open ReservationService.Core.Types

module Parser =
    open Types


    // -------- Strictness helpers --------
    let private isKnownOperator (op:string) =
        match op with
        | "Equal" | "=="
        | "NotEqual" | "!="
        | "LessThan" | "<"
        | "LessThanOrEqual" | "<="
        | "GreaterThan" | ">"
        | "GreaterThanOrEqual" | ">="
        | "Contains"
        | "StartsWith"
        | "EndsWith"
        | "In"
        | "NotIn"
        | "IsNull"
        | "IsNotNull" -> true
        | _ -> false

    // Compute max depth and max array length of a JsonElement
    let rec private computeDepthAndArray (el: System.Text.Json.JsonElement) (depth:int) : int * int =
        let mutable maxDepth = depth
        let mutable maxArr = 0
        match el.ValueKind with
        | System.Text.Json.JsonValueKind.Object ->
            for p in el.EnumerateObject() do
                let d,a = computeDepthAndArray p.Value (depth + 1)
                if d > maxDepth then maxDepth <- d
                if a > maxArr then maxArr <- a
            (maxDepth, maxArr)
        | System.Text.Json.JsonValueKind.Array ->
            let len = el.GetArrayLength()
            maxArr <- max maxArr len
            for v in el.EnumerateArray() do
                let d,a = computeDepthAndArray v (depth + 1)
                if d > maxDepth then maxDepth <- d
                if a > maxArr then maxArr <- max maxArr a
            (maxDepth, maxArr)
        | _ -> (maxDepth, maxArr)

    // Path existence check using dotted paths with numeric array indices
    let private tryTraverse (root: System.Text.Json.JsonElement) (path:string) : bool =
        let parts = path.Split('.') |> Array.toList
        let rec go (elem: System.Text.Json.JsonElement) (ps: string list) : bool =
            match ps with
            | [] -> true
            | p::rest ->
                match elem.ValueKind with
                | System.Text.Json.JsonValueKind.Object ->
                    match elem.TryGetProperty(p) with
                    | true, next -> go next rest
                    | _ -> false
                | System.Text.Json.JsonValueKind.Array ->
                    match System.Int32.TryParse(p) with
                    | true, idx when idx >= 0 && idx < elem.GetArrayLength() ->
                        go (elem.[idx]) rest
                    | _ -> false
                | _ -> false
        go root parts

    // Collect all field paths used in a condition tree
    let rec private collectPaths (c: Types.Cond) : string list =
        match c with
        | Types.Cond.Simple s ->
            let rf = s.ReferenceField |> Option.toList
            s.Field :: rf
        | Types.Cond.Composite comp ->
            comp.Children |> Array.toList |> List.collect collectPaths

    // Map string to ComparisonOperator (version 1)
    let private parseOperator (op:string) : ComparisonOperator =
        match op with
        | "Equal" | "==" -> Equal
        | "NotEqual" | "!=" -> NotEqual
        | "LessThan" | "<" -> LessThan
        | "LessThanOrEqual" | "<=" -> LessThanOrEqual
        | "GreaterThan" | ">" -> GreaterThan
        | "GreaterThanOrEqual" | ">=" -> GreaterThanOrEqual
        | "Contains" -> Contains
        | "StartsWith" -> StartsWith
        | "EndsWith" -> EndsWith
        | "In" -> In
        | "NotIn" -> NotIn
        | "IsNull" -> IsNull
        | "IsNotNull" -> IsNotNull
        | x -> failwith ($"Unknown operator: {x}")

    let private toRuleValue (v: DslValue) : RuleValue =
        match v with
        | VString s -> SString s
        | VInt i -> SInt i
        | VDecimal d -> SDecimal d
        | VBool b -> SBool b
        | VDateTime dt -> SDateTimeOffset (DateTimeOffset(dt))
        | VDate d -> SString (d.ToString("yyyy-MM-dd"))
        | VTime t -> SString (t.ToString("HH:mm:ss"))
        | VDuration ts -> SString (ts.ToString())
        | VArray arr ->
            let rec conv (x:DslValue) : RuleValue =
                match x with
                | VString s -> SString s
                | VInt i -> SInt i
                | VDecimal d -> SDecimal d
                | VBool b -> SBool b
                | VDateTime dt -> SDateTimeOffset (DateTimeOffset(dt))
                | VDate d -> SString (d.ToString("yyyy-MM-dd"))
                | VTime t -> SString (t.ToString("HH:mm:ss"))
                | VDuration ts -> SString (ts.ToString())
                | VArray inner -> inner |> Array.map conv |> SArray
                | VObject _ -> SString "{object}"
                | VNull -> SNull
            arr |> Array.map conv |> SArray
        | VObject _ -> SString "{object}"
        | VNull -> SNull

    let private toActionType (s:string) : RuleActionType =
        match s with
        | "Validation" -> RuleActionType.Validation
        | "BusinessLogic" -> RuleActionType.BusinessLogic
        | "ConflictDetection" -> RuleActionType.ConflictDetection
        | "Pricing" -> RuleActionType.Pricing
        | "Approval" -> RuleActionType.Approval
        | "Notification" -> RuleActionType.Notification
        | other -> RuleActionType.Unknown other

    let private toSeverity (s:string option) : RuleSeverity =
        match s |> Option.defaultValue "Error" with
        | "Info" -> RuleSeverity.Info
        | "Warning" -> RuleSeverity.Warning
        | "Error" -> RuleSeverity.Error
        | "Critical" -> RuleSeverity.Critical
        | other -> RuleSeverity.Unknown other

    let private toValidationType (sOpt:string option) : ReservationService.Core.RuleTypes.ValidationType =
        match sOpt |> Option.defaultValue "reservation" with
        | "reservation" -> ReservationService.Core.RuleTypes.ValidationType.Reservation
        | "customer" -> ReservationService.Core.RuleTypes.ValidationType.Customer
        | "resource" -> ReservationService.Core.RuleTypes.ValidationType.Resource
        | "payment" -> ReservationService.Core.RuleTypes.ValidationType.Payment
        | other -> ReservationService.Core.RuleTypes.ValidationType.Unknown other

    // Compile a Cond to ComplexCondition
    let rec private compileCond (c:Cond) : ComplexCondition =
        match c with
        | Cond.Simple s ->
            ComplexCondition.Simple {
                Field = s.Field
                Operator = parseOperator s.Operator
                Value = s.Value |> Option.map toRuleValue
                ReferenceField = s.ReferenceField
            }
        | Cond.Composite comp ->
            let op =
                match comp.Op with
                | "And" -> LogicalOperator.And
                | "Or" -> LogicalOperator.Or
                | "Not" -> LogicalOperator.Not
                | x -> failwith ($"Unknown logical operator: {x}")
            ComplexCondition.Composite (op, comp.Children |> Array.toList |> List.map compileCond)

    let compileRule (r: RuleDef) : string * ValidationRule =
        let condition = compileCond r.Condition
        let actions : RuleAction[] =
            r.Actions
            |> Array.map (fun a -> { Type = toActionType a.Type; Message = r.Name; Severity = toSeverity a.Severity; Parameters = (a.Params |> Option.defaultValue Map.empty |> Map.map (fun _ v -> match v with | VString s -> s | _ -> v.ToString())) })
        let baseRule : IRule = {
            Id = ReservationService.Core.Types.RuleId (System.Guid.Parse r.Id)
            Name = r.Name
            Description = r.Description |> Option.defaultValue ""
            Condition = Some condition
            Actions = actions
            Priority = r.Priority |> Option.defaultValue 100
            Enabled = r.Enabled |> Option.defaultValue true
            Tags = r.Tags |> Option.defaultValue [||]
        }
        let validation : ValidationRule = {
            Rule = baseRule
            ValidationType = (toValidationType r.Scope)
            ErrorMessage = r.Name
        }
        (r.Id, validation)

    // Compile the DSL request (strict)
    let compileRequest (json:string) : RuleSet * JsonDocument * RuleExecutionContext =
        // Parse raw JSON and strongly-typed Request
        let jdFull = JsonDocument.Parse(json)
        let opts = JsonSerializerOptions()
        opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
        opts.Converters.Add(JsonFSharpConverter())
        let req : Types.Request = System.Text.Json.JsonSerializer.Deserialize<Types.Request>(json, opts)
        if obj.ReferenceEquals(req, null) then failwith "Invalid DSL request JSON"

        // Strict schema version
        if not (String.Equals(req.SchemaVersion, "1.0", StringComparison.Ordinal)) then
            failwith (sprintf "Unsupported schemaVersion: %s" req.SchemaVersion)

        // Extract the 'data' subtree as the evaluation root (required)
        let hasData, dataElem = jdFull.RootElement.TryGetProperty("data")
        if not hasData then failwith "Missing required 'data' property"
        let dataDoc = JsonDocument.Parse(dataElem.GetRawText())

        // Evaluation settings with strict defaults
        let eval = req.Settings |> Option.bind (fun s -> s.Evaluation)
        let maxDepth = eval |> Option.bind (fun e -> e.MaxDepth) |> Option.defaultValue 12
        let maxRules = eval |> Option.bind (fun e -> e.MaxRules) |> Option.defaultValue 200
        let maxArray = eval |> Option.bind (fun e -> e.MaxArray) |> Option.defaultValue 1000
        let failOnUnknownField = eval |> Option.bind (fun e -> e.FailOnUnknownField) |> Option.defaultValue true
        let unknownOpPolicy = eval |> Option.bind (fun e -> e.UnknownOperatorPolicy) |> Option.defaultValue "error"

        // Bounds: rules count
        if req.Rules.Length > maxRules then
            failwith (sprintf "Rule count %d exceeds MaxRules %d" req.Rules.Length maxRules)

        // Bounds: data depth and array sizes
        let dDepth, dArr = computeDepthAndArray dataDoc.RootElement 1
        if dDepth > maxDepth then failwith (sprintf "Data depth %d exceeds MaxDepth %d" dDepth maxDepth)
        if dArr > maxArray then failwith (sprintf "Array size %d exceeds MaxArray %d" dArr maxArray)

        // Alias normalization
        let aliasMap = req.Domain |> Option.bind (fun d -> d.Aliases) |> Option.defaultValue Map.empty
        let normalizePath (p:string) = match Map.tryFind p aliasMap with | Some v -> v | None -> p
        let rec normalizeCond (c: Types.Cond) : Types.Cond =
            match c with
            | Cond.Simple s ->
                let rf' = s.ReferenceField |> Option.map normalizePath
                Cond.Simple { s with Field = normalizePath s.Field; ReferenceField = rf' }
            | Cond.Composite comp ->
                Cond.Composite { comp with Children = comp.Children |> Array.map normalizeCond }
        let normalizedRules : Types.RuleDef array =
            req.Rules |> Array.map (fun r -> { r with Condition = normalizeCond r.Condition })

        // Validate operators (respect policy, default error)
        if unknownOpPolicy <> "ignore" then
            normalizedRules
            |> Array.iter (fun r ->
                let rec validateCond (c: Types.Cond) =
                    match c with
                    | Types.Cond.Simple s -> if not (isKnownOperator s.Operator) then failwith (sprintf "Unknown operator: %s" s.Operator)
                    | Types.Cond.Composite comp -> comp.Children |> Array.iter validateCond
                validateCond r.Condition)

        // Validate that referenced fields exist (when strict)
        if failOnUnknownField then
            let missing =
                normalizedRules
                |> Array.collect (fun r -> collectPaths r.Condition |> List.toArray)
                |> Array.map normalizePath
                |> Array.distinct
                |> Array.filter (fun p -> not (tryTraverse dataDoc.RootElement p))
            if missing.Length > 0 then
                failwith (sprintf "Unknown field path(s): %s" (String.Join(", ", missing)))

        // Compile rules
        let compiled = normalizedRules |> Array.map compileRule |> Array.map snd
        let rules: RuleSet = {
            ValidationRules = compiled
            BusinessRules = [||]
            ConflictRules = [||]
            PricingRules = [||]
            ApprovalRules = [||]
            Metadata = Map.empty
        }
        let ctx : RuleExecutionContext = {
            TraceEnabled = req.Settings |> Option.bind (fun s -> s.Explain) |> Option.defaultValue false
            UserId = None
            SessionId = None
            AdditionalData = Map.empty
            CaseInsensitive = true
            TargetTimeZoneId = None
            CollectTrace = false
        }
        (rules, dataDoc, ctx)

    /// Compile only rules (validation-only RuleSet)
    let compileRules (ruleDefs: RuleDef array) : RuleSet =
        let compiled = ruleDefs |> Array.map compileRule |> Array.map snd
        {
            ValidationRules = compiled
            BusinessRules = [||]
            ConflictRules = [||]
            PricingRules = [||]
            ApprovalRules = [||]
            Metadata = Map.empty
        }


