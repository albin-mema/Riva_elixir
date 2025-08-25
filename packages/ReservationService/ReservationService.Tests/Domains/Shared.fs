namespace ReservationService.Tests.Domains

open System
open System.Text.Json
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleDomain
open ReservationService.Core.RuleEngine

/// Shared helpers for domain scenario tests
module Shared =

    // ---- JSON builders ----

    /// Merge reservation JSON with optional resourceAvailability JSON
    let mergeReservationAndResources (reservationJson: string) (resourcesJson: string option) : string =
        match resourcesJson with
        | Some r when not (String.IsNullOrWhiteSpace r) ->
            match System.Text.Json.Nodes.JsonNode.Parse(reservationJson) with
            | :? System.Text.Json.Nodes.JsonObject as resObj ->
                match System.Text.Json.Nodes.JsonNode.Parse(r) with
                | :? System.Text.Json.Nodes.JsonObject as obj ->
                    let ra = if obj.ContainsKey("resourceAvailability") then obj["resourceAvailability"] else (obj :> System.Text.Json.Nodes.JsonNode)
                    let raNode = match ra with | null -> null | _ -> ra.DeepClone()
                    resObj["resourceAvailability"] <- raNode
                | _ -> ()
                resObj.ToJsonString()
            | _ -> reservationJson
        | _ -> reservationJson

    // ---- Common rule builders (keep generic across domains) ----

    let capacityRule (id: string) (participantsField: string) (capacityField: string) : ValidationRule =
        let cond = ComplexCondition.Simple { Field = participantsField; Operator = GreaterThan; Value = None; ReferenceField = Some capacityField }
        let action = { Type = Validation; Message = "Capacity exceeded"; Severity = Critical; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Capacity" "" (Some cond) [| action |]
          ValidationType = "capacity"; ErrorMessage = "Capacity exceeded" }

    let minParticipantsRule (id: string) (min: int) : ValidationRule =
        let cond = ComplexCondition.Simple { Field = "participants"; Operator = LessThan; Value = Some (SInt min); ReferenceField = None }
        let action = { Type = Validation; Message = $"Minimum {min} participant(s) required"; Severity = Critical; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Minimum participants" "" (Some cond) [| action |]
          ValidationType = "min-participants"; ErrorMessage = $"Minimum {min} participant(s) required" }

    let leadTimeRule (id: string) (leadMinutesField: string) (minMinutes: int) : ValidationRule =
        let cond = ComplexCondition.Simple { Field = leadMinutesField; Operator = LessThan; Value = Some (SInt minMinutes); ReferenceField = None }
        let action = { Type = Validation; Message = $"Minimum {minMinutes} minutes lead time required"; Severity = Critical; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Lead time" "" (Some cond) [| action |]
          ValidationType = "lead-time"; ErrorMessage = $"Minimum {minMinutes} minutes lead time required" }

    let withinBusinessHoursRule (id: string) (startField: string) (endField: string) (openField: string) (closeField: string) : ValidationRule =
        // Fail when start < open OR end > close
        let startTooEarly = ComplexCondition.Simple { Field = startField; Operator = LessThan; Value = None; ReferenceField = Some openField }
        let endTooLate = ComplexCondition.Simple { Field = endField; Operator = GreaterThan; Value = None; ReferenceField = Some closeField }
        let cond = ComplexCondition.Composite (Or, [ startTooEarly; endTooLate ])
        let action = { Type = Validation; Message = "Outside of business hours"; Severity = Critical; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Business hours" "" (Some cond) [| action |]
          ValidationType = "business-hours"; ErrorMessage = "Outside of business hours" }

    // ---- Shared pricing/discount logic ----

    /// Pricing rule with a simple multiplier applied when a boolean or equality condition is met.
    let pricingMultiplierRule (id: string) (conditionField: string) (expected: RuleValue) (multiplier: decimal) : PricingRule =
        let cond = ComplexCondition.Simple { Field = conditionField; Operator = Equal; Value = Some expected; ReferenceField = None }
        let action = { Type = Pricing; Message = $"apply-multiplier:{multiplier}"; Severity = Info; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Pricing multiplier" "" (Some cond) [| action |]
          PricingType = "multiplier"; Formula = string multiplier }

    /// Compose a discount rule for known discount codes or customer segments.
    let discountCodeRule (id: string) (codeField: string) (code: string) (percentOff: int) : PricingRule =
        let cond = ComplexCondition.Simple { Field = codeField; Operator = Equal; Value = Some (SString code); ReferenceField = None }
        let action = { Type = Pricing; Message = (sprintf "apply-discount:%i%%" percentOff); Severity = Info; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Discount code" "" (Some cond) [| action |]
          PricingType = "discount"; Formula = (sprintf "-%i%%" percentOff) }

    /// A shared set of typical pricing rules used by many domains (VIP, peak hours, seasonal).
    let commonPricingRules () : PricingRule[] =
        [|
            pricingMultiplierRule "PRICE-VIP" "isVIP" (SBool true) 1.5m
            pricingMultiplierRule "PRICE-PEAK" "isPeakHour" (SBool true) 1.3m
            pricingMultiplierRule "PRICE-SUMMER" "season" (SString "summer") 1.2m
            pricingMultiplierRule "PRICE-WINTER" "season" (SString "winter") 0.8m
        |]

    // ---- Convenience: create rule set and execute (migrated harness) ----

    let createRuleSet (validation: ValidationRule[]) (businessOpt: BusinessRule[] option) (pricingOpt: PricingRule[] option) : RuleSet =
        {
            ValidationRules = validation
            BusinessRules = defaultArg businessOpt [||]
            ConflictRules = [||]
            PricingRules = defaultArg pricingOpt [||]
            ApprovalRules = [||]
            Metadata = Map.empty
        }

    let private createContext () : RuleExecutionContext =
        {
            TraceEnabled = false
            UserId = None
            SessionId = None
            AdditionalData = Map.empty
            CaseInsensitive = true
            TargetTimeZoneId = None
            CollectTrace = false
        }

    /// Optional debug toggle via env var DOMAINS_DEBUG=1
    let private isDebug () =
        match System.Environment.GetEnvironmentVariable("DOMAINS_DEBUG") with
        | null | "" -> false
        | v when v = "1" || v.Equals("true", StringComparison.OrdinalIgnoreCase) -> true
        | _ -> false

    let execute (ruleSet: RuleSet) (reservationJson: string) (resourceJsonOpt: string option) : RuleEngine.RuleSetEvaluationResult =
        let merged = mergeReservationAndResources reservationJson resourceJsonOpt
        if isDebug() then System.Console.WriteLine($"DOMAINS_EXEC input: {merged}")
        use doc = JsonDocument.Parse(merged)
        let ctx = createContext()
        interpretRules ruleSet doc ctx

    // ---- DSL execution path: accept full Request JSON (with Domain, Rules, Data) ----
    module Dsl =
        open ReservationService.Core.Dsl
        open ReservationService.Core.JsonSerialization
        open ReservationService.Core.Dsl.Types

        /// Execute a full DSL Request JSON string through the same engine
        let executeRequestJson (requestJson: string) : RuleEngine.RuleSetEvaluationResult =
            if isDebug() then System.Console.WriteLine($"DOMAINS_EXEC_DSL input: {requestJson}")
            let (rules, jsonData, ctx) = ReservationService.Core.Dsl.Parser.compileRequest requestJson
            RuleEngine.interpretRules rules jsonData ctx

        // ---- Helpers to build common rules as DSL RuleDef ----
        open ReservationService.Core.Dsl.Types
        open ReservationService.Core.Dsl

        let private mkAction (code:string) (severity:string) : ActionDef =
            { Type = "Validation"; Severity = Some severity; Code = code; Params = None; PropertyUpdates = None }

        let capacityRuleDsl (id:string) (participantsField:string) (capacityField:string) : RuleDef =
            let cond = Cond.Simple { Field = participantsField; Operator = ">"; Value = None; ReferenceField = Some capacityField }
            { Id = id; Name = "Capacity"; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond; Actions=[| mkAction "CAPACITY" "Critical" |]; Kind=Some "Validation"; Scope=Some "reservation" }

        let leadTimeRuleDsl (id:string) (leadMinutesField:string) (minMinutes:int) : RuleDef =
            let cond = Cond.Simple { Field = leadMinutesField; Operator = "<"; Value = Some (DslValue.VInt minMinutes); ReferenceField = None }
            { Id = id; Name = "Lead time"; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond; Actions=[| mkAction "LEAD" "Critical" |]; Kind=Some "Validation"; Scope=Some "reservation" }

        let minParticipantsRuleDsl (id:string) (minCount:int) : RuleDef =
            let cond = Cond.Simple { Field = "participants"; Operator = "<"; Value = Some (DslValue.VInt minCount); ReferenceField = None }
            { Id = id; Name = "Min participants"; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond; Actions=[| mkAction "MIN_PARTICIPANTS" "Critical" |]; Kind=Some "Validation"; Scope=Some "reservation" }

        let businessHoursRuleDsl (id:string) (startField:string) (endField:string) (openField:string) (closeField:string) : RuleDef =
            let left = Cond.Simple { Field = startField; Operator = "<"; Value=None; ReferenceField = Some openField }
            let right = Cond.Simple { Field = endField; Operator = ">"; Value=None; ReferenceField = Some closeField }
            let cond = Cond.Composite { Op = "Or"; Children = [| left; right |] }
            { Id = id; Name = "Business hours"; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond; Actions=[| mkAction "BUSINESS_HOURS" "Critical" |]; Kind=Some "Validation"; Scope=Some "reservation" }

        let noOverlapRuleDsl (id:string) (startField:string) (endField:string) : RuleDef =
            let cond = Cond.Simple { Field = startField; Operator = ">="; Value=None; ReferenceField = Some endField }
            { Id = id; Name = "No overlap"; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond; Actions=[| mkAction "NO_OVERLAP" "Critical" |]; Kind=Some "Validation"; Scope=Some "reservation" }

        let closedDaysRuleDsl (id:string) (startDateField:string) (closedDatesField:string) : RuleDef =
            let cond = Cond.Simple { Field = startDateField; Operator = "In"; Value=None; ReferenceField = Some closedDatesField }
            { Id = id; Name = "Closed days"; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond; Actions=[| mkAction "CLOSED_DAY" "Critical" |]; Kind=Some "Validation"; Scope=Some "reservation" }

        /// Build a generic validation RuleDef from a DSL condition
        let validationRuleDsl (id:string) (name:string) (code:string) (severity:string) (condition:Cond) : RuleDef =
            { Id = id; Name = name; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=condition; Actions=[| mkAction code severity |]; Kind=Some "Validation"; Scope=Some "reservation" }

        // ---- Build Request JSON from rules + data fragments ----
        let private defaultDomainDef () : DomainDef =
            let fields : DomainFieldDef array =
                [|
                    { Name = "participants"; Type = "Int"; Required=None; Unit=None }
                    { Name = "leadMinutes"; Type = "Int"; Required=None; Unit=None }
                    { Name = "timeRange"; Type = "Object"; Required=None; Unit=None }
                    { Name = "resourceAvailability"; Type = "Object"; Required=None; Unit=None }
                |]
            { Types = Some [| { Name = "Reservation"; Fields = fields } |]; Enums=None; Derived=None; Aliases=None }

        let private buildDataObject (reservationJson:string) (resourceJsonOpt:string option) : System.Text.Json.Nodes.JsonObject =
            let merged = mergeReservationAndResources reservationJson resourceJsonOpt
            match System.Text.Json.Nodes.JsonNode.Parse(merged) with
            | :? System.Text.Json.Nodes.JsonObject as obj -> obj
            | _ -> let o = System.Text.Json.Nodes.JsonObject() in o

        let createRequestJson (rules: RuleDef array) (reservationJson:string) (resourceJsonOpt:string option) : string =
            let dataObj = buildDataObject reservationJson resourceJsonOpt
            let req : Request = { SchemaVersion = "1.0"; Settings=None; Domain=Some (defaultDomainDef()); Data = dataObj; Functions=None; Rules = rules; Messages=None }
            ReservationService.Core.JsonSerialization.JsonSerialization.serializeToJson req

        let executeRulesWithData (rules: RuleDef array) (reservationJson:string) (resourceJsonOpt:string option) : RuleEngine.RuleSetEvaluationResult =
            let reqJson = createRequestJson rules reservationJson resourceJsonOpt
            executeRequestJson reqJson

