namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine

/// Common rule tests shared across domains (restaurant, car rental-like payloads, etc.)
/// These are expressed using RuleTypes and executed by the RuleEngine to validate behavior.
module CommonRulesTests =

    open ScenarioHarness

    // --- Builders (lightweight, JSON-based) ---

    type ReservationJsonBuilder() =
        let fields = Dictionary<string, obj>()
        do
            fields["participants"] <- box 4
            fields["leadMinutes"] <- box 120
            let timeRange = Dictionary<string, obj>()
            timeRange["start"] <- box (DateTimeOffset(2025,8,21,19,0,0,TimeSpan.Zero).ToString("o"))
            timeRange["end"] <- box (DateTimeOffset(2025,8,21,20,0,0,TimeSpan.Zero).ToString("o"))
            timeRange["timeZone"] <- box "UTC"
            fields["timeRange"] <- box timeRange

        member this.With (k:string, v: obj) = fields[k] <- v; this
        member this.WithParticipants n = fields["participants"] <- box n; this
        member this.WithLeadMinutes n = fields["leadMinutes"] <- box n; this
        member this.AsVIP() = fields["isVIP"] <- box true; this
        member this.AsSpecialEvent(authorized: bool) = fields["isSpecialEvent"] <- box true; fields["hasAuthorization"] <- box authorized; this
        member this.Build() = JsonSerializer.Serialize(fields)

    type ResourcesJsonBuilder() =
        let res = Dictionary<string, obj>()
        member this.WithTable (name:string, capacity:int) = res[name] <- box {| capacity = capacity |}; this
        member this.WithTotalCapacity total = res["totalCapacity"] <- box total; this
        member this.Build() =
            let outer = Dictionary<string, obj>()
            outer["resourceAvailability"] <- box res
            JsonSerializer.Serialize(outer)

    // --- Common rules ---

    let capacityRule id participantsField capField : ValidationRule =
        let cond = ComplexCondition.Simple {
            Field = participantsField
            Operator = GreaterThan
            Value = None
            Values = None
            ReferenceField = Some capField
        }
        let action = {
            ActionType = Reject
            Severity = Critical
            Message = Some "Capacity exceeded"
            Parameters = Map.empty
            PropertyUpdates = None
            Suggestions = None
        }
        {
            Rule = {
                Id = id; Name = "Capacity"; Description = None; Priority = 100; Enabled = true; Tags = [||];
                Condition = Some cond; Actions = [| action |]
            }
            ValidationScope = "participants"
            IsRequired = true
            Dependencies = [||]
        }

    let minLeadRule id leadField minMinutes : ValidationRule =
        let cond = ComplexCondition.Simple { Field = leadField; Operator = LessThan; Value = Some (SInt minMinutes); Values = None; ReferenceField = None }
        let action = { ActionType = Reject; Severity = Critical; Message = Some ($"Minimum {minMinutes} minutes lead time required"); Parameters = Map.empty; PropertyUpdates = None; Suggestions = None }
        { Rule = { Id = id; Name = "Lead time"; Description=None; Priority=100; Enabled=true; Tags=[||]; Condition=Some cond; Actions=[|action|] }
          ValidationScope = "timeRange"; IsRequired = true; Dependencies = [||] }

    let requiresAuthWhenSpecialEvent id : ValidationRule =
        let condition = ComplexCondition.Composite (And, [
            ComplexCondition.Simple { Field = "isSpecialEvent"; Operator = Equals; Value = Some (SBool true); Values = None; ReferenceField = None }
            ComplexCondition.Simple { Field = "hasAuthorization"; Operator = Equals; Value = Some (SBool false); Values = None; ReferenceField = None }
        ])
        let action = { ActionType = Reject; Severity = Critical; Message = Some "Special events require authorization"; Parameters = Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id = id; Name = "Authorization required"; Description=None; Priority=90; Enabled=true; Tags=[||]; Condition=Some condition; Actions=[|action|] }
          ValidationScope = "reservation"; IsRequired = true; Dependencies=[||] }

    // --- Tests ---

    [<Fact>]
    let ``Capacity: participants must not exceed capacity`` () =
        let ruleSet =
            createRuleSet [| capacityRule "CAP-001" "participants" "resourceAvailability.table-1.capacity" |] None None
        let reservation = ReservationJsonBuilder().WithParticipants(5).Build()
        let resources = ResourcesJsonBuilder().WithTable("table-1", 4).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Lead time: must meet minimum minutes`` () =
        let ruleSet =
            createRuleSet [| minLeadRule "LEAD-001" "leadMinutes" 60 |] None None
        let reservation = ReservationJsonBuilder().WithLeadMinutes(30).Build()
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(true, false, true)>]
    [<InlineData(true, true, false)>]
    [<InlineData(false, true, false)>]
    let ``Special events require authorization unless not special`` (isSpecial: bool, hasAuth: bool, shouldFail: bool) =
        let ruleSet = createRuleSet [| requiresAuthWhenSpecialEvent "AUTH-001" |] None None
        let reservation =
            let b = ReservationJsonBuilder()
            let b = if isSpecial then b.AsSpecialEvent(hasAuth) else b
            b.Build()
        let result = execute ruleSet reservation None
        Assert.Equal(shouldFail, result.HasCriticalErrors)

