namespace ReservationService.Tests.Domains.Common

open System
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Core.RuleDomain
open ReservationService.Tests.Domains.Shared

module CommonRules =

    [<Fact>]
    let ``Capacity: participants must not exceed capacity`` () =
        let ruleSet = createRuleSet [| capacityRule "CAP-001" "participants" "maxCapacity" |] None None
        let reservation = "{\"participants\": 5, \"maxCapacity\": 4}"
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Lead time: must meet minimum minutes`` () =
        let ruleSet = createRuleSet [| leadTimeRule "LEAD-001" "leadMinutes" 60 |] None None
        let reservation = "{\"leadMinutes\": 30}"
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(true, false, true)>]
    [<InlineData(true, true, false)>]
    [<InlineData(false, true, false)>]
    let ``Special events require authorization unless not special`` (isSpecial: bool, hasAuth: bool, shouldFail: bool) =
        let cond = ComplexCondition.Composite (And, [
            ComplexCondition.Simple { Field = "isSpecialEvent"; Operator = Equal; Value = Some (SBool true); ReferenceField = None }
            ComplexCondition.Simple { Field = "hasAuthorization"; Operator = Equal; Value = Some (SBool false); ReferenceField = None }
        ])
        let action = { Type = Validation; Message = "Special events require authorization"; Severity = Critical; Parameters = Map.empty }
        let rule : ValidationRule = { Rule = RuleHelpers.createRule "AUTH-001" "Authorization required" "" (Some cond) [|action|]; ValidationType = "auth"; ErrorMessage = "Special events require authorization" }
        let ruleSet = createRuleSet [| rule |] None None
        let reservation =
            if isSpecial then
                $"{{\"isSpecialEvent\": true, \"hasAuthorization\": {hasAuth.ToString().ToLower()} }}"
            else
                "{\"isSpecialEvent\": false}"
        let result = execute ruleSet reservation None
        Assert.Equal(shouldFail, result.HasCriticalErrors)

