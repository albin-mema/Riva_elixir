namespace ReservationService.Tests.Domains.Restaurant

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleDomain
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Shared.Dsl
open ReservationService.Core.Dsl.Types

module CommonRules =

    type RestaurantReservationJsonBuilder() =
        let fields = Dictionary<string, obj>()
        do
            fields["participants"] <- box 4
            fields["leadMinutes"] <- box 120
            let timeRange = Dictionary<string, obj>()
            timeRange["start"] <- box (DateTimeOffset.UtcNow.AddHours(2.0).ToString("o"))
            timeRange["end"] <- box (DateTimeOffset.UtcNow.AddHours(3.0).ToString("o"))
            timeRange["timeZone"] <- box "UTC"
            fields["timeRange"] <- box timeRange
        member this.With (k:string, v: obj) = fields[k] <- v; this
        member this.WithParticipants n = fields["participants"] <- box n; this
        member this.WithLeadMinutes n = fields["leadMinutes"] <- box n; this
        member this.Build() = JsonSerializer.Serialize(fields)

    [<Fact>]
    let ``Restaurant: capacity cannot be exceeded`` () =
        let rules = [| validationRuleDsl "REST-CAP" "Capacity" "CAPACITY" "Critical" (Cond.Simple { Field = "participants"; Operator = ">"; Value=None; ReferenceField=Some "maxCapacity" }) |]
        let reservation = RestaurantReservationJsonBuilder().WithParticipants(5).With("maxCapacity", box 4).Build()
        let result = Dsl.executeRulesWithData rules reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Restaurant: must meet minimum lead time`` () =
        let rules = [| validationRuleDsl "REST-LEAD" "Lead time" "LEAD" "Critical" (Cond.Simple { Field = "leadMinutes"; Operator = "<"; Value=Some (DslValue.VInt 60); ReferenceField=None }) |]
        let reservation = RestaurantReservationJsonBuilder().WithLeadMinutes(30).Build()
        let result = Dsl.executeRulesWithData rules reservation None
        Assert.True(result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(true, false, true)>]
    [<InlineData(true, true, false)>]
    [<InlineData(false, true, false)>]
    let ``Restaurant: special event requires authorization`` (isSpecial: bool, hasAuth: bool, shouldFail: bool) =
        let reservation =
            let b = RestaurantReservationJsonBuilder()
            let b = if isSpecial then b.With("isSpecialEvent", box true).With("hasAuthorization", box hasAuth) else b
            b.Build()
        // Build the auth rule inline and run
        let cond = ComplexCondition.Composite (And, [
            ComplexCondition.Simple { Field = "isSpecialEvent"; Operator = Equal; Value = Some (SBool true); ReferenceField = None }
            ComplexCondition.Simple { Field = "hasAuthorization"; Operator = Equal; Value = Some (SBool false); ReferenceField = None }
        ])
        let action = { Type = ReservationService.Core.RuleDomain.RuleActionType.Validation; Message = "Special events require authorization"; Severity = ReservationService.Core.RuleDomain.RuleSeverity.Critical; Parameters = Map.empty }
        let authRule : ValidationRule = { Rule = RuleHelpers.createRule "REST-AUTH" "Authorization required" "" (Some cond) [|action|]; ValidationType = "auth"; ErrorMessage = "Special events require authorization" }
        let result = execute (createRuleSet [| authRule |] None None) reservation None
        Assert.Equal(shouldFail, result.HasCriticalErrors)

