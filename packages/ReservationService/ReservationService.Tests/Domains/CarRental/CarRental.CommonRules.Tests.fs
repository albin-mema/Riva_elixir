namespace ReservationService.Tests.Domains.CarRental

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Core.RuleDomain
open ReservationService.Tests.Domains.Shared

module CommonRules =

    // Minimal reservation + resources builder tailored to car rental-like data
    type RentalReservationJsonBuilder() =
        let fields = Dictionary<string, obj>()
        do
            fields["participants"] <- box 2 // driver + optional passenger
            fields["leadMinutes"] <- box 120
            let timeRange = Dictionary<string, obj>()
            timeRange["start"] <- box (DateTimeOffset.UtcNow.AddDays(1).ToString("o"))
            timeRange["end"] <- box (DateTimeOffset.UtcNow.AddDays(3).ToString("o"))
            timeRange["timeZone"] <- box "UTC"
            fields["timeRange"] <- box timeRange
            fields["pickupLocation"] <- box "NYC-Downtown"
            fields["dropoffLocation"] <- box "NYC-Downtown"
        member this.With (k:string, v: obj) = fields[k] <- v; this
        member this.WithParticipants n = fields["participants"] <- box n; this
        member this.WithLeadMinutes n = fields["leadMinutes"] <- box n; this
        member this.AsSpecialEvent(authorized: bool) = fields["isSpecialEvent"] <- box true; fields["hasAuthorization"] <- box authorized; this
        member this.Build() = JsonSerializer.Serialize(fields)

    // Rules (reusing Shared where possible)
    let capacityRule id field refField : ValidationRule = ReservationService.Tests.Domains.Shared.capacityRule id field refField
    let minLeadRule id field minMinutes : ValidationRule = ReservationService.Tests.Domains.Shared.leadTimeRule id field minMinutes
    let requiresAuthWhenSpecialEvent id : ValidationRule =
        let cond = ComplexCondition.Composite (And, [
            ComplexCondition.Simple { Field = "isSpecialEvent"; Operator = Equal; Value = Some (SBool true); ReferenceField = None }
            ComplexCondition.Simple { Field = "hasAuthorization"; Operator = Equal; Value = Some (SBool false); ReferenceField = None }
        ])
        let action = { Type = Validation; Message = "Special events require authorization"; Severity = Critical; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Authorization required" "" (Some cond) [| action |]
          ValidationType = "auth"; ErrorMessage = "Special events require authorization" }

    [<Fact>]
    let ``Car rental: capacity cannot be exceeded`` () =
        let ruleSet = createRuleSet [| capacityRule "CAP-VEH" "participants" "maxCapacity" |] None None
        let reservation = RentalReservationJsonBuilder().WithParticipants(5).With("maxCapacity", box 4).Build()
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Car rental: must meet minimum lead time`` () =
        let ruleSet = createRuleSet [| minLeadRule "LEAD-VEH" "leadMinutes" 60 |] None None
        let reservation = RentalReservationJsonBuilder().WithLeadMinutes(30).Build()
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(true, false, true)>]
    [<InlineData(true, true, false)>]
    [<InlineData(false, true, false)>]
    let ``Car rental: special event requires authorization`` (isSpecial: bool, hasAuth: bool, shouldFail: bool) =
        let ruleSet = createRuleSet [| requiresAuthWhenSpecialEvent "AUTH-VEH" |] None None
        let reservation =
            let b = RentalReservationJsonBuilder()
            let b = if isSpecial then b.AsSpecialEvent(hasAuth) else b
            b.Build()
        let result = execute ruleSet reservation None
        Assert.Equal(shouldFail, result.HasCriticalErrors)

