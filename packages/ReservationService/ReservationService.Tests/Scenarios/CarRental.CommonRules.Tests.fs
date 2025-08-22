namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine

module CarRental_CommonRules =

    open ScenarioHarness

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

    type VehicleResourcesJsonBuilder() =
        let res = Dictionary<string, obj>()
        member this.WithVehicleCapacity (name:string, capacity:int) = res[name] <- box {| capacity = capacity |}; this
        member this.WithFleetCapacity total = res["totalCapacity"] <- box total; this
        member this.Build() =
            let outer = Dictionary<string, obj>()
            outer["resourceAvailability"] <- box res
            JsonSerializer.Serialize(outer)

    let capacityRule id participantsField capField : ValidationRule =
        let cond = ComplexCondition.Simple { Field = participantsField; Operator = GreaterThan; Value = None; Values=None; ReferenceField = Some capField }
        let action = { ActionType = Reject; Severity = Critical; Message = Some "Capacity exceeded"; Parameters=Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id=id; Name="Capacity"; Description=None; Priority=100; Enabled=true; Tags=[||]; Condition=Some cond; Actions=[|action|] }
          ValidationScope = "participants"; IsRequired = true; Dependencies=[||] }

    let minLeadRule id leadField minMinutes : ValidationRule =
        let cond = ComplexCondition.Simple { Field = leadField; Operator = LessThan; Value = Some (SInt minMinutes); Values=None; ReferenceField=None }
        let action = { ActionType = Reject; Severity = Critical; Message = Some ($"Minimum {minMinutes} minutes lead time required"); Parameters=Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id=id; Name="Lead time"; Description=None; Priority=100; Enabled=true; Tags=[||]; Condition=Some cond; Actions=[|action|] }
          ValidationScope = "timeRange"; IsRequired = true; Dependencies=[||] }

    let requiresAuthWhenSpecialEvent id : ValidationRule =
        let cond = ComplexCondition.Composite (And, [
            ComplexCondition.Simple { Field="isSpecialEvent"; Operator=Equals; Value=Some (SBool true); Values=None; ReferenceField=None }
            ComplexCondition.Simple { Field="hasAuthorization"; Operator=Equals; Value=Some (SBool false); Values=None; ReferenceField=None }
        ])
        let action = { ActionType=Reject; Severity=Critical; Message=Some "Special events require authorization"; Parameters=Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id=id; Name="Authorization"; Description=None; Priority=90; Enabled=true; Tags=[||]; Condition=Some cond; Actions=[|action|] }
          ValidationScope = "reservation"; IsRequired = true; Dependencies=[||] }

    [<Fact>]
    let ``Car rental: capacity cannot be exceeded`` () =
        let ruleSet = createRuleSet [| capacityRule "CAP-VEH" "participants" "resourceAvailability.suv-1.capacity" |] None None
        let reservation = RentalReservationJsonBuilder().WithParticipants(5).Build()
        let resources = VehicleResourcesJsonBuilder().WithVehicleCapacity("suv-1", 4).Build()
        let result = execute ruleSet reservation (Some resources)
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

