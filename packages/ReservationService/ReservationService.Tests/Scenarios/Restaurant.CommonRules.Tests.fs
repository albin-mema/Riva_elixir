namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine

module Restaurant_CommonRules =

    open ScenarioHarness

    // Reuse common rule patterns by calling the shared functions directly

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

    type TablesResourcesJsonBuilder() =
        let res = Dictionary<string, obj>()
        member this.WithTable (name:string, capacity:int) = res[name] <- box {| capacity = capacity |}; this
        member this.Build() =
            let outer = Dictionary<string, obj>()
            outer["resourceAvailability"] <- box res
            JsonSerializer.Serialize(outer)

    [<Fact>]
    let ``Restaurant: capacity cannot be exceeded`` () =
        let ruleSet = createRuleSet [| CommonRulesTests.capacityRule "REST-CAP" "participants" "resourceAvailability.table-2.capacity" |] None None
        let reservation = RestaurantReservationJsonBuilder().WithParticipants(5).Build()
        let resources = TablesResourcesJsonBuilder().WithTable("table-2", 4).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Restaurant: must meet minimum lead time`` () =
        let ruleSet = createRuleSet [| CommonRulesTests.minLeadRule "REST-LEAD" "leadMinutes" 60 |] None None
        let reservation = RestaurantReservationJsonBuilder().WithLeadMinutes(30).Build()
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(true, false, true)>]
    [<InlineData(true, true, false)>]
    [<InlineData(false, true, false)>]
    let ``Restaurant: special event requires authorization`` (isSpecial: bool, hasAuth: bool, shouldFail: bool) =
        let ruleSet = createRuleSet [| CommonRulesTests.requiresAuthWhenSpecialEvent "REST-AUTH" |] None None
        let reservation =
            let b = RestaurantReservationJsonBuilder()
            let b = if isSpecial then b.With("isSpecialEvent", box true).With("hasAuthorization", box hasAuth) else b
            b.Build()
        let result = execute ruleSet reservation None
        Assert.Equal(shouldFail, result.HasCriticalErrors)

