namespace ReservationService.Tests.Domains.Taxi

open System
open System.Text.Json
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Common

module TaxiCommonRules =

    [<Fact>]
    let ``Taxi: passenger count cannot exceed vehicle capacity`` () =
        let rs = createRuleSet [| capacityRule "TAXI-CAP" "participants" "resourceAvailability.vehicle.capacity" |] None None
        let resv = JsonDocument.Parse("{ \"participants\": 5 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"vehicle\": { \"capacity\": 4 } } }"
        let result = execute rs resv resources
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Taxi: booking must meet minimum lead time`` () =
        let rs = createRuleSet [| leadTimeRule "TAXI-LEAD" "leadMinutes" 15 |] None None
        let resv = JsonDocument.Parse("{ \"leadMinutes\": 5 }").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Taxi: start < end (no overlap)`` () =
        let rs = createRuleSet [| ClosedDaysAndNoOverlap.noOverlapRule "TAXI-NOOVER" |] None None
        let start = DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)
        let finish = start.AddMinutes(30.0)
        let resv = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{start:o}\", \"end\": \"{finish:o}\" }} }}").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.False(result.HasCriticalErrors)

