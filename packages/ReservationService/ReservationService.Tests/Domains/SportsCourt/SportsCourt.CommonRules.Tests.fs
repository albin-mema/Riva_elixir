namespace ReservationService.Tests.Domains.SportsCourt

open System
open System.Text.Json
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Common

module SportsCourtCommonRules =

    [<Fact>]
    let ``SportsCourt: slot cannot exceed capacity`` () =
        let rs = createRuleSet [| capacityRule "SC-CAP" "participants" "resourceAvailability.court.capacity" |] None None
        let resv = JsonDocument.Parse("{ \"participants\": 6 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"court\": { \"capacity\": 4 } } }"
        let result = execute rs resv resources
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``SportsCourt: must meet lead time`` () =
        let rs = createRuleSet [| leadTimeRule "SC-LEAD" "leadMinutes" 120 |] None None
        let resv = JsonDocument.Parse("{ \"leadMinutes\": 30 }").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``SportsCourt: working hours honored`` () =
        let rs = createRuleSet [| withinBusinessHoursRule "SC-HOURS" "startHour" "endHour" "resourceAvailability.court.open" "resourceAvailability.court.close" |] None None
        let resv = JsonDocument.Parse("{ \"startHour\": 9, \"endHour\": 12 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"court\": { \"open\": 8, \"close\": 21 } } }"
        let result = execute rs resv resources
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``SportsCourt: start < end (no overlap)`` () =
        let rs = createRuleSet [| ClosedDaysAndNoOverlap.noOverlapRule "SC-NOOVER" |] None None
        let start = DateTimeOffset(2025,8,21,16,0,0,TimeSpan.Zero)
        let finish = start.AddHours(1.0)
        let resv = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{start:o}\", \"end\": \"{finish:o}\" }} }}").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.False(result.HasCriticalErrors)

