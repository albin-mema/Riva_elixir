namespace ReservationService.Tests.Domains.Museum

open System
open System.Text.Json
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Common

module MuseumCommonRules =

    [<Fact>]
    let ``Museum: timed entry capacity cannot be exceeded`` () =
        let rs = createRuleSet [| capacityRule "MUSEUM-CAP" "participants" "resourceAvailability.entry-10am.capacity" |] None None
        let resv = JsonDocument.Parse("{ \"participants\": 31 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"entry-10am\": { \"capacity\": 30 } } }"
        let result = execute rs resv resources
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Museum: 24h lead time; equal passes`` () =
        let rs = createRuleSet [| leadTimeRule "MUS-LEAD" "leadMinutes" (24*60) |] None None
        let resv = JsonDocument.Parse($"{{ \"leadMinutes\": %d{24*60} }}").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Museum: working hours boundary inclusive`` () =
        let rs = createRuleSet [| withinBusinessHoursRule "MUS-HOURS" "startHour" "endHour" "resourceAvailability.exhibit.open" "resourceAvailability.exhibit.close" |] None None
        let resv = JsonDocument.Parse("{ \"startHour\": 10, \"endHour\": 17 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"exhibit\": { \"open\": 10, \"close\": 17 } } }"
        let result = execute rs resv resources
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Museum: closed day blocks reservation`` () =
        let rs = createRuleSet [| ClosedDaysAndNoOverlap.closedDaysRule "MUS-CLOSED" |] None None
        let rtb = ClosedDaysAndNoOverlap.ReservationTimeBuilder()
        let reservation = rtb.WithStart(DateTimeOffset(2025,9,1,11,0,0,TimeSpan.Zero)).Build()
        let resources = ClosedDaysAndNoOverlap.ResourceCalendarBuilder().WithClosedDates([ DateTimeOffset(2025,9,1,0,0,0,TimeSpan.Zero) ]).Build()
        let result = execute rs reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Museum: no-overlap (start < end)`` () =
        let rs = createRuleSet [| ClosedDaysAndNoOverlap.noOverlapRule "MUS-NOOVER" |] None None
        let start = DateTimeOffset(2025,9,2,10,0,0,TimeSpan.Zero)
        let finish = start.AddHours(1.0)
        let resv = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{start:o}\", \"end\": \"{finish:o}\" }} }}").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.False(result.HasCriticalErrors)

