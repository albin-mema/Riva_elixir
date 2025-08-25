namespace ReservationService.Tests.Domains.Parking

open System
open System.Text.Json
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Common

module ParkingCommonRules =

    [<Fact>]
    let ``Parking: lot capacity cannot be exceeded`` () =
        let rs = createRuleSet [| capacityRule "PARK-CAP" "participants" "resourceAvailability.lot.capacity" |] None None
        let resv = JsonDocument.Parse("{ \"participants\": 120 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"lot\": { \"capacity\": 100 } } }"
        let result = execute rs resv resources
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Parking: small lead time; equal passes`` () =
        let rs = createRuleSet [| leadTimeRule "PARK-LEAD" "leadMinutes" 10 |] None None
        let resv = JsonDocument.Parse("{ \"leadMinutes\": 10 }").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Parking: working hours boundary inclusive`` () =
        let rs = createRuleSet [| withinBusinessHoursRule "PARK-HOURS" "startHour" "endHour" "resourceAvailability.lot.open" "resourceAvailability.lot.close" |] None None
        let resv = JsonDocument.Parse("{ \"startHour\": 7, \"endHour\": 22 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"lot\": { \"open\": 7, \"close\": 22 } } }"
        let result = execute rs resv resources
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Parking: closed day blocks reservation`` () =
        let rs = createRuleSet [| ClosedDaysAndNoOverlap.closedDaysRule "PARK-CLOSED" |] None None
        let rtb = ClosedDaysAndNoOverlap.ReservationTimeBuilder()
        let reservation = rtb.WithStart(DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)).Build()
        let resources = ClosedDaysAndNoOverlap.ResourceCalendarBuilder().WithClosedDates([ DateTimeOffset(2025,8,21,0,0,0,TimeSpan.Zero) ]).Build()
        let result = execute rs reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Parking: no-overlap (start < end)`` () =
        let rs = createRuleSet [| ClosedDaysAndNoOverlap.noOverlapRule "PARK-NOOVER" |] None None
        let start = DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)
        let finish = start.AddHours(2.0)
        let resv = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{start:o}\", \"end\": \"{finish:o}\" }} }}").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.False(result.HasCriticalErrors)

