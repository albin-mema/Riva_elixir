namespace ReservationService.Tests.Domains.Hotel

open System
open System.Text.Json
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Shared.Dsl
open ReservationService.Tests.Domains.Common
open ReservationService.Core.Dsl.Types

module HotelCommonRules =

    // Example tests focus on generalized rules only

    [<Fact>]
    let ``Hotel: room capacity cannot be exceeded`` () =
        let rules = [| capacityRuleDsl "HOTEL-CAP" "participants" "resourceAvailability.room.capacity" |]
        let reservation = JsonDocument.Parse("{ \"participants\": 3 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"room\": { \"capacity\": 2 } } }"
        let result = Dsl.executeRulesWithData rules reservation resources
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Hotel: booking must meet minimum lead time`` () =
        let rules = [| leadTimeRuleDsl "HOTEL-LEAD" "leadMinutes" 120 |]
        let reservation = JsonDocument.Parse("{ \"leadMinutes\": 60 }").RootElement.GetRawText()
        let result = Dsl.executeRulesWithData rules reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Hotel: booking within business hours passes`` () =
        let rules = [| businessHoursRuleDsl "HOTEL-HOURS" "startHour" "endHour" "resourceAvailability.frontdesk.open" "resourceAvailability.frontdesk.close" |]
        let reservation = JsonDocument.Parse("{ \"startHour\": 10, \"endHour\": 18 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"frontdesk\": { \"open\": 8, \"close\": 20 } } }"
        let result = Dsl.executeRulesWithData rules reservation resources
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Hotel: no-overlap must hold (start < end)`` () =
        let rules = [| noOverlapRuleDsl "HOTEL-NOOVER" "timeRange.start" "timeRange.end" |]
        let start = DateTimeOffset(2025,9,1,10,0,0,TimeSpan.Zero)
        let finish = start.AddHours(2.0)
        let reservation = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{start:o}\", \"end\": \"{finish:o}\" }} }}").RootElement.GetRawText()
        let result = Dsl.executeRulesWithData rules reservation None
        Assert.False(result.HasCriticalErrors)

