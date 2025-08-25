namespace ReservationService.Tests.Domains.Salon

open System
open System.Text.Json
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Common

module SalonCommonRules =

    [<Fact>]
    let ``Salon: stylist capacity cannot be exceeded`` () =
        let rs = createRuleSet [| capacityRule "SALON-CAP" "participants" "resourceAvailability.stylist-1.capacity" |] None None
        let resv = JsonDocument.Parse("{ \"participants\": 3 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"stylist-1\": { \"capacity\": 2 } } }"
        let result = execute rs resv resources
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Salon: must meet minimum lead time; equal passes`` () =
        let rs = createRuleSet [| leadTimeRule "SALON-LEAD" "leadMinutes" 60 |] None None
        let resv = JsonDocument.Parse("{ \"leadMinutes\": 60 }").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Salon: working hours boundary inclusive (equal open/close passes)`` () =
        let rs = createRuleSet [| withinBusinessHoursRule "SALON-HOURS" "startHour" "endHour" "resourceAvailability.shop.open" "resourceAvailability.shop.close" |] None None
        let resv = JsonDocument.Parse("{ \"startHour\": 9, \"endHour\": 18 }").RootElement.GetRawText()
        let resources = Some "{ \"resourceAvailability\": { \"shop\": { \"open\": 9, \"close\": 18 } } }"
        let result = execute rs resv resources
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Salon: no-overlap (start < end)`` () =
        let rs = createRuleSet [| ClosedDaysAndNoOverlap.noOverlapRule "SALON-NOOVER" |] None None
        let start = DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)
        let finish = start.AddMinutes(30.0)
        let resv = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{start:o}\", \"end\": \"{finish:o}\" }} }}").RootElement.GetRawText()
        let result = execute rs resv None
        Assert.False(result.HasCriticalErrors)

