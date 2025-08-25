namespace ReservationService.Tests.Domains.Hotel

open FsCheck
open FsCheck.Xunit
open System.Text.Json
open ReservationService.Core.RuleTypes
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Shared.Dsl
open ReservationService.Core.Dsl.Types

module HotelPropertyTests =

    [<Property(MaxTest = 50)>]
    let ``Capacity monotonicity`` (cap: NonNegativeInt, pDelta: NonNegativeInt) =
        let cap' = max 1 cap.Get
        let delta = pDelta.Get % 5
        let rules = [| Dsl.capacityRuleDsl "HOTEL-CAP-PROP" "participants" "resourceAvailability.room.capacity" |]
        let mk p =
            let r = JsonDocument.Parse($"{{ \"participants\": {p} }}").RootElement.GetRawText()
            let res = Some ($"{{ \"resourceAvailability\": {{ \"room\": {{ \"capacity\": {cap'} }} }} }}")
            Dsl.executeRulesWithData rules r res
        let r1 = mk (cap' - 1)
        let r2 = mk cap'
        let r3 = mk (cap' + delta + 1)
        (r1.HasCriticalErrors = false) && (r2.HasCriticalErrors = false) && (r3.HasCriticalErrors = true)

    [<Property(MaxTest = 50)>]
    let ``Lead time threshold monotonicity`` (lead: NonNegativeInt) =
        let leadVal = lead.Get % 480
        let run min =
            let rules = [| Dsl.leadTimeRuleDsl "HOTEL-LEAD-PROP" "leadMinutes" min |]
            let r = JsonDocument.Parse($"{{ \"leadMinutes\": {leadVal} }}").RootElement.GetRawText()
            Dsl.executeRulesWithData rules r None
        let low = run 60
        let high = run 240
        ((low.HasCriticalErrors && high.HasCriticalErrors) || ((not high.HasCriticalErrors) && (not low.HasCriticalErrors)))

