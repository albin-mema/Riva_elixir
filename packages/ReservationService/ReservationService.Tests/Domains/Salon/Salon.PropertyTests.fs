namespace ReservationService.Tests.Domains.Salon

open FsCheck
open FsCheck.Xunit
open System.Text.Json
open ReservationService.Core.RuleTypes
open ReservationService.Tests.Domains.Shared

module SalonPropertyTests =

    [<Property(MaxTest = 50)>]
    let ``Capacity monotonicity`` (cap: NonNegativeInt, d: NonNegativeInt) =
        let cap' = max 1 cap.Get
        let delta = d.Get % 5
        let rs = createRuleSet [| capacityRule "SALON-CAP-PROP" "participants" "resourceAvailability.stylist-1.capacity" |] None None
        let mk p =
            let r = JsonDocument.Parse($"{{ \"participants\": {p} }}").RootElement.GetRawText()
            let res = Some ($"{{ \"resourceAvailability\": {{ \"stylist-1\": {{ \"capacity\": {cap'} }} }} }}")
            execute rs r res
        let r1 = mk (cap' - 1)
        let r2 = mk cap'
        let r3 = mk (cap' + delta + 1)
        (r1.HasCriticalErrors = false) && (r2.HasCriticalErrors = false) && (r3.HasCriticalErrors = true)

    [<Property(MaxTest = 50)>]
    let ``Lead time: equality passes and monotonic failset`` (lead: NonNegativeInt) =
        let leadVal = lead.Get % 240
        let run min =
            let rs = createRuleSet [| leadTimeRule "SALON-LEAD-PROP" "leadMinutes" min |] None None
            let r = JsonDocument.Parse($"{{ \"leadMinutes\": {leadVal} }}").RootElement.GetRawText()
            execute rs r None
        let low = run 30
        let eq = run 60
        let high = run 120
        // Equality should pass only when the input equals the threshold
        let lowFail, eqFail, highFail = low.HasCriticalErrors, eq.HasCriticalErrors, high.HasCriticalErrors
        let equalityCondition = if leadVal = 60 then (not eqFail) else true
        // Monotonicity across thresholds: if low fails then high must fail; if high passes, low must pass
        equalityCondition && ((not lowFail) || highFail) && (highFail || (not lowFail))

