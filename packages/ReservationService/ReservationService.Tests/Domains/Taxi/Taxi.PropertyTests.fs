namespace ReservationService.Tests.Domains.Taxi

open FsCheck
open FsCheck.Xunit
open System.Text.Json
open ReservationService.Core.RuleTypes
open ReservationService.Tests.Domains.Shared

module TaxiPropertyTests =

    [<Property(MaxTest = 50)>]
    let ``Capacity monotonicity`` (cap: NonNegativeInt, d: NonNegativeInt) =
        let cap' = max 1 cap.Get
        let delta = d.Get % 3
        let rs = createRuleSet [| capacityRule "TAXI-CAP-PROP" "participants" "resourceAvailability.vehicle.capacity" |] None None
        let mk p =
            let r = JsonDocument.Parse($"{{ \"participants\": {p} }}").RootElement.GetRawText()
            let res = Some ($"{{ \"resourceAvailability\": {{ \"vehicle\": {{ \"capacity\": {cap'} }} }} }}")
            execute rs r res
        let r1 = mk (cap' - 1)
        let r2 = mk cap'
        let r3 = mk (cap' + delta + 1)
        (r1.HasCriticalErrors = false) && (r2.HasCriticalErrors = false) && (r3.HasCriticalErrors = true)

    [<Property(MaxTest = 50)>]
    let ``Lead time threshold monotonicity`` (lead: NonNegativeInt) =
        let leadVal = lead.Get % 60
        let run min =
            let rs = createRuleSet [| leadTimeRule "TAXI-LEAD-PROP" "leadMinutes" min |] None None
            let r = JsonDocument.Parse($"{{ \"leadMinutes\": {leadVal} }}").RootElement.GetRawText()
            execute rs r None
        let low = run 5
        let high = run 20
        let lowFail, highFail = low.HasCriticalErrors, high.HasCriticalErrors
        // Monotonicity w.r.t increasing required minimum: fail set must not shrink
        // 1) If low fails, high must fail
        // 2) If high passes, low must pass
        ((not lowFail) || highFail) && ((not (not highFail)) || (not lowFail))

