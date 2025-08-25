namespace ReservationService.Tests.Domains.SportsCourt

open FsCheck
open FsCheck.Xunit
open System.Text.Json
open ReservationService.Core.RuleTypes
open ReservationService.Tests.Domains.Shared

module SportsCourtPropertyTests =

    [<Property(MaxTest = 50)>]
    let ``Capacity monotonicity`` (cap: NonNegativeInt, d: NonNegativeInt) =
        let cap' = max 1 cap.Get
        let delta = d.Get % 5
        let rs = createRuleSet [| capacityRule "SC-CAP-PROP" "participants" "resourceAvailability.court.capacity" |] None None
        let mk p =
            let r = JsonDocument.Parse($"{{ \"participants\": {p} }}").RootElement.GetRawText()
            let res = Some ($"{{ \"resourceAvailability\": {{ \"court\": {{ \"capacity\": {cap'} }} }} }}")
            execute rs r res
        let r1 = mk (cap' - 1)
        let r2 = mk cap'
        let r3 = mk (cap' + delta + 1)
        (r1.HasCriticalErrors = false) && (r2.HasCriticalErrors = false) && (r3.HasCriticalErrors = true)

    [<Property(MaxTest = 50)>]
    let ``Lead time threshold monotonicity`` (lead: NonNegativeInt) =
        let leadVal = lead.Get % 360
        let run min =
            let rs = createRuleSet [| leadTimeRule "SC-LEAD-PROP" "leadMinutes" min |] None None
            let r = JsonDocument.Parse($"{{ \"leadMinutes\": {leadVal} }}").RootElement.GetRawText()
            execute rs r None
        let low = run 60
        let high = run 180
        ((low.HasCriticalErrors && high.HasCriticalErrors) || ((not high.HasCriticalErrors) && (not low.HasCriticalErrors)))

