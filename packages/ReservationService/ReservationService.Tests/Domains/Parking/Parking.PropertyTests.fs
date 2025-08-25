namespace ReservationService.Tests.Domains.Parking

open FsCheck
open FsCheck.Xunit
open System.Text.Json
open ReservationService.Core.RuleTypes
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Shared.Dsl
open ReservationService.Core.Dsl.Types

module ParkingPropertyTests =

    [<Property(MaxTest = 50)>]
    let ``Capacity monotonicity`` (cap: NonNegativeInt, d: NonNegativeInt) =
        let cap' = max 1 cap.Get
        let delta = d.Get % 50
        let rules = [| Dsl.capacityRuleDsl "PARK-CAP-PROP" "participants" "resourceAvailability.lot.capacity" |]
        let mk p =
            let r = JsonDocument.Parse($"{{ \"participants\": {p} }}").RootElement.GetRawText()
            let res = Some ($"{{ \"resourceAvailability\": {{ \"lot\": {{ \"capacity\": {cap'} }} }} }}")
            Dsl.executeRulesWithData rules r res
        let r1 = mk (max 0 (cap' - 1))
        let r2 = mk cap'
        let r3 = mk (cap' + delta + 1)
        (r1.HasCriticalErrors = false) && (r2.HasCriticalErrors = false) && (r3.HasCriticalErrors = true)

    [<Property(MaxTest = 50)>]
    let ``Lead time: equality passes and monotonic failset`` (lead: NonNegativeInt) =
        let leadVal = lead.Get % 60
        let run (min:int) (leadValue:int) =
            let rules = [| Dsl.leadTimeRuleDsl "PARK-LEAD-PROP" "leadMinutes" min |]
            let r = JsonDocument.Parse($"{{ \"leadMinutes\": {leadValue} }}").RootElement.GetRawText()
            Dsl.executeRulesWithData rules r None
        let low = run 5 leadVal
        let eq = run 10 10
        let high = run 20 leadVal
        let eqPasses = eq.HasCriticalErrors = false
        let lowFail, highFail = low.HasCriticalErrors, high.HasCriticalErrors
        eqPasses && ((not lowFail) || highFail) && (highFail || (not lowFail))

