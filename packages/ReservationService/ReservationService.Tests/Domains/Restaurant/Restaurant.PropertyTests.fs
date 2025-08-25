namespace ReservationService.Tests.Domains.Restaurant

open FsCheck
open FsCheck.Xunit
open System.Text.Json
open ReservationService.Core.RuleTypes
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Shared.Dsl
open ReservationService.Core.Dsl.Types

module RestaurantPropertyTests =

    [<Property(MaxTest = 50)>]
    let ``Capacity: increasing participants monotonically increases failure likelihood`` (baseCap: NonNegativeInt, delta: NonNegativeInt) =
        let cap = max 1 baseCap.Get
        let delta' = delta.Get % 5
        let rules = [| Dsl.capacityRuleDsl "CAP-PROP" "participants" "resourceAvailability.table-1.capacity" |]
        let mk participants =
            let resJson = $"{{ \"resourceAvailability\": {{ \"table-1\": {{ \"capacity\": {cap} }} }} }}"
            let r = JsonDocument.Parse($"{{ \"participants\": {participants} }}").RootElement.GetRawText()
            Dsl.executeRulesWithData rules r (Some resJson)
        let r1 = mk (cap - 1)
        let r2 = mk (cap)
        let r3 = mk (cap + delta' + 1)
        (r1.HasCriticalErrors = false) && (r2.HasCriticalErrors = false) && (r3.HasCriticalErrors = true)

    [<Property(MaxTest = 50)>]
    let ``LeadTime: raising minimum threshold never reduces failures`` (lead: NonNegativeInt, bump: NonNegativeInt) =
        let leadVal = lead.Get % 300
        let bumpVal = bump.Get % 120
        let r leadMin =
            let rules = [| Dsl.leadTimeRuleDsl "LEAD-PROP" "leadMinutes" leadMin |]
            let r = JsonDocument.Parse($"{{ \"leadMinutes\": {leadVal} }}").RootElement.GetRawText()
            Dsl.executeRulesWithData rules r None
        let rLow = r 30
        let rHigh = r 120
        // If we fail with a lower threshold, we must also fail with a higher one; if pass with higher, must pass with lower
        ((rLow.HasCriticalErrors && rHigh.HasCriticalErrors) || ((not rHigh.HasCriticalErrors) && (not rLow.HasCriticalErrors)))

