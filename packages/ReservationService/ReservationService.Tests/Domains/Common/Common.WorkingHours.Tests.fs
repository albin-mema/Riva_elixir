namespace ReservationService.Tests.Domains.Common

open System
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains.Shared.Dsl
open ReservationService.Core.Dsl.Types

module WorkingHours =

    [<Fact>]
    let ``Working hours: fails when start before open`` () =
        let rules = [| businessHoursRuleDsl "WH-001" "timeRange.startMinutes" "timeRange.endMinutes" "timeRange.openMinutes" "timeRange.closeMinutes" |]
        // reservation contains its own hours to keep this common
        let reservation =
            let startM, endM = 8*60, 9*60
            let openM, closeM = 9*60, 17*60
            $"{{\"timeRange\": {{ \"startMinutes\": {startM}, \"endMinutes\": {endM}, \"openMinutes\": {openM}, \"closeMinutes\": {closeM} }} }}"
        let result = Dsl.executeRulesWithData rules reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Working hours: fails when end after close`` () =
        let rules = [| businessHoursRuleDsl "WH-002" "timeRange.startMinutes" "timeRange.endMinutes" "timeRange.openMinutes" "timeRange.closeMinutes" |]
        let reservation =
            let startM, endM = 16*60+30, 18*60
            let openM, closeM = 9*60, 17*60
            $"{{\"timeRange\": {{ \"startMinutes\": {startM}, \"endMinutes\": {endM}, \"openMinutes\": {openM}, \"closeMinutes\": {closeM} }} }}"
        let result = Dsl.executeRulesWithData rules reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Working hours: passes when fully within open-close`` () =
        let rules = [| businessHoursRuleDsl "WH-003" "timeRange.startMinutes" "timeRange.endMinutes" "timeRange.openMinutes" "timeRange.closeMinutes" |]
        let reservation =
            let startM, endM = 10*60, 11*60
            let openM, closeM = 9*60, 17*60
            $"{{\"timeRange\": {{ \"startMinutes\": {startM}, \"endMinutes\": {endM}, \"openMinutes\": {openM}, \"closeMinutes\": {closeM} }} }}"
        let result = Dsl.executeRulesWithData rules reservation None
        Assert.False(result.HasCriticalErrors)

