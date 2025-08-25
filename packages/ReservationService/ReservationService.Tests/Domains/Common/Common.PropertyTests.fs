namespace ReservationService.Tests.Domains.Common

open System
open FsCheck
open FsCheck.Xunit
open System.Text.Json
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains.Shared

module CommonPropertyTests =

    [<Property(MaxTest = 50)>]
    let ``NoOverlap: start < end should pass`` (day: NonNegativeInt, sh: NonNegativeInt, dur: PositiveInt) =
        let day' = (day.Get % 28) + 1
        let sh' = sh.Get % 23
        let durH = (dur.Get % 4) + 1
        let start = DateTimeOffset(2025,8,day',sh',0,0,TimeSpan.Zero)
        let finish = start.AddHours(float durH)
        let ruleSet = createRuleSet [| ClosedDaysAndNoOverlap.noOverlapRule "NO-PROP-OK" |] None None
        let tr = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{start:o}\", \"end\": \"{finish:o}\" }} }}").RootElement.GetRawText()
        let result = execute ruleSet tr None
        result.HasCriticalErrors = false

    [<Property(MaxTest = 50)>]
    let ``NoOverlap: start >= end should fail`` (day: NonNegativeInt, sh: NonNegativeInt) =
        let day' = (day.Get % 28) + 1
        let sh' = sh.Get % 24
        let start = DateTimeOffset(2025,8,day',sh',0,0,TimeSpan.Zero)
        let ruleSet = createRuleSet [| ClosedDaysAndNoOverlap.noOverlapRule "NO-PROP-FAIL" |] None None
        let tr = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{start:o}\", \"end\": \"{start:o}\" }} }}").RootElement.GetRawText()
        let result = execute ruleSet tr None
        result.HasCriticalErrors = true

    [<Property(MaxTest = 50)>]
    let ``ClosedDays: empty closedDates never fails`` (day: NonNegativeInt) =
        let day' = (day.Get % 28) + 1
        let date = DateTimeOffset(2025,8,day',10,0,0,TimeSpan.Zero)
        let dateIso = date.ToString("o")
        let dateOnly = date.ToString("yyyy-MM-dd")
        let r = JsonDocument.Parse($"{{ \"timeRange\": {{ \"start\": \"{dateIso}\", \"startDate\": \"{dateOnly}\" }} }}").RootElement.GetRawText()
        let ruleSet = createRuleSet [| ClosedDaysAndNoOverlap.closedDaysRule "CD-PROP" |] None None
        let result = execute ruleSet r (Some "{ \"resourceAvailability\": { \"closedDates\": [] } }")
        result.HasCriticalErrors = false

