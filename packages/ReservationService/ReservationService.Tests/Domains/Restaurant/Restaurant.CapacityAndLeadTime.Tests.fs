namespace ReservationService.Tests.Domains.Restaurant

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Core.RuleDomain
open ReservationService.Tests.Domains.Shared

module CapacityAndLeadTime =

    // Minimal, generalized restaurant reservation JSON builder
    type ReservationBuilder() =
        let fields = Dictionary<string, obj>()
        do
            fields["participants"] <- box 4
            fields["leadMinutes"] <- box 120
            let tr = Dictionary<string, obj>()
            tr["start"] <- box (DateTimeOffset(2025,8,21,19,0,0,TimeSpan.Zero).ToString("o"))
            tr["end"] <- box (DateTimeOffset(2025,8,21,20,0,0,TimeSpan.Zero).ToString("o"))
            tr["timeZone"] <- box "UTC"
            fields["timeRange"] <- box tr
        member this.WithParticipants n = fields["participants"] <- box n; this
        member this.WithLeadTime n = fields["leadMinutes"] <- box n; this
        member this.With (k:string, v: obj) = fields[k] <- v; this
        member this.Build() = JsonSerializer.Serialize(fields)

    // Rules
    let capacityRule id participantsField capacityField : ValidationRule =
        let cond = ComplexCondition.Simple { Field = participantsField; Operator = GreaterThan; Value = None; ReferenceField = Some capacityField }
        let action = { Type = Validation; Severity = Critical; Message = "Capacity exceeded"; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Capacity" "" (Some cond) [| action |]
          ValidationType = "capacity"; ErrorMessage = "Capacity exceeded" }

    let minParticipantsRule id minCount : ValidationRule =
        let cond = ComplexCondition.Simple { Field = "participants"; Operator = LessThan; Value = Some (SInt minCount); ReferenceField = None }
        let action = { Type = Validation; Severity = Critical; Message = $"Minimum {minCount} participant(s) required"; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Minimum participants" "" (Some cond) [| action |]
          ValidationType = "min-participants"; ErrorMessage = $"Minimum {minCount} participant(s) required" }

    let leadTimeRule id leadField minMinutes : ValidationRule =
        let cond = ComplexCondition.Simple { Field = leadField; Operator = LessThan; Value = Some (SInt minMinutes); ReferenceField = None }
        let action = { Type = Validation; Severity = Critical; Message = $"Minimum {minMinutes} minutes lead time required"; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Lead time" "" (Some cond) [| action |]
          ValidationType = "lead-time"; ErrorMessage = $"Minimum {minMinutes} minutes lead time required" }

    // Tests
    [<Theory>]
    [<InlineData(1, 2, false)>]
    [<InlineData(2, 2, false)>]
    [<InlineData(3, 2, true)>]
    [<InlineData(0, 2, true)>]
    [<InlineData(-1, 2, true)>]
    let ``Basic capacity validation`` (participants: int, capacity: int, shouldFail: bool) =
        let rules = [| capacityRule "CAP-001" "participants" "resourceAvailability.table-1.capacity"; minParticipantsRule "MIN-001" 1 |]
        let ruleSet = createRuleSet rules None None
        let reservation = ReservationBuilder().WithParticipants(participants).With("resourceAvailability", box {| ``table-1`` = {| capacity = capacity |} |}).Build()
        let result = execute ruleSet reservation None
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(false, false, false, false, 30, 60, true)>]
    [<InlineData(false, false, false, false, 60, 60, false)>]
    [<InlineData(false, false, false, false, 90, 60, false)>]
    [<InlineData(true, false, false, false, 120, 180, true)>]
    [<InlineData(true, false, false, false, 180, 180, false)>]
    [<InlineData(false, true, false, false, 60, 90, true)>]
    [<InlineData(false, false, true, false, 90, 120, true)>]
    [<InlineData(false, false, false, true, 15, 30, true)>]
    [<InlineData(false, false, false, true, 30, 30, false)>]
    let ``Dynamic lead time validation`` (isHoliday: bool, isWeekend: bool, isPeakHour: bool, isVIP: bool, leadMinutes: int, requiredMinutes: int, shouldFail: bool) =
        let rules = [|
            if isHoliday then leadTimeRule "LEAD-HOL" "leadMinutes" 180
            elif isWeekend then leadTimeRule "LEAD-WKD" "leadMinutes" 90
            elif isPeakHour then leadTimeRule "LEAD-PEAK" "leadMinutes" 120
            elif isVIP then leadTimeRule "LEAD-VIP" "leadMinutes" 30
            else leadTimeRule "LEAD-REG" "leadMinutes" 60
        |]
        let ruleSet = createRuleSet rules None None
        let reservation =
            ReservationBuilder()
                .WithLeadTime(leadMinutes)
                .With("isHoliday", box isHoliday)
                .With("isWeekend", box isWeekend)
                .With("isPeakHour", box isPeakHour)
                .With("isVIP", box isVIP)
                .Build()
        let result = execute ruleSet reservation None
        Assert.Equal(shouldFail, result.HasCriticalErrors)

