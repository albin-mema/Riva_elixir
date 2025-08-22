namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine

/// Generalized working-hours rules that apply across domains
module CommonWorkingHoursTests =

    open ScenarioHarness

    // Represent times as minutes since midnight (integers) to keep it cross-domain and simple
    // Fields used:
    //   reservation: timeRange.startMinutes, timeRange.endMinutes
    //   resources: resourceAvailability.hours.openMinutes, resourceAvailability.hours.closeMinutes

    let workingHoursRule id : ValidationRule =
        // fail if start < open OR end > close
        let startTooEarly = ComplexCondition.Simple {
            Field = "timeRange.startMinutes"
            Operator = LessThan
            Value = Some (SInt 0) // placeholder; we will use ReferenceField instead
            Values = None
            ReferenceField = Some "resourceAvailability.hours.openMinutes"
        }
        let endTooLate = ComplexCondition.Simple {
            Field = "timeRange.endMinutes"
            Operator = GreaterThan
            Value = Some (SInt 0)
            Values = None
            ReferenceField = Some "resourceAvailability.hours.closeMinutes"
        }
        let condition = ComplexCondition.Composite (Or, [ startTooEarly; endTooLate ])
        let action = { ActionType = Reject; Severity = Critical; Message = Some "Outside working hours"; Parameters=Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id = id; Name = "Working hours"; Description=None; Priority=100; Enabled=true; Tags=[||]; Condition=Some condition; Actions=[|action|] }
          ValidationScope = "timeRange"; IsRequired=true; Dependencies=[||] }

    type ReservationMinutesBuilder() =
        let fields = Dictionary<string, obj>()
        do
            let tr = Dictionary<string, obj>()
            tr["startMinutes"] <- box (11*60) // 11:00
            tr["endMinutes"] <- box (12*60 + 30) // 12:30
            fields["timeRange"] <- box tr
        member this.WithStart m =
            let tr = fields["timeRange"] :?> Dictionary<string, obj>
            tr["startMinutes"] <- box m
            this
        member this.WithEnd m =
            let tr = fields["timeRange"] :?> Dictionary<string, obj>
            tr["endMinutes"] <- box m
            this
        member this.Build() = JsonSerializer.Serialize(fields)

    type ResourceHoursBuilder() =
        let res = Dictionary<string, obj>()
        member this.WithHours (openM:int, closeM:int) =
            let hours = Dictionary<string, obj>()
            hours["openMinutes"] <- box openM
            hours["closeMinutes"] <- box closeM
            let ra = Dictionary<string, obj>()
            ra["hours"] <- box hours
            let outer = Dictionary<string, obj>()
            outer["resourceAvailability"] <- box ra
            res.Clear()
            for KeyValue(k, v) in outer do res[k] <- v
            this
        member this.Build() = JsonSerializer.Serialize(res)

    [<Fact>]
    let ``Working hours: fails when start before open`` () =
        let ruleSet = createRuleSet [| workingHoursRule "WH-001" |] None None
        let reservation = ReservationMinutesBuilder().WithStart(8*60).WithEnd(9*60).Build() // 08:00-09:00
        let resources = ResourceHoursBuilder().WithHours(9*60, 17*60).Build() // 09:00-17:00
        let result = execute ruleSet reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Working hours: fails when end after close`` () =
        let ruleSet = createRuleSet [| workingHoursRule "WH-002" |] None None
        let reservation = ReservationMinutesBuilder().WithStart(16*60+30).WithEnd(18*60).Build() // 16:30-18:00
        let resources = ResourceHoursBuilder().WithHours(9*60, 17*60).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Working hours: passes when fully within open-close`` () =
        let ruleSet = createRuleSet [| workingHoursRule "WH-003" |] None None
        let reservation = ReservationMinutesBuilder().WithStart(10*60).WithEnd(11*60).Build() // 10:00-11:00
        let resources = ResourceHoursBuilder().WithHours(9*60, 17*60).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.False(result.HasCriticalErrors)

