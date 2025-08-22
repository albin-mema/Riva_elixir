namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine

/// Generalized rules: Closed days and No-overlap
module CommonClosedDaysAndNoOverlapTests =

    open ScenarioHarness

    // Builders
    type ReservationTimeBuilder() =
        let fields = Dictionary<string, obj>()
        do
            let tr = Dictionary<string, obj>()
            let start = DateTimeOffset(2025, 8, 21, 10, 0, 0, TimeSpan.Zero)
            let finish = DateTimeOffset(2025, 8, 21, 11, 0, 0, TimeSpan.Zero)
            tr["start"] <- box (start.ToString("o"))
            tr["startDate"] <- box (start.ToString("yyyy-MM-dd"))
            tr["end"] <- box (finish.ToString("o"))
            tr["timeZone"] <- box "UTC"
            fields["timeRange"] <- box tr
        member this.WithStart (dt: DateTimeOffset) =
            let tr = fields["timeRange"] :?> Dictionary<string, obj>
            tr["start"] <- box (dt.ToString("o"))
            tr["startDate"] <- box (dt.ToString("yyyy-MM-dd"))
            this
        member this.WithEnd (dt: DateTimeOffset) =
            let tr = fields["timeRange"] :?> Dictionary<string, obj>
            tr["end"] <- box (dt.ToString("o")); this
        member this.Build() = JsonSerializer.Serialize(fields)

    type ResourceCalendarBuilder() =
        let res = Dictionary<string, obj>()
        member this.WithClosedDates (dates: DateTimeOffset list) =
            let arr : string[] = dates |> List.map (fun d -> d.ToString("yyyy-MM-dd")) |> List.toArray
            let cal = Dictionary<string, obj>()
            cal["closedDates"] <- box arr
            let outer = Dictionary<string, obj>()
            outer["resourceAvailability"] <- box cal
            res.Clear(); for KeyValue(k,v) in outer do res[k] <- v
            this
        member this.Build() = JsonSerializer.Serialize(res)

    // Rule builders
    let closedDaysRule id : ValidationRule =
        // if closedDates contains startDate -> reject
        let cond =
            ComplexCondition.Simple {
                Field = "resourceAvailability.closedDates"
                Operator = Contains
                Value = None
                Values = None
                ReferenceField = Some "timeRange.startDate"
            }
        let action = { ActionType = Reject; Severity = Critical; Message = Some "Closed day"; Parameters = Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id=id; Name="Closed days"; Description=None; Priority=100; Enabled=true; Tags=[||]; Condition=Some cond; Actions=[|action|] }
          ValidationScope = "timeRange"; IsRequired = true; Dependencies=[||] }

    let noOverlapRule id : ValidationRule =
        // Ensure start < end; if not, reject
        let cond = ComplexCondition.Simple { Field="timeRange.start"; Operator=GreaterThanOrEqual; Value=None; Values=None; ReferenceField=Some "timeRange.end" }
        let action = { ActionType = Reject; Severity = Critical; Message = Some "Time range must have positive duration"; Parameters=Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id=id; Name="No overlap/positive duration"; Description=None; Priority=100; Enabled=true; Tags=[||]; Condition=Some cond; Actions=[|action|] }
          ValidationScope = "timeRange"; IsRequired = true; Dependencies=[||] }

    // Tests
    [<Fact>]
    let ``ClosedDays: fails when start date is closed`` () =
        let ruleSet = createRuleSet [| closedDaysRule "CD-001" |] None None
        let reservation = ReservationTimeBuilder().WithStart(DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)).Build()
        let resources = ResourceCalendarBuilder().WithClosedDates([ DateTimeOffset(2025,8,21,0,0,0,TimeSpan.Zero) ]).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``ClosedDays: passes when not closed`` () =
        let ruleSet = createRuleSet [| closedDaysRule "CD-002" |] None None
        let reservation = ReservationTimeBuilder().WithStart(DateTimeOffset(2025,8,22,10,0,0,TimeSpan.Zero)).Build()
        let resources = ResourceCalendarBuilder().WithClosedDates([ DateTimeOffset(2025,8,21,0,0,0,TimeSpan.Zero) ]).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``NoOverlap: fails when start >= end`` () =
        let ruleSet = createRuleSet [| noOverlapRule "NO-001" |] None None
        let r = ReservationTimeBuilder().WithStart(DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero)).WithEnd(DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero)).Build()
        let result = execute ruleSet r None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``NoOverlap: passes for start < end`` () =
        let ruleSet = createRuleSet [| noOverlapRule "NO-002" |] None None
        let r = ReservationTimeBuilder().WithStart(DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)).WithEnd(DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero)).Build()
        let result = execute ruleSet r None
        Assert.False(result.HasCriticalErrors)

