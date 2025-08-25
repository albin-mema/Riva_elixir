namespace ReservationService.Tests.Domains.Common

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Core.RuleDomain
open ReservationService.Tests.Domains.Shared

module ClosedDaysAndNoOverlap =

    // Builders kept minimal and domain-agnostic
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

    // Rules (generic)
    let closedDaysRule id : ValidationRule =
        let cond = ComplexCondition.Simple {
            Field = "timeRange.startDate"
            Operator = In
            Value = None
            ReferenceField = Some "resourceAvailability.closedDates"
        }
        let action = { Type = Validation; Severity = Critical; Message = "Closed day"; Parameters = Map.empty }
        { Rule = RuleHelpers.createRule id "Closed days" "" (Some cond) [|action|]
          ValidationType = "timeRange"; ErrorMessage = "Closed day" }

    let noOverlapRule id : ValidationRule =
        let cond = ComplexCondition.Simple { Field="timeRange.start"; Operator=GreaterThanOrEqual; Value=None; ReferenceField=Some "timeRange.end" }
        let action = { Type = Validation; Severity = Critical; Message = "Time range must have positive duration"; Parameters=Map.empty }
        { Rule = RuleHelpers.createRule id "No overlap/positive duration" "" (Some cond) [|action|]
          ValidationType = "timeRange"; ErrorMessage = "Time range must have positive duration" }

    // Tests
    [<Fact>]
    let ``ClosedDays: fails when start date is closed`` () =
        let ruleSet = createRuleSet [| closedDaysRule "CD-001" |] None None
        let reservation = ReservationTimeBuilder().WithStart(DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)).Build()
        let resources = ResourceCalendarBuilder().WithClosedDates([ DateTimeOffset(2025,8,21,0,0,0,TimeSpan.Zero); DateTimeOffset(2025,9,1,0,0,0,TimeSpan.Zero) ]).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``ClosedDays: passes when not closed`` () =
        let ruleSet = createRuleSet [| closedDaysRule "CD-002" |] None None
        let reservation = ReservationTimeBuilder().WithStart(DateTimeOffset(2025,8,22,10,0,0,TimeSpan.Zero)).Build()
        let result = execute ruleSet reservation None
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

