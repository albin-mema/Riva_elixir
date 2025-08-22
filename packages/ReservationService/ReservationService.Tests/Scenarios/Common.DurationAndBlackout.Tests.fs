namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine

module CommonDurationAndBlackoutTests =

    open ScenarioHarness

    // Reservation builder with timeRange and derived duration
    type ReservationTimeBuilder() =
        let fields = Dictionary<string, obj>()
        do
            let tr = Dictionary<string, obj>()
            let start = DateTimeOffset(2025, 8, 21, 10, 0, 0, TimeSpan.Zero)
            let finish = DateTimeOffset(2025, 8, 21, 11, 0, 0, TimeSpan.Zero)
            tr["start"] <- box (start.ToString("o"))
            tr["end"] <- box (finish.ToString("o"))
            tr["timeZone"] <- box "UTC"
            tr["durationMinutes"] <- box (int ((finish - start).TotalMinutes))
            tr["startMinutes"] <- box (start.Hour*60 + start.Minute)
            tr["endMinutes"] <- box (finish.Hour*60 + finish.Minute)
            fields["timeRange"] <- box tr
        member this.WithStart (dt: DateTimeOffset) =
            let tr = fields["timeRange"] :?> Dictionary<string, obj>
            let endVal = tr["end"] :?> string |> DateTimeOffset.Parse
            tr["start"] <- box (dt.ToString("o"))
            tr["durationMinutes"] <- box (int ((endVal - dt).TotalMinutes))
            tr["startMinutes"] <- box (dt.Hour*60 + dt.Minute)
            this
        member this.WithEnd (dt: DateTimeOffset) =
            let tr = fields["timeRange"] :?> Dictionary<string, obj>
            let startVal = tr["start"] :?> string |> DateTimeOffset.Parse
            tr["end"] <- box (dt.ToString("o"))
            tr["durationMinutes"] <- box (int ((dt - startVal).TotalMinutes))
            tr["endMinutes"] <- box (dt.Hour*60 + dt.Minute)
            this
        member this.Build() = JsonSerializer.Serialize(fields)

    // Resources builder including blackout ranges
    type ResourceBlackoutsBuilder() =
        let res = Dictionary<string, obj>()
        member this.WithBlackout (startDt: DateTimeOffset, endDt: DateTimeOffset) =
            // serialize as single blackout object: resourceAvailability.blackout.{start,end}
            let cal = Dictionary<string, obj>()
            let blk = Dictionary<string, obj>()
            blk["start"] <- box (startDt.ToString("o"))
            blk["end"] <- box (endDt.ToString("o"))
            blk["startMinutes"] <- box (startDt.Hour*60 + startDt.Minute)
            blk["endMinutes"] <- box (endDt.Hour*60 + endDt.Minute)
            cal["blackout"] <- box blk
            let outer = Dictionary<string, obj>()
            outer["resourceAvailability"] <- box cal
            res.Clear(); for KeyValue(k,v) in outer do res[k] <- v
            this
        member this.Build() = JsonSerializer.Serialize(res)

    // Rule builders
    let maxDurationRule id (maxMinutes:int) : ValidationRule =
        let cond = ComplexCondition.Simple { Field="timeRange.durationMinutes"; Operator=GreaterThan; Value=Some (SInt maxMinutes); Values=None; ReferenceField=None }
        let action = { ActionType=Reject; Severity=Critical; Message=Some (sprintf "Duration exceeds %d minutes" maxMinutes); Parameters=Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id=id; Name="Max duration"; Description=None; Priority=100; Enabled=true; Tags=[||]; Condition=Some cond; Actions=[|action|] }
          ValidationScope = "timeRange"; IsRequired=true; Dependencies=[||] }

    // Overlap: reservation [start, end) intersects any blackout [s, e)
    let blackoutOverlapRule id : ValidationRule =
        // Build: (start < blackout.end) AND (end > blackout.start)
        let startBeforeBlockEnd = ComplexCondition.Simple { Field="timeRange.startMinutes"; Operator=LessThan; Value=None; Values=None; ReferenceField=Some "resourceAvailability.blackout.endMinutes" }
        let endAfterBlockStart = ComplexCondition.Simple { Field="timeRange.endMinutes"; Operator=GreaterThan; Value=None; Values=None; ReferenceField=Some "resourceAvailability.blackout.startMinutes" }
        let cond = ComplexCondition.Composite (And, [ startBeforeBlockEnd; endAfterBlockStart ])
        let action = { ActionType=Reject; Severity=Critical; Message=Some "Reservation overlaps a blackout"; Parameters=Map.empty; PropertyUpdates=None; Suggestions=None }
        { Rule = { Id=id; Name="Blackout overlap"; Description=None; Priority=100; Enabled=true; Tags=[||]; Condition=Some cond; Actions=[|action|] }
          ValidationScope = "timeRange"; IsRequired=true; Dependencies=[||] }

    // Tests
    [<Fact>]
    let ``MaxDuration: fails when duration exceeds max`` () =
        let ruleSet = createRuleSet [| maxDurationRule "MAXD-001" 30 |] None None
        let reservation = ReservationTimeBuilder().WithEnd(DateTimeOffset(2025,8,21,10,45,0,TimeSpan.Zero)).WithEnd(DateTimeOffset(2025,8,21,11,1,0,TimeSpan.Zero)).Build()
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``MaxDuration: passes when duration within max`` () =
        let ruleSet = createRuleSet [| maxDurationRule "MAXD-002" 120 |] None None
        let reservation = ReservationTimeBuilder().WithEnd(DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero)).Build()
        let result = execute ruleSet reservation None
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Blackout: fails when overlaps any blackout window`` () =
        let ruleSet = createRuleSet [| blackoutOverlapRule "BLK-001" |] None None
        let start = DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)
        let endv = DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero)
        let reservation = ReservationTimeBuilder().WithStart(start).WithEnd(endv).Build()
        let resources =
            ResourceBlackoutsBuilder().WithBlackout(
                DateTimeOffset(2025,8,21,10,30,0,TimeSpan.Zero), DateTimeOffset(2025,8,21,10,45,0,TimeSpan.Zero)
            ).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Blackout: passes when no overlap`` () =
        let ruleSet = createRuleSet [| blackoutOverlapRule "BLK-002" |] None None
        let start = DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)
        let endv = DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero)
        let reservation = ReservationTimeBuilder().WithStart(start).WithEnd(endv).Build()
        let resources =
            ResourceBlackoutsBuilder().WithBlackout(
                DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero), DateTimeOffset(2025,8,21,11,15,0,TimeSpan.Zero)
            ).Build()
        let result = execute ruleSet reservation (Some resources)
        Assert.False(result.HasCriticalErrors)

