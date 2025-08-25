namespace ReservationService.Tests.Domains.Common

open System
open System.Text.Json
open System.Text.Json.Nodes
open Xunit
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Core.RuleDomain
open ReservationService.Tests.Domains.Shared

module DurationAndBlackout =

    // Builders (inline for brevity)
    let private buildReservation (startDt: DateTimeOffset) (endDt: DateTimeOffset) =
        let node = JsonObject()
        let tr = JsonObject()
        tr["start"] <- JsonValue.Create(startDt.ToString("o"))
        tr["end"] <- JsonValue.Create(endDt.ToString("o"))
        tr["timeZone"] <- JsonValue.Create("UTC")
        tr["durationMinutes"] <- JsonValue.Create(int ((endDt - startDt).TotalMinutes))
        tr["startMinutes"] <- JsonValue.Create(startDt.Hour*60 + startDt.Minute)
        tr["endMinutes"] <- JsonValue.Create(endDt.Hour*60 + endDt.Minute)
        node["timeRange"] <- tr
        node.ToJsonString()

    let private addBlackout (reservationJson: string) (blkStart: DateTimeOffset, blkEnd: DateTimeOffset) =
        let node = JsonNode.Parse(reservationJson) :?> JsonObject
        let blk = JsonObject()
        blk["startMinutes"] <- JsonValue.Create(blkStart.Hour*60 + blkStart.Minute)
        blk["endMinutes"] <- JsonValue.Create(blkEnd.Hour*60 + blkEnd.Minute)
        node["blackout"] <- blk
        node.ToJsonString()

    // Rule builders
    let private maxDurationRule id (maxMinutes:int) : ValidationRule =
        let cond = ComplexCondition.Simple { Field="timeRange.durationMinutes"; Operator=GreaterThan; Value=Some (SInt maxMinutes); ReferenceField=None }
        let action = { Type=Validation; Severity=Critical; Message=sprintf "Duration exceeds %d minutes" maxMinutes; Parameters=Map.empty }
        { Rule = RuleHelpers.createRule id "Max duration" "" (Some cond) [|action|]
          ValidationType = "timeRange"; ErrorMessage = sprintf "Duration exceeds %d minutes" maxMinutes }

    let private blackoutOverlapRule id : ValidationRule =
        let startBeforeBlockEnd = ComplexCondition.Simple { Field="timeRange.startMinutes"; Operator=LessThan; Value=None; ReferenceField=Some "blackout.endMinutes" }
        let endAfterBlockStart = ComplexCondition.Simple { Field="timeRange.endMinutes"; Operator=GreaterThan; Value=None; ReferenceField=Some "blackout.startMinutes" }
        let cond = ComplexCondition.Composite (And, [ startBeforeBlockEnd; endAfterBlockStart ])
        let action = { Type=Validation; Severity=Critical; Message="Reservation overlaps a blackout"; Parameters=Map.empty }
        { Rule = RuleHelpers.createRule id "Blackout overlap" "" (Some cond) [|action|]
          ValidationType = "timeRange"; ErrorMessage = "Reservation overlaps a blackout" }

    // Tests
    [<Fact>]
    let ``MaxDuration: fails when duration exceeds max`` () =
        let ruleSet = createRuleSet [| maxDurationRule "MAXD-001" 30 |] None None
        let reservation = buildReservation (DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)) (DateTimeOffset(2025,8,21,11,1,0,TimeSpan.Zero))
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``MaxDuration: passes when duration within max`` () =
        let ruleSet = createRuleSet [| maxDurationRule "MAXD-002" 120 |] None None
        let reservation = buildReservation (DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)) (DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero))
        let result = execute ruleSet reservation None
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Blackout: fails when overlaps any blackout window`` () =
        let ruleSet = createRuleSet [| blackoutOverlapRule "BLK-001" |] None None
        let reservation = buildReservation (DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)) (DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero))
        let reservation = addBlackout reservation (DateTimeOffset(2025,8,21,10,30,0,TimeSpan.Zero), DateTimeOffset(2025,8,21,10,45,0,TimeSpan.Zero))
        let result = execute ruleSet reservation None
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Blackout: passes when no overlap`` () =
        let ruleSet = createRuleSet [| blackoutOverlapRule "BLK-002" |] None None
        let reservation = buildReservation (DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)) (DateTimeOffset(2025,8,21,11,0,0,TimeSpan.Zero))
        let result = execute ruleSet reservation None
        Assert.False(result.HasCriticalErrors)

