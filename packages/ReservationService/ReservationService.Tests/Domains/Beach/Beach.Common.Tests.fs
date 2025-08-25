namespace ReservationService.Tests.Domains.Beach

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine
open ReservationService.Core.RuleDomain
open ReservationService.Tests.Domains.Shared
open ReservationService.Tests.Domains

module Common =

    // Focused, domain-agnostic subset extracted from the enhanced Beach suite

    type ReservationJsonBuilder() =
        let fields = Dictionary<string, obj>()
        do
            fields["participants"] <- box 2
            fields["leadMinutes"] <- box 120
            fields["umbrellaSize"] <- box "standard"
            fields["beachZone"] <- box "main"
            let start = DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)
            let finish = DateTimeOffset(2025,8,21,12,0,0,TimeSpan.Zero)
            let tr = Dictionary<string, obj>()
            tr["start"] <- box (start.ToString("o"))
            tr["end"] <- box (finish.ToString("o"))
            fields["timeRange"] <- box tr
        member this.With (k:string, v: obj) = fields[k] <- v; this
        member this.WithParticipants n = fields["participants"] <- box n; this
        member this.WithLeadMinutes n = fields["leadMinutes"] <- box n; this
        member this.WithUmbrella s = fields["umbrellaSize"] <- box s; this
        member this.WithZone z = fields["beachZone"] <- box z; this
        member this.Build() = JsonSerializer.Serialize(fields)

    [<Fact>]
    let ``Beach: capacity cannot be exceeded in a zone`` () =
        let ruleSet = createRuleSet [| Shared.capacityRule "BEACH-CAP" "participants" "resourceAvailability.main.capacity" |] None None
        let reservation = ReservationJsonBuilder().WithParticipants(5).Build()
        let resources = JsonSerializer.Serialize({| resourceAvailability = {| main = {| capacity = 4 |} |} |})
        let result = execute ruleSet reservation (Some resources)
        Assert.True(result.HasCriticalErrors)

    [<Theory>]
    [<InlineData("standard", "main", false)>]
    [<InlineData("nonexistent", "main", true)>]
    [<InlineData("standard", "nonexistent", true)>]
    let ``Beach: umbrella size and zone validation (simplified)`` (umbrellaSize: string, zone: string, shouldFail: bool) =
        // Fail if either is an explicitly unsupported literal
        let cond = ComplexCondition.Composite (Or, [
            ComplexCondition.Simple { Field = "umbrellaSize"; Operator = Equal; Value = Some (SString "nonexistent"); ReferenceField = None }
            ComplexCondition.Simple { Field = "beachZone"; Operator = Equal; Value = Some (SString "nonexistent"); ReferenceField = None }
        ])
        let action = { Type = Validation; Severity = Critical; Message = "Invalid umbrella size or zone"; Parameters = Map.empty }
        let rule : ValidationRule = { Rule = RuleHelpers.createRule "UMB-ZONE" "Umbrella/Zone" "" (Some cond) [|action|]; ValidationType = "beach"; ErrorMessage = "Invalid umbrella size or zone" }
        let ruleSet = createRuleSet [| rule |] None None
        let reservation = ReservationJsonBuilder().WithUmbrella(umbrellaSize).WithZone(zone).Build()
        let result = execute ruleSet reservation None
        Assert.Equal(shouldFail, result.HasCriticalErrors)

