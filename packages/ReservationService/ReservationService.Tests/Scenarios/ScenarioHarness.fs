namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine

/// A small, shared harness to make scenario tests concise and consistent
module ScenarioHarness =

    /// Create a RuleSet with optional groups
    let createRuleSet (validationRules: ValidationRule[]) (businessRulesOpt: BusinessRule[] option) (pricingRulesOpt: PricingRule[] option) : RuleSet =
        {
            ValidationRules = validationRules
            BusinessRules = defaultArg businessRulesOpt [||]
            ConflictRules = [||]
            PricingRules = defaultArg pricingRulesOpt [||]
            ApprovalRules = [||]
            Metadata = Map.empty
        }

    /// Minimal default execution context
    let defaultContext () : RuleExecutionContext =
        {
            Timestamp = DateTimeOffset.UtcNow
            ExecutedBy = None
            ContextData = Map.empty
            Parameters = Map.empty
            TraceEnabled = false
        }

    /// Combine a reservation JSON payload with resourceAvailability JSON (if provided)
    /// - If resourceJsonOpt is None, returns the reservation as-is
    /// - If provided, adds/overwrites the `resourceAvailability` property under the reservation root
    let combineReservationAndResources (reservationJson: string) (resourceJsonOpt: string option) : JsonDocument =
        match resourceJsonOpt with
        | None -> JsonDocument.Parse(reservationJson)
        | Some resourceJson ->
            let reservationDoc = JsonDocument.Parse(reservationJson)
            let resourceDoc = JsonDocument.Parse(resourceJson)
            let combined = Dictionary<string, JsonElement>()

            // Copy all reservation properties
            for p in reservationDoc.RootElement.EnumerateObject() do
                combined[p.Name] <- p.Value

            // Inject/overwrite resourceAvailability from resource JSON
            let mutable resAvailEl = Unchecked.defaultof<JsonElement>
            if resourceDoc.RootElement.ValueKind = JsonValueKind.Object && resourceDoc.RootElement.TryGetProperty("resourceAvailability", &resAvailEl) then
                combined["resourceAvailability"] <- resAvailEl
            else
                // If resource JSON isn't wrapped, attempt to wrap the whole object as resourceAvailability
                combined["resourceAvailability"] <- resourceDoc.RootElement

            // Serialize back to a single document
            let merged = JsonSerializer.Serialize(combined)
            JsonDocument.Parse(merged)

    /// Execute a rule set against reservation + optional resources JSON
    let execute (ruleSet: RuleSet) (reservationJson: string) (resourceJsonOpt: string option) : RuleSetEvaluationResult =
        use doc = combineReservationAndResources reservationJson resourceJsonOpt
        let ctx = defaultContext()
        interpretRules ruleSet doc ctx

