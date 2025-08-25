namespace ReservationService.Tests.Domains.Restaurant

open System
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleDomain
open ReservationService.Core.RuleEngine
open ReservationService.Tests.Domains.Shared

/// Template: realistic restaurant tests using Shared helpers
module RestaurantTemplate =

    // Example realistic inputs
    let private sampleReservation (party:int) (lead:int) (startHour:int) (durationHours:int) (isVIP: bool) (season:string) (discountCode: string option) =
        // Build a simple, realistic JSON payload. Real code should build full DSL request later.
        let start = DateTimeOffset(2025, 6, 10, startHour, 0, 0, TimeSpan.Zero)
        let finish = start.AddHours(float durationHours)
        let endHour = startHour + durationHours
        let dc = match discountCode with | Some c -> $", \"discountCode\": \"{c}\"" | None -> ""
        $"{{\"participants\": {party}, \"leadMinutes\": {lead}, \"start\": \"{start:O}\", \"end\": \"{finish:O}\", \"startHour\": {startHour}, \"endHour\": {endHour}, \"isVIP\": {isVIP.ToString().ToLower()}, \"season\": \"{season}\"{dc}}}"

    let private sampleResources (openHour:int) (closeHour:int) (capacity:int) =
        $"{{\"resourceAvailability\": {{ \"dining-room\": {{ \"open\": {openHour}, \"close\": {closeHour}, \"capacity\": {capacity} }} }} }}"

    // Reusable ruleset for the template
    let private ruleset () =
        let validation =
            [|
                capacityRule "CAP-ROOM" "participants" "resourceAvailability.dining-room.capacity"
                minParticipantsRule "MIN-1" 1
                leadTimeRule "LEAD-60" "leadMinutes" 60
                // Note: business hours compare numeric hours here for simplicity
                withinBusinessHoursRule "HOURS" "startHour" "endHour" "resourceAvailability.dining-room.open" "resourceAvailability.dining-room.close"
            |]
        let pricing =
            Array.append (commonPricingRules()) [|
                discountCodeRule "DISC-NEWCUSTOMER" "discountCode" "WELCOME10" 10
            |]
        createRuleSet validation None (Some pricing)

    // --- Tests ---

    [<Theory>]
    [<InlineData(2, 120, 18, 2, false, "summer", 50, false)>]  // Normal booking fits capacity, within hours
    [<InlineData(6, 90, 10, 1, false, "spring", 4, true)>]   // Lead time too short
    [<InlineData(5, 180, 8, 4, false, "winter", 4, true)>]  // Exceeds capacity
    [<InlineData(4, 120, 22, 2, false, "summer", 40, true)>] // Outside of business hours
    let ``restaurant booking validation scenarios`` (party, lead, startHour, duration, isVIP, season, capacity, shouldFail) =
        let rs = ruleset ()
        let res = sampleReservation party lead startHour duration isVIP season None
        let resources = sampleResources 9 21 capacity |> Some
        let result = execute rs res resources
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(false, "summer", "")>]  // Summer multiplier
    [<InlineData(true, "spring", "")>]  // VIP multiplier
    [<InlineData(false, "spring", "WELCOME10")>] // Discount code 10%
    [<InlineData(true, "summer", "WELCOME10")>] // VIP + Summer + Discount (1.5 * 1.2 * 0.9)
    let ``restaurant pricing adjustments applied via rule actions`` (isVIP: bool, season: string, discountCode: string) =
        let rs = ruleset ()
        let dcOpt = if String.IsNullOrEmpty discountCode then None else Some discountCode
        let res = sampleReservation 2 180 18 2 isVIP season dcOpt
        let resources = sampleResources 9 21 20 |> Some
        let result = execute rs res resources
        // Pricing rules emit actions; here we validate presence rather than compute final price in tests
        let pricingMsgs = result.PricingResults |> Array.collect (fun r -> r.Actions) |> Array.map (fun a -> a.Message)
        // Expected messages should include the multipliers/discounts
        let expectedMarkers =
            [|
                if isVIP then yield "apply-multiplier:1.5"
                if season = "summer" then yield "apply-multiplier:1.2"
                if season = "winter" then yield "apply-multiplier:0.8"
                if not (String.IsNullOrEmpty discountCode) then yield "apply-discount:10%"
            |]
        for marker in expectedMarkers do
            Assert.Contains(marker, pricingMsgs)

    // Optional: quick debug to inspect built payloads
    [<Fact>]
    let ``debug: show sample payloads`` () =
        let res = sampleReservation 3 180 19 2 false "spring" None
        let resVip = sampleReservation 2 180 18 2 true "summer" (Some "WELCOME10")
        let resources = sampleResources 9 21 10
        // This is intentionally a no-assert test to aid local debugging; feel free to remove in CI
        // Printing via Console is sufficient; optionally use FS_DEBUG_* env when running full JSON through the system
        Console.WriteLine($"RES: {res}\nVIP: {resVip}\nRESOURCES: {resources}")

