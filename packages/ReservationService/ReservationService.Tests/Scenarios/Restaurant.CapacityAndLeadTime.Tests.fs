namespace ReservationService.Tests.Enhanced

(***
Enhanced Domain: Restaurant table reservations with comprehensive testing
Features tested:
- Capacity management (single/multi-table, accessibility, seasonal)
- Lead time requirements (dynamic based on context)
- Business hours and special schedules
- Pricing rules and surge pricing
- Conflict resolution and overbooking
- Integration scenarios and error handling
- Performance under load
- Security and validation
***)

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open ReservationService.Core
open ReservationService.Core.RuleTypes
open ReservationService.Core.RuleEngine

// ===== TEST DATA BUILDERS =====

module TestData =

    type ReservationBuilder() =
        let mutable participants = 4
        let mutable leadMinutes = 120
        let mutable startTime = DateTimeOffset(2025,8,21,19,0,0,TimeSpan.Zero)
        let mutable endTime = DateTimeOffset(2025,8,21,20,0,0,TimeSpan.Zero)
        let mutable isVIP = false
        let mutable isHoliday = false
        let mutable isPeakHour = false
        let mutable isWeekend = false
        let mutable hasAccessibilityNeeds = false
        let mutable hasSpecialDietary = false
        let mutable isWalkIn = false
        let mutable isSpecialEvent = false
        let mutable hasAuthorization = false
        let mutable customFields = Map.empty<string, obj>

        member this.WithParticipants(count: int) =
            participants <- count; this

        member this.WithLeadTime(minutes: int) =
            leadMinutes <- minutes; this

        member this.WithTimeRange(start: DateTimeOffset, finish: DateTimeOffset) =
            startTime <- start; endTime <- finish; this

        member this.AsVIP() =
            isVIP <- true; this

        member this.AsHoliday() =
            isHoliday <- true; this

        member this.AsPeakHour() =
            isPeakHour <- true; this

        member this.AsWeekend() =
            isWeekend <- true; this

        member this.WithAccessibilityNeeds() =
            hasAccessibilityNeeds <- true; this

        member this.WithSpecialDietary() =
            hasSpecialDietary <- true; this

        member this.AsWalkIn() =
            isWalkIn <- true; leadMinutes <- 0; this

        member this.AsSpecialEvent(authorized: bool) =
            isSpecialEvent <- true; hasAuthorization <- authorized; this

        member this.WithCustomField(key: string, value: obj) =
            customFields <- customFields.Add(key, value); this

        member this.Build() =
            // Build JSON via object graph to avoid interpolation brace escaping issues
            let fields = Dictionary<string, obj>()
            fields["participants"] <- box participants
            fields["leadMinutes"] <- box leadMinutes
            let timeRange = Dictionary<string, obj>()
            timeRange["start"] <- box (startTime.ToString("o"))
            timeRange["end"] <- box (endTime.ToString("o"))
            timeRange["timeZone"] <- box "UTC"
            fields["timeRange"] <- box timeRange
            fields["isVIP"] <- box isVIP
            fields["isHoliday"] <- box isHoliday
            fields["isPeakHour"] <- box isPeakHour
            fields["isWeekend"] <- box isWeekend
            fields["hasAccessibilityNeeds"] <- box hasAccessibilityNeeds
            fields["hasSpecialDietary"] <- box hasSpecialDietary
            fields["isWalkIn"] <- box isWalkIn
            fields["isSpecialEvent"] <- box isSpecialEvent
            fields["hasAuthorization"] <- box hasAuthorization
            // Merge custom fields (overrides if same key)
            customFields |> Map.iter (fun k v -> fields[k] <- v)
            JsonSerializer.Serialize(fields)

    type ResourceBuilder() =
        let resources = Dictionary<string, obj>()

        member this.AddTable(name: string, capacity: int, ?isAccessible: bool, ?isOutdoor: bool, ?isPrivate: bool) =
            let table =
                {|
                    capacity = capacity
                    isAccessible = defaultArg isAccessible false
                    isOutdoor = defaultArg isOutdoor false
                    isPrivate = defaultArg isPrivate false
                |}
            resources.[name] <- table
            this

        member this.AddTotalCapacity(total: int) =
            resources.["totalCapacity"] <- total
            this

        member this.WithDynamicPricing(multiplier: decimal) =
            resources.["pricingMultiplier"] <- multiplier
            this

        member this.Build() =
            // Wrap resources under resourceAvailability and serialize
            let outer = Dictionary<string, obj>()
            outer["resourceAvailability"] <- box resources
            JsonSerializer.Serialize(outer)

    type RuleBuilder() =

        static member CapacityRule(id: string, field: string, referenceField: string, ?message: string) =
            let cond = ComplexCondition.Simple {
                Field = field
                Operator = GreaterThan
                Value = None
                Values = None
                ReferenceField = Some referenceField
            }
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some "Capacity exceeded")
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Capacity validation"
                    Description = None
                    Priority = 100
                    Enabled = true
                    Tags = [||]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "participants"
                IsRequired = true
                Dependencies = [||]
            }

        static member LeadTimeRule(id: string, field: string, minMinutes: int, ?message: string) =
            let cond = ComplexCondition.Simple {
                Field = field
                Operator = LessThan
                Value = Some (SInt minMinutes)
                Values = None
                ReferenceField = None
            }
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some $"Minimum {minMinutes} minutes lead time required")
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Lead time validation"
                    Description = None
                    Priority = 100
                    Enabled = true
                    Tags = [||]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "timeRange"
                IsRequired = true
                Dependencies = [||]
            }
        static member MinParticipantsRule(id: string, minCount: int, ?message: string) =
            let cond = ComplexCondition.Simple {
                Field = "participants"
                Operator = LessThan
                Value = Some (SInt minCount)
                Values = None
                ReferenceField = None
            }
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some (sprintf "Minimum %d participant(s) required" minCount))
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Minimum participants validation"
                    Description = None
                    Priority = 110
                    Enabled = true
                    Tags = [||]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "participants"
                IsRequired = true
                Dependencies = [||]
            }

        static member MaxParticipantsRule(id: string, maxCount: int, ?message: string) =
            let cond = ComplexCondition.Simple {
                Field = "participants"
                Operator = GreaterThan
                Value = Some (SInt maxCount)
                Values = None
                ReferenceField = None
            }
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some (sprintf "Maximum %d participants allowed" maxCount))
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Maximum participants validation"
                    Description = None
                    Priority = 110
                    Enabled = true
                    Tags = [||]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "participants"
                IsRequired = true
                Dependencies = [||]
            }


        static member ConditionalRule(id: string, conditionField: string, conditionValue: RuleValue, targetField: string, targetValue: RuleValue, operator: ComparisonOperator, ?message: string) =
            let condition = ComplexCondition.Composite (And, [
                ComplexCondition.Simple { Field = conditionField; Operator = Equals; Value = Some conditionValue; Values = None; ReferenceField = None }
                ComplexCondition.Simple { Field = targetField; Operator = operator; Value = Some targetValue; Values = None; ReferenceField = None }
            ])
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some "Conditional rule violation")
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Conditional validation"
                    Description = None
                    Priority = 90
                    Enabled = true
                    Tags = [||]
                    Condition = Some condition
                    Actions = [| action |]
                }
                ValidationScope = "reservation"
                IsRequired = true
                Dependencies = [||]
            }

        static member PricingRule(id: string, conditionField: string, conditionValue: RuleValue, multiplier: decimal) : PricingRule =
            let cond = ComplexCondition.Simple {
                Field = conditionField
                Operator = Equals
                Value = Some conditionValue
                Values = None
                ReferenceField = None
            }
            let action = {
                ActionType = SetProperty
                Severity = Info
                Message = Some ($"Price multiplier: {multiplier}x")
                Parameters = Map.empty
                PropertyUpdates = Some (Map.empty.Add("priceMultiplier", SDecimal multiplier))
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Dynamic pricing"
                    Description = None
                    Priority = 50
                    Enabled = true
                    Tags = [| "pricing" |]
                    Condition = Some cond
                    Actions = [| action |]
                }
                PricingComponent = "multiplier"
                CalculationMethod = "set"
                BaseAmount = None
                Modifiers = Map.empty.Add("multiplier", multiplier)
            }

// ===== COMPREHENSIVE TEST SUITE =====

module ComprehensiveReservationTests =

    let private createRuleSet (validationRules: ValidationRule[]) (businessRulesOpt: BusinessRule[] option) (pricingRulesOpt: PricingRule[] option) : RuleSet =
        {
            ValidationRules = validationRules
            BusinessRules = defaultArg businessRulesOpt [||]
            ConflictRules = [||]
            PricingRules = defaultArg pricingRulesOpt [||]
            ApprovalRules = [||]
            Metadata = Map.empty
        }

    let private createContext() =
        {
            Timestamp = DateTimeOffset.UtcNow
            ExecutedBy = None
            ContextData = Map.empty
            Parameters = Map.empty
            TraceEnabled = false
        }

    let private executeRules (ruleSet: RuleSet) (jsonData: string) =
        let resourceData = TestData.ResourceBuilder()
                            .AddTable("table-1", 2)
                            .AddTable("table-2", 4)
                            .AddTable("table-3", 6)
                            .AddTable("private-dining", 12, isPrivate=true)
                            .AddTable("outdoor-1", 8, isOutdoor=true)
                            .AddTable("accessible-1", 4, isAccessible=true)
                            .AddTotalCapacity(36)
                            .Build()

        let combinedJson =
            let reservationDoc = JsonDocument.Parse(jsonData)
            let reservationObj = reservationDoc.RootElement
            let resourceDoc = JsonDocument.Parse(resourceData)
            let resourceRoot = resourceDoc.RootElement
            let combined = Dictionary<string, JsonElement>()

            // First, copy all reservation properties
            for prop in reservationObj.EnumerateObject() do
                combined.[prop.Name] <- prop.Value
            
            // Then, merge resourceAvailability: custom reservation resources override defaults
            let defaultResources = resourceRoot.GetProperty("resourceAvailability")
            let combinedResources = Dictionary<string, JsonElement>()
            
            // Start with default resources
            for prop in defaultResources.EnumerateObject() do
                combinedResources.[prop.Name] <- prop.Value
            
            // Override with custom resources from reservation if they exist
            if combined.ContainsKey("resourceAvailability") then
                let customResources = JsonDocument.Parse(combined.["resourceAvailability"].GetRawText()).RootElement
                for prop in customResources.EnumerateObject() do
                    combinedResources.[prop.Name] <- prop.Value
            
            combined.["resourceAvailability"] <- JsonSerializer.SerializeToElement(combinedResources)

            JsonSerializer.Serialize(combined)

        let doc = JsonDocument.Parse(combinedJson)
        let ctx = createContext()
        interpretRules ruleSet doc ctx

    // ===== BASIC CAPACITY TESTS =====

    [<Theory>]
    [<InlineData(1, 2, false)>]  // Under capacity
    [<InlineData(2, 2, false)>]  // Exact capacity
    [<InlineData(3, 2, true)>]   // Over capacity
    [<InlineData(0, 2, true)>]   // Invalid participant count
    [<InlineData(-1, 2, true)>]  // Negative participants
    let ``Basic capacity validation`` (participants: int, capacity: int, shouldFail: bool) =
        // Add both capacity validation AND minimum participant validation
        let capacityRule = TestData.RuleBuilder.CapacityRule("CAP-001", "participants", "resourceAvailability.table-1.capacity")
        let minParticipantsRule = TestData.RuleBuilder.MinParticipantsRule("MIN-PART-001", 1, "Minimum 1 participant required")
        let ruleSet = createRuleSet [| capacityRule; minParticipantsRule |] None None

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(participants)
                           .WithCustomField("resourceAvailability", {| ``table-1`` = {| capacity = capacity |} |})
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== LEAD TIME MATRIX TESTS =====

    [<Theory>]
    [<InlineData(false, false, false, false, 30, 60, true)>]   // Regular: 30min < 60min required -> FAIL
    [<InlineData(false, false, false, false, 60, 60, false)>]  // Regular: 60min = 60min required -> PASS
    [<InlineData(false, false, false, false, 90, 60, false)>]  // Regular: 90min > 60min required -> PASS
    [<InlineData(true, false, false, false, 120, 180, true)>]  // Holiday: 120min < 180min required -> FAIL
    [<InlineData(true, false, false, false, 180, 180, false)>] // Holiday: 180min = 180min required -> PASS
    [<InlineData(false, true, false, false, 60, 90, true)>]    // Weekend: 60min < 90min required -> FAIL
    [<InlineData(false, false, true, false, 90, 120, true)>]   // Peak: 90min < 120min required -> FAIL
    [<InlineData(false, false, false, true, 15, 30, true)>]    // VIP: 15min < 30min required -> FAIL
    [<InlineData(false, false, false, true, 30, 30, false)>]   // VIP: 30min = 30min required -> PASS
    let ``Dynamic lead time validation`` (isHoliday: bool, isWeekend: bool, isPeakHour: bool, isVIP: bool, leadMinutes: int, requiredMinutes: int, shouldFail: bool) =
        let rules = [|
            if isHoliday then TestData.RuleBuilder.LeadTimeRule("LEAD-HOL", "leadMinutes", 180, "Holiday reservations require 180min lead time")
            elif isWeekend then TestData.RuleBuilder.LeadTimeRule("LEAD-WKD", "leadMinutes", 90, "Weekend reservations require 90min lead time")
            elif isPeakHour then TestData.RuleBuilder.LeadTimeRule("LEAD-PEAK", "leadMinutes", 120, "Peak hour reservations require 120min lead time")
            elif isVIP then TestData.RuleBuilder.LeadTimeRule("LEAD-VIP", "leadMinutes", 30, "VIP reservations require 30min lead time")
            else TestData.RuleBuilder.LeadTimeRule("LEAD-REG", "leadMinutes", 60, "Regular reservations require 60min lead time")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithLeadTime(leadMinutes)
                           .WithCustomField("isHoliday", isHoliday)
                           .WithCustomField("isWeekend", isWeekend)
                           .WithCustomField("isPeakHour", isPeakHour)
                           .WithCustomField("isVIP", isVIP)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== COMPLEX COMBINATION TESTS =====

    [<Theory>]
    [<InlineData(4, 6, 120, false, false, false)>]   // Valid capacity, valid lead time -> PASS
    [<InlineData(8, 6, 120, false, false, true)>]    // Invalid capacity, valid lead time -> FAIL
    [<InlineData(4, 6, 30, false, false, true)>]     // Valid capacity, invalid lead time -> FAIL
    [<InlineData(8, 6, 30, false, false, true)>]     // Invalid capacity, invalid lead time -> FAIL
    [<InlineData(8, 6, 30, true, false, false)>]     // Invalid normally, but VIP override -> PASS
    [<InlineData(4, 6, 120, false, true, false)>]    // Valid with pricing adjustment -> PASS
    let ``Multi-constraint validation with overrides`` (participants: int, capacity: int, leadMinutes: int, isVIP: bool, hasPricing: bool, shouldFail: bool) =
        let rules = [|
            if not isVIP then
                TestData.RuleBuilder.CapacityRule("CAP-001", "participants", "resourceAvailability.table-2.capacity")
                TestData.RuleBuilder.LeadTimeRule("LEAD-001", "leadMinutes", 60)
        |]

        let pricingRules =
            if hasPricing then [| TestData.RuleBuilder.PricingRule("PRICE-001", "isPeakHour", SBool true, 1.5m) |]
            else [||]

        let ruleSet = createRuleSet rules None (Some pricingRules)

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(participants)
                           .WithLeadTime(leadMinutes)
                           .WithCustomField("isVIP", isVIP)
                           .WithCustomField("isPeakHour", hasPricing)
                           .WithCustomField("resourceAvailability", {| ``table-2`` = {| capacity = capacity |} |})
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== ACCESSIBILITY AND SPECIAL NEEDS =====

    [<Fact>]
    let ``Accessibility requirements with adequate capacity`` () =
        let rules = [|
            TestData.RuleBuilder.CapacityRule("CAP-ACCESS", "participants", "resourceAvailability.accessible-1.capacity", "No accessible tables available")
            TestData.RuleBuilder.ConditionalRule("ACCESS-REQ", "hasAccessibilityNeeds", SBool true, "participants", SInt 4, GreaterThan, "Accessible table capacity exceeded")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(3)
                           .WithAccessibilityNeeds()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.False(result.HasCriticalErrors)

    [<Fact>]
    let ``Accessibility requirements exceeding capacity`` () =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("ACCESS-CAP", "hasAccessibilityNeeds", SBool true, "participants", SInt 4, GreaterThan, "Accessible table capacity exceeded")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(6)
                           .WithAccessibilityNeeds()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.True(result.HasCriticalErrors)

    // ===== SPECIAL EVENT AND AUTHORIZATION =====

    [<Theory>]
    [<InlineData(false, false, false)>] // Regular reservation -> PASS
    [<InlineData(true, false, true)>]   // Special event without auth -> FAIL
    [<InlineData(true, true, false)>]   // Special event with auth -> PASS
    let ``Special event authorization requirements`` (isSpecialEvent: bool, hasAuth: bool, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("EVENT-AUTH", "isSpecialEvent", SBool true, "hasAuthorization", SBool false, Equals, "Special events require authorization")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation =
            let b = TestData.ReservationBuilder()
            let b = if isSpecialEvent then b.AsSpecialEvent(hasAuth) else b.WithCustomField("isSpecialEvent", false).WithCustomField("hasAuthorization", hasAuth)
            b.Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== WALK-IN SCENARIOS =====

    [<Theory>]
    [<InlineData(2, 4, false, false)>]  // Walk-in under capacity -> PASS
    [<InlineData(6, 4, false, true)>]   // Walk-in over capacity -> FAIL
    [<InlineData(2, 4, true, false)>]   // Walk-in VIP under capacity -> PASS
    [<InlineData(6, 4, true, false)>]   // Walk-in VIP over capacity but override -> PASS
    let ``Walk-in reservation handling`` (participants: int, capacity: int, isVIP: bool, shouldFail: bool) =
        let rules = [|
            if not isVIP then
                TestData.RuleBuilder.CapacityRule("CAP-WALKIN", "participants", "resourceAvailability.table-2.capacity")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(participants)
                           .AsWalkIn()
                           .WithCustomField("isVIP", isVIP)
                           .WithCustomField("resourceAvailability", {| ``table-2`` = {| capacity = capacity |} |})
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== ERROR HANDLING TESTS =====

    [<Fact>]
    let ``Handle malformed JSON gracefully`` () =
        let rule = TestData.RuleBuilder.CapacityRule("CAP-001", "participants", "resourceAvailability.table-1.capacity")
        let ruleSet = createRuleSet [| rule |] None None

        let malformedJson = """{ "participants": "invalid", "leadMinutes": }"""

        Assert.ThrowsAny<JsonException>(fun () ->
            let doc = JsonDocument.Parse(malformedJson)
            let ctx = createContext()
            interpretRules ruleSet doc ctx |> ignore
        )

    [<Fact>]
    let ``Handle missing required fields`` () =
        let rule = TestData.RuleBuilder.CapacityRule("CAP-001", "participants", "resourceAvailability.table-1.capacity")
        let ruleSet = createRuleSet [| rule |] None None

        let incompleteJson = """{ "leadMinutes": 120 }"""
        let doc = JsonDocument.Parse(incompleteJson)
        let ctx = createContext()

        let result = interpretRules ruleSet doc ctx
        // Should handle gracefully - exact behavior depends on rule engine implementation
        Assert.NotNull(result)

    [<Fact>]
    let ``Handle rule with non-existent reference field`` () =
        let rule = TestData.RuleBuilder.CapacityRule("CAP-001", "participants", "resourceAvailability.nonexistent.capacity")
        let ruleSet = createRuleSet [| rule |] None None

        let reservation = TestData.ReservationBuilder().Build()

        // Should not throw exception - rule engine should handle missing references gracefully
        let result = executeRules ruleSet reservation
        Assert.NotNull(result)

    // ===== PERFORMANCE TESTS =====

    [<Fact>]
    let ``Performance with large rule set`` () =
        let rules =
            [| 1..100 |]
            |> Array.map (fun i -> TestData.RuleBuilder.LeadTimeRule($"PERF-{i}", "leadMinutes", 60 + i))

        let ruleSet = createRuleSet rules None None
        let reservation = TestData.ReservationBuilder().WithLeadTime(200).Build()

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let result = executeRules ruleSet reservation
        stopwatch.Stop()

        Assert.False(result.HasCriticalErrors)
        Assert.True(stopwatch.ElapsedMilliseconds < 1000) // Should complete within 1 second

    [<Theory>]
    [<InlineData(10)>]
    [<InlineData(50)>]
    [<InlineData(100)>]
    let ``Performance with multiple reservations`` (count: int) =
        let rule = TestData.RuleBuilder.CapacityRule("CAP-001", "participants", "resourceAvailability.table-1.capacity")
        let ruleSet = createRuleSet [| rule |] None None

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        for i in 1..count do
            let reservation = TestData.ReservationBuilder()
                               .WithParticipants(2)
                               .Build()
            let result = executeRules ruleSet reservation
            Assert.False(result.HasCriticalErrors)

        stopwatch.Stop()
        let avgMs = float stopwatch.ElapsedMilliseconds / float count
        Assert.True(avgMs < 50.0) // Average should be under 50ms per reservation

    // ===== INTEGRATION SCENARIO TESTS =====

    [<Fact>]
    let ``Full workflow: Holiday weekend peak hour large group with dietary needs`` () =
        let rules = [|
            TestData.RuleBuilder.CapacityRule("CAP-PRIVATE", "participants", "resourceAvailability.private-dining.capacity")
            TestData.RuleBuilder.LeadTimeRule("LEAD-HOLIDAY", "leadMinutes", 240) // 4 hour minimum for holidays
            TestData.RuleBuilder.ConditionalRule("DIETARY-LEAD", "hasSpecialDietary", SBool true, "leadMinutes", SInt 180, LessThan, "Special dietary needs require 3+ hour notice")
        |]

        let pricingRules = [|
            TestData.RuleBuilder.PricingRule("PRICE-HOLIDAY", "isHoliday", SBool true, 1.8m)
            TestData.RuleBuilder.PricingRule("PRICE-WEEKEND", "isWeekend", SBool true, 1.3m)
            TestData.RuleBuilder.PricingRule("PRICE-PEAK", "isPeakHour", SBool true, 1.5m)
        |]

        let ruleSet = createRuleSet rules None (Some pricingRules)

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(10)
                           .WithLeadTime(300) // 5 hours
                           .AsHoliday()
                           .AsWeekend()
                           .AsPeakHour()
                           .WithSpecialDietary()
                           .Build()

        let result = executeRules ruleSet reservation

        Assert.False(result.HasCriticalErrors) // Should pass all validations
        // Pricing updates are represented as actions; ensure we have at least one pricing action
        let hasPricingAction =
            result.PricingResults
            |> Array.exists (fun r -> r.Matched && r.Actions |> Array.exists (fun a -> a.ActionType = SetProperty))
        Assert.True(hasPricingAction)

    [<Fact>]
    let ``Overbooking scenario with conflict resolution`` () =
        // Fix: Check overbooking permission BEFORE applying capacity limits
        let rules = [|
            // First, check if overbooking is allowed and apply relaxed limits
            TestData.RuleBuilder.ConditionalRule("OVERBOOKING-PERMIT", "allowOverbooking", SBool true, "participants", SInt 40, GreaterThan, "Overbooking limited to 10% above capacity")
            // Then apply normal capacity limits only if overbooking is not allowed
            TestData.RuleBuilder.ConditionalRule("CAP-TOTAL", "allowOverbooking", SBool false, "participants", SInt 36, GreaterThan, "Capacity exceeded")
        |]

        let ruleSet = createRuleSet rules None None

        // Test normal capacity limit
        let normalReservation = TestData.ReservationBuilder()
                                  .WithParticipants(40) // Exceeds normal capacity of 36
                                  .WithCustomField("allowOverbooking", false)
                                  .Build()

        let normalResult = executeRules ruleSet normalReservation
        Assert.True(normalResult.HasCriticalErrors)

        // Test with overbooking allowed
        let overbookedReservation = TestData.ReservationBuilder()
                                     .WithParticipants(38) // Within overbooking limit
                                     .WithCustomField("allowOverbooking", true)
                                     .Build()

        let overbookedResult = executeRules ruleSet overbookedReservation
        Assert.False(overbookedResult.HasCriticalErrors)

    // ===== SECURITY AND VALIDATION TESTS =====

    [<Theory>]
    [<InlineData(-5, true)>]     // Negative participants
    [<InlineData(0, true)>]      // Zero participants
    [<InlineData(1000, true)>]   // Unreasonably large group
    [<InlineData(4, false)>]     // Valid participants
    let ``Input validation for participant count`` (participants: int, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.MinParticipantsRule("VAL-MIN", 1, "Minimum 1 participant required")
            TestData.RuleBuilder.MaxParticipantsRule("VAL-MAX", 50, "Maximum 50 participants allowed")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(participants)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(-60, true)>]    // Negative lead time
    [<InlineData(0, false)>]     // Walk-in (0 lead time)
    [<InlineData(10080, false)>] // 1 week advance (valid)
    let ``Input validation for lead time`` (leadMinutes: int, shouldFail: bool) =
        let rules = [|
            // Only enforce non-negative lead time. 0 is valid (walk-in), large positive is valid.
            TestData.RuleBuilder.LeadTimeRule("VAL-LEAD-NONNEG", "leadMinutes", 0, "Lead time cannot be negative")
        |]
        let ruleSet = createRuleSet rules None None
        let reservation = TestData.ReservationBuilder().WithLeadTime(leadMinutes).Build()
        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)
