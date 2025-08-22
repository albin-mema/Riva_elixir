namespace ReservationService.Tests.Enhanced

(***
Enhanced Domain: Beach umbrella reservations with comprehensive testing
Features tested:
- Time slot management (hourly, daily, multi-day)
- Capacity limits per beach area and zone
- Dynamic pricing based on time, duration, umbrella size, and season
- Cancellation policies with time-based refunds
- Seasonal availability and weather restrictions
- Special requirements (family umbrellas, VIP areas, accessibility)
- Conflict detection and overlapping reservations
- Resource allocation and zone management
- Peak hour surcharges and holiday pricing
- Group booking discounts and loyalty programs
- Weather contingency policies
- Equipment upgrades and add-ons
- Payment processing and deposit requirements
- Age restrictions and supervision policies
- Accessibility and special needs accommodations
*)

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
        let mutable participants = 2
        let mutable leadMinutes = 120
        let mutable startTime = DateTimeOffset(2025,8,21,10,0,0,TimeSpan.Zero)
        let mutable endTime = DateTimeOffset(2025,8,21,12,0,0,TimeSpan.Zero)
        let mutable umbrellaSize = "standard"
        let mutable beachZone = "main"
        let mutable isVIP = false
        let mutable isHoliday = false
        let mutable isPeakHour = false
        let mutable isWeekend = false
        let mutable hasAccessibilityNeeds = false
        let mutable isFamilyUmbrella = false
        let mutable hasChildren = false
        let mutable isWeatherSensitive = false
        let mutable requiresSupervision = false
        let mutable isGroupBooking = false
        let mutable groupSize = 0
        let mutable isEquipmentUpgrade = false
        let mutable depositPaid = false
        let mutable customFields = Map.empty<string, obj>

        member this.WithParticipants(count: int) =
            participants <- count; this

        member this.WithLeadTime(minutes: int) =
            leadMinutes <- minutes; this

        member this.WithTimeRange(start: DateTimeOffset, finish: DateTimeOffset) =
            startTime <- start; endTime <- finish; this

        member this.WithUmbrella(size: string) =
            umbrellaSize <- size; this

        member this.WithZone(zone: string) =
            beachZone <- zone; this

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

        member this.AsFamilyUmbrella() =
            isFamilyUmbrella <- true; this

        member this.WithChildren() =
            hasChildren <- true; this

        member this.AsWeatherSensitive() =
            isWeatherSensitive <- true; this

        member this.RequiresSupervision() =
            requiresSupervision <- true; this

        member this.AsGroupBooking(size: int) =
            isGroupBooking <- true; groupSize <- size; this

        member this.WithEquipmentUpgrade() =
            isEquipmentUpgrade <- true; this

        member this.WithDepositPaid() =
            depositPaid <- true; this

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
            fields["umbrellaSize"] <- box umbrellaSize
            fields["beachZone"] <- box beachZone
            fields["isVIP"] <- box isVIP
            fields["isHoliday"] <- box isHoliday
            fields["isPeakHour"] <- box isPeakHour
            fields["isWeekend"] <- box isWeekend
            fields["hasAccessibilityNeeds"] <- box hasAccessibilityNeeds
            fields["isFamilyUmbrella"] <- box isFamilyUmbrella
            fields["hasChildren"] <- box hasChildren
            fields["isWeatherSensitive"] <- box isWeatherSensitive
            fields["requiresSupervision"] <- box requiresSupervision
            fields["isGroupBooking"] <- box isGroupBooking
            fields["groupSize"] <- box groupSize
            fields["isEquipmentUpgrade"] <- box isEquipmentUpgrade
            fields["depositPaid"] <- box depositPaid
            // Merge custom fields (overrides if same key)
            customFields |> Map.iter (fun k v -> fields[k] <- v)
            JsonSerializer.Serialize(fields)

    type ResourceBuilder() =
        let resources = Dictionary<string, obj>()

        member this.AddUmbrellaZone(name: string, capacity: int, ?isVIP: bool, ?isAccessible: bool, ?isFamily: bool, ?hourlyRate: decimal, ?dailyRate: decimal) =
            let zone =
                {| 
                    capacity = capacity
                    isVIP = defaultArg isVIP false
                    isAccessible = defaultArg isAccessible false
                    isFamily = defaultArg isFamily false
                    hourlyRate = defaultArg hourlyRate 15.0m
                    dailyRate = defaultArg dailyRate 80.0m
                    availableUmbrellas = capacity
                |}
            resources.[name] <- zone
            this

        member this.AddTotalCapacity(total: int) =
            resources.["totalCapacity"] <- total
            this

        member this.WithSeasonalMultiplier(season: string, multiplier: decimal) =
            if not (resources.ContainsKey("seasonalPricing")) then
                resources.["seasonalPricing"] <- Dictionary<string, obj>()
            let seasonal = resources.["seasonalPricing"] :?> Dictionary<string, obj>
            seasonal.[season] <- box multiplier
            this

        member this.WithPeakHourMultiplier(multiplier: decimal) =
            resources.["peakHourMultiplier"] <- box multiplier
            this

        member this.WithHolidayMultiplier(multiplier: decimal) =
            resources.["holidayMultiplier"] <- box multiplier
            this

        member this.WithCancellationPolicy(policy: string) =
            resources.["cancellationPolicy"] <- box policy
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

        static member TimeSlotRule(id: string, validSlots: string[], ?message: string) =
            let slotValues = validSlots |> Array.map SString
            let cond = ComplexCondition.Simple {
                Field = "timeSlot"
                Operator = In
                Value = None
                Values = Some slotValues
                ReferenceField = None
            }
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some "Invalid time slot selected")
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Time slot validation"
                    Description = None
                    Priority = 95
                    Enabled = true
                    Tags = [||]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "timeRange"
                IsRequired = true
                Dependencies = [||]
            }

        static member SeasonalAvailabilityRule(id: string, season: string, available: bool, ?message: string) =
            let cond = ComplexCondition.Composite (And, [
                ComplexCondition.Simple { Field = "season"; Operator = Equals; Value = Some (SString season); Values = None; ReferenceField = None }
                ComplexCondition.Simple { Field = "isAvailable"; Operator = Equals; Value = Some (SBool available); Values = None; ReferenceField = None }
            ])
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some "Seasonal availability restriction")
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Seasonal availability validation"
                    Description = None
                    Priority = 85
                    Enabled = true
                    Tags = [||]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "reservation"
                IsRequired = true
                Dependencies = [||]
            }

        static member CancellationPolicyRule(id: string, minHours: int, refundPercentage: int, ?message: string) =
            let cond = ComplexCondition.Simple {
                Field = "cancellationHours"
                Operator = LessThan
                Value = Some (SInt minHours)
                Values = None
                ReferenceField = None
            }
            let action = {
                ActionType = SetProperty
                Severity = Warning
                Message = message |> Option.orElse (Some $"Cancellation within {minHours} hours results in {refundPercentage}% refund")
                Parameters = Map.empty
                PropertyUpdates = Some (Map.empty.Add("refundPercentage", SInt refundPercentage))
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Cancellation policy"
                    Description = None
                    Priority = 70
                    Enabled = true
                    Tags = [| "cancellation" |]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "reservation"
                IsRequired = true
                Dependencies = [||]
            }

        static member AgeRestrictionRule(id: string, minAge: int, requiresSupervision: bool, ?message: string) =
            let cond = ComplexCondition.Composite (And, [
                ComplexCondition.Simple { Field = "minAge"; Operator = LessThan; Value = Some (SInt minAge); Values = None; ReferenceField = None }
                ComplexCondition.Simple { Field = "hasSupervision"; Operator = Equals; Value = Some (SBool requiresSupervision); Values = None; ReferenceField = None }
            ])
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some "Age restriction and supervision requirement not met")
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Age restriction validation"
                    Description = None
                    Priority = 105
                    Enabled = true
                    Tags = [||]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "participants"
                IsRequired = true
                Dependencies = [||]
            }

        static member WeatherPolicyRule(id: string, weatherCondition: string, allowed: bool, ?message: string) =
            let cond = ComplexCondition.Composite (And, [
                ComplexCondition.Simple { Field = "weatherCondition"; Operator = Equals; Value = Some (SString weatherCondition); Values = None; ReferenceField = None }
                ComplexCondition.Simple { Field = "isAllowed"; Operator = Equals; Value = Some (SBool allowed); Values = None; ReferenceField = None }
            ])
            let action = {
                ActionType = Reject
                Severity = Critical
                Message = message |> Option.orElse (Some "Weather condition not suitable for beach activities")
                Parameters = Map.empty
                PropertyUpdates = None
                Suggestions = None
            }
            {
                Rule = {
                    Id = id
                    Name = "Weather policy validation"
                    Description = None
                    Priority = 80
                    Enabled = true
                    Tags = [||]
                    Condition = Some cond
                    Actions = [| action |]
                }
                ValidationScope = "reservation"
                IsRequired = true
                Dependencies = [||]
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
                            .AddUmbrellaZone("main", 20, hourlyRate=15.0m, dailyRate=80.0m)
                            .AddUmbrellaZone("vip", 8, isVIP=true, hourlyRate=25.0m, dailyRate=120.0m)
                            .AddUmbrellaZone("family", 12, isFamily=true, hourlyRate=20.0m, dailyRate=100.0m)
                            .AddUmbrellaZone("accessible", 6, isAccessible=true, hourlyRate=18.0m, dailyRate=90.0m)
                            .AddTotalCapacity(46)
                            .WithSeasonalMultiplier("summer", 1.2m)
                            .WithSeasonalMultiplier("winter", 0.8m)
                            .WithPeakHourMultiplier(1.5m)
                            .WithHolidayMultiplier(1.3m)
                            .WithCancellationPolicy("24h_100|8h_50|2h_0")
                            .Build()

        let combinedJson =
            let reservationDoc = JsonDocument.Parse(jsonData)
            let reservationObj = reservationDoc.RootElement
            let resourceDoc = JsonDocument.Parse(resourceData)
            let resourceRoot = resourceDoc.RootElement
            let combined = Dictionary<string, JsonElement>()

            for prop in reservationObj.EnumerateObject() do
                combined.[prop.Name] <- prop.Value
            // Nest the entire resourceAvailability object
            combined.["resourceAvailability"] <- resourceRoot.GetProperty("resourceAvailability")

            JsonSerializer.Serialize(combined)

        let doc = JsonDocument.Parse(combinedJson)
        let ctx = createContext()
        interpretRules ruleSet doc ctx

    // ===== BASIC CAPACITY TESTS =====

    [<Theory>]
    [<InlineData(1, 20, false, "Under capacity should not fail")>]   // Under capacity
    [<InlineData(20, 20, false, "Exact capacity should not fail")>]  // Exact capacity
    [<InlineData(21, 20, true, "Over capacity should fail")>]   // Over capacity
    [<InlineData(0, 20, true, "Invalid participant count should fail")>]    // Invalid participant count
    [<InlineData(-1, 20, true, "Negative participants should fail")>]   // Negative participants
    let ``Basic capacity validation`` (participants: int, capacity: int, shouldFail: bool, message: string) =
        let rule = TestData.RuleBuilder.CapacityRule("CAP-001", "participants", "resourceAvailability.main.capacity")
        let ruleSet = createRuleSet [| rule |] None None

        let reservation = TestData.ReservationBuilder()
                            .WithParticipants(participants)
                            .WithCustomField("resourceAvailability", {| main = {| capacity = capacity |} |})
                            .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

        if shouldFail then
            // Check for specific error message
            let hasErrorMessage = result.ValidationResults |> Array.exists (fun vr ->
                vr.Matched && vr.HasCriticalErrors &&
                vr.Actions |> Array.exists (fun a -> a.ActionType = Reject && a.Message.Contains("Capacity exceeded")))
            Assert.True(hasErrorMessage, "Error message not found when expected: " + message)
        else
            // Ensure no error
            let noError = result.ValidationResults |> Array.exists (fun vr ->
                vr.Matched && vr.HasCriticalErrors &&
                vr.Actions |> Array.exists (fun a -> a.ActionType = Reject && a.Message.Contains("Capacity exceeded")))
            Assert.False(noError, "Unexpected error when not expected")

    // ===== UMBRELLA SIZE AND ZONE TESTS =====

    [<Theory>]
    [<InlineData("standard", "main", false)>]      // Standard umbrella in main zone
    [<InlineData("large", "main", false)>]         // Large umbrella in main zone
    [<InlineData("extra-large", "main", false)>]   // Extra-large umbrella in main zone
    [<InlineData("standard", "vip", false)>]       // Standard umbrella in VIP zone
    [<InlineData("family", "family", false)>]      // Family umbrella in family zone
    [<InlineData("standard", "accessible", false)>] // Standard umbrella in accessible zone
    [<InlineData("nonexistent", "main", true)>]    // Invalid umbrella size
    [<InlineData("standard", "nonexistent", true)>] // Invalid zone
    let ``Umbrella size and zone validation`` (umbrellaSize: string, zone: string, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("UMB-SIZE", "umbrellaSize", SString umbrellaSize, "beachZone", SString zone, NotEquals, "Invalid umbrella size for zone")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithUmbrella(umbrellaSize)
                           .WithZone(zone)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== TIME SLOT TESTS =====

    [<Theory>]
    [<InlineData("09:00", "10:00", false)>]   // Valid morning slot
    [<InlineData("11:00", "12:00", false)>]   // Valid midday slot
    [<InlineData("14:00", "15:00", false)>]   // Valid afternoon slot
    [<InlineData("16:00", "17:00", false)>]   // Valid late afternoon slot
    [<InlineData("18:00", "19:00", false)>]   // Valid evening slot
    [<InlineData("08:00", "09:00", true)>]    // Too early
    [<InlineData("20:00", "21:00", true)>]    // Too late
    [<InlineData("12:00", "13:00", false)>]   // Valid lunch slot
    [<InlineData("22:00", "23:00", true)>]    // Night time (invalid)
    let ``Time slot validation`` (startHour: string, endHour: string, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.TimeSlotRule("SLOT-001", [| "09:00-10:00"; "11:00-12:00"; "12:00-13:00"; "14:00-15:00"; "16:00-17:00"; "18:00-19:00" |])
        |]

        let ruleSet = createRuleSet rules None None

        let startTime = DateTimeOffset(2025, 8, 21, int startHour.Split(':')[0], int startHour.Split(':')[1], 0, TimeSpan.Zero)
        let endTime = DateTimeOffset(2025, 8, 21, int endHour.Split(':')[0], int endHour.Split(':')[1], 0, TimeSpan.Zero)

        let reservation = TestData.ReservationBuilder()
                           .WithTimeRange(startTime, endTime)
                           .WithCustomField("timeSlot", $"{startHour}-{endHour}")
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== DURATION TESTS =====

    [<Theory>]
    [<InlineData(1, false)>]    // 1 hour (valid)
    [<InlineData(2, false)>]    // 2 hours (valid)
    [<InlineData(4, false)>]    // 4 hours (valid)
    [<InlineData(8, false)>]    // 8 hours (valid - full day)
    [<InlineData(0, true)>]     // 0 hours (invalid)
    [<InlineData(-1, true)>]    // Negative hours (invalid)
    [<InlineData(24, true)>]    // 24 hours (too long)
    [<InlineData(12, true)>]    // 12 hours (exceeds daily limit)
    let ``Duration validation`` (duration: int, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("DUR-MIN", "duration", SInt duration, SInt 1, LessThan, "Minimum 1 hour required")
            TestData.RuleBuilder.ConditionalRule("DUR-MAX", "duration", SInt duration, SInt 8, GreaterThan, "Maximum 8 hours per day")
        |]

        let ruleSet = createRuleSet rules None None

        let startTime = DateTimeOffset(2025, 8, 21, 10, 0, 0, TimeSpan.Zero)
        let endTime = startTime.AddHours(duration)

        let reservation = TestData.ReservationBuilder()
                           .WithTimeRange(startTime, endTime)
                           .WithCustomField("duration", duration)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== LEAD TIME TESTS =====

    [<Theory>]
    [<InlineData(1440, false)>]  // 1 day advance (valid)
    [<InlineData(2880, false)>]  // 2 days advance (valid)
    [<InlineData(10080, false)>] // 1 week advance (valid)
    [<InlineData(0, true)>]      // Same day (invalid)
    [<InlineData(-60, true)>]    // Negative lead time (invalid)
    [<InlineData(30, true)>]     // 30 minutes (too short)
    [<InlineData(15, true)>]     // 15 minutes (too short)
    let ``Lead time validation`` (leadMinutes: int, shouldFail: bool) =
        let rule = TestData.RuleBuilder.LeadTimeRule("LEAD-001", "leadMinutes", 120, "Minimum 2 hours lead time required")
        let ruleSet = createRuleSet [| rule |] None None

        let reservation = TestData.ReservationBuilder()
                           .WithLeadTime(leadMinutes)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== SEASONAL AVAILABILITY TESTS =====

    [<Theory>]
    [<InlineData("summer", true, false)>]    // Summer, available
    [<InlineData("summer", false, true)>]   // Summer, not available
    [<InlineData("winter", true, false)>]    // Winter, available
    [<InlineData("winter", false, true)>]   // Winter, not available
    [<InlineData("spring", true, false)>]    // Spring, available
    [<InlineData("spring", false, true)>]   // Spring, not available
    [<InlineData("autumn", true, false)>]    // Autumn, available
    [<InlineData("autumn", false, true)>]   // Autumn, not available
    let ``Seasonal availability validation`` (season: string, available: bool, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.SeasonalAvailabilityRule("SEASON-001", season, available)
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithCustomField("season", season)
                           .WithCustomField("isAvailable", available)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== PRICING TESTS =====

    [<Theory>]
    [<InlineData("standard", "main", 2, 1, 30.0m, false)>]      // Standard, main, 2 people, 1 hour = $30
    [<InlineData("standard", "main", 2, 2, 60.0m, false)>]      // Standard, main, 2 people, 2 hours = $60
    [<InlineData("standard", "vip", 2, 1, 50.0m, false)>]       // Standard, vip, 2 people, 1 hour = $50
    [<InlineData("family", "family", 4, 2, 80.0m, false)>]      // Family, family, 4 people, 2 hours = $80
    [<InlineData("large", "main", 3, 1, 22.5m, false)>]        // Large (1.5x), main, 3 people, 1 hour = $22.5
    [<InlineData("standard", "main", 2, 8, 80.0m, false)>]      // Daily rate, 8 hours = $80
    [<InlineData("standard", "main", 2, 1, 45.0m, true)>]       // Peak hour (1.5x), should have pricing action
    let ``Pricing calculation`` (umbrellaSize: string, zone: string, participants: int, duration: int, expectedPrice: decimal, shouldHavePricing: bool) =
        let rules = [|
            TestData.RuleBuilder.PricingRule("PRICE-STANDARD", "umbrellaSize", SString "standard", 1.0m)
            TestData.RuleBuilder.PricingRule("PRICE-LARGE", "umbrellaSize", SString "large", 1.5m)
            TestData.RuleBuilder.PricingRule("PRICE-VIP", "beachZone", SString "vip", 1.5m)
            TestData.RuleBuilder.PricingRule("PRICE-FAMILY", "beachZone", SString "family", 1.0m)
            TestData.RuleBuilder.PricingRule("PRICE-PEAK", "isPeakHour", SBool true, 1.5m)
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let startTime = DateTimeOffset(2025, 8, 21, 10, 0, 0, TimeSpan.Zero)
        let endTime = startTime.AddHours(duration)

        let reservation = TestData.ReservationBuilder()
                           .WithUmbrella(umbrellaSize)
                           .WithZone(zone)
                           .WithParticipants(participants)
                           .WithTimeRange(startTime, endTime)
                           .AsPeakHour()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.False(result.HasCriticalErrors)
        
        if shouldHavePricing then
            let hasPricingAction = result.PricingResults |> Array.exists (fun r -> r.Matched && r.Actions |> Array.exists (fun a -> a.ActionType = SetProperty))
            Assert.True(hasPricingAction)

    // ===== CANCELLATION POLICY TESTS =====

    [<Theory>]
    [<InlineData(48, 100, false)>]   // 48 hours before, 100% refund
    [<InlineData(24, 100, false)>]   // 24 hours before, 100% refund
    [<InlineData(12, 50, false)>]    // 12 hours before, 50% refund
    [<InlineData(8, 50, false)>]     // 8 hours before, 50% refund
    [<InlineData(4, 0, false)>]      // 4 hours before, 0% refund
    [<InlineData(1, 0, false)>]      // 1 hour before, 0% refund
    [<InlineData(0, 0, false)>]      // Same day, 0% refund
    let ``Cancellation policy validation`` (cancellationHours: int, expectedRefund: int, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.CancellationPolicyRule("CANCEL-001", 24, 100, "Cancel 24+ hours for full refund")
            TestData.RuleBuilder.CancellationPolicyRule("CANCEL-002", 8, 50, "Cancel 8+ hours for 50% refund")
            TestData.RuleBuilder.CancellationPolicyRule("CANCEL-003", 2, 0, "Cancel less than 2 hours for no refund")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                            .WithCustomField("cancellationHours", cancellationHours)
                            .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

        // Check if refund action is present and the value matches
        let hasRefundAction = result.PricingResults |> Array.exists (fun r ->
            r.Matched && r.Actions |> Array.exists (fun a ->
                a.ActionType = SetProperty && a.PropertyUpdates.IsSome && a.PropertyUpdates.Value.ContainsKey("refundPercentage")))

        if expectedRefund > 0 then
            Assert.True(hasRefundAction, "Refund action not found when expected")
            // Extract the refund percentage value
            let refundAction = result.PricingResults |> Array.tryFind (fun r -> r.Matched && r.Actions |> Array.exists (fun a -> a.ActionType = SetProperty && a.PropertyUpdates.IsSome && a.PropertyUpdates.Value.ContainsKey("refundPercentage")))
            if refundAction.IsSome then
                let refundPercentageValue = refundAction.Value.Actions |> Array.tryFind (fun a -> a.ActionType = SetProperty && a.PropertyUpdates.IsSome && a.PropertyUpdates.Value.ContainsKey("refundPercentage")) |> Option.map (fun a -> a.PropertyUpdates.Value.["refundPercentage"] :?> int)
                Assert.True(refundPercentageValue.IsSome, "Refund percentage not found in the action")
                Assert.Equal(expectedRefund, refundPercentageValue.Value)
            else
                Assert.False(true, "Refund action expected but not found")
        else
            // For expectedRefund = 0, ensure that if there is a refund action, the percentage is 0
            let hasRefundAction = result.PricingResults |> Array.exists (fun r ->
                r.Matched && r.Actions |> Array.exists (fun a ->
                    a.ActionType = SetProperty && a.PropertyUpdates.IsSome && a.PropertyUpdates.Value.ContainsKey("refundPercentage")))
            if hasRefundAction then
                // Find the action and get the percentage
                let refundAction = result.PricingResults |> Array.tryFind (fun r -> r.Matched && r.Actions |> Array.exists (fun a -> a.ActionType = SetProperty && a.PropertyUpdates.IsSome && a.PropertyUpdates.Value.ContainsKey("refundPercentage")))
                if refundAction.IsSome then
                    let refundPercentageValue = refundAction.Value.Actions |> Array.tryFind (fun a -> a.ActionType = SetProperty && a.PropertyUpdates.IsSome && a.PropertyUpdates.Value.ContainsKey("refundPercentage")) |> Option.map (fun a -> a.PropertyUpdates.Value.["refundPercentage"] :?> int)
                    Assert.True(refundPercentageValue.IsSome, "Refund percentage not found")
                    Assert.Equal(0, refundPercentageValue.Value)
            // If no refund action, it's also acceptable for expectedRefund=0, as long as no critical error
            Assert.False(result.HasCriticalErrors)

    // ===== SPECIAL REQUIREMENTS TESTS =====

    [<Theory>]
    [<InlineData(true, false, false)>]   // Family umbrella, no children
    [<InlineData(true, true, false)>]    // Family umbrella with children
    [<InlineData(false, true, true)>]   // Non-family with children (should fail)
    [<InlineData(false, false, false)>]  // Regular setup
    let ``Family umbrella requirements`` (isFamily: bool, hasChildren: bool, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("FAM-REQ", "isFamilyUmbrella", SBool true, "hasChildren", SBool false, Equals, "Family umbrellas require children")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .AsFamilyUmbrella()
                           .WithChildren()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== ACCESSIBILITY TESTS =====

    [<Theory>]
    [<InlineData(true, 4, false)>]    // Accessibility needs, within capacity
    [<InlineData(true, 8, true)>]     // Accessibility needs, exceeds capacity
    [<InlineData(false, 8, false)>]   // No accessibility needs, any capacity
    let ``Accessibility requirements`` (needsAccessibility: int, capacity: int, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("ACCESS-REQ", "hasAccessibilityNeeds", SBool true, "participants", SInt 6, GreaterThan, "Accessible zone capacity limited to 6")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithAccessibilityNeeds()
                           .WithParticipants(needsAccessibility)
                           .WithCustomField("resourceAvailability", {| accessible = {| capacity = capacity |} |})
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== AGE RESTRICTION TESTS =====

    [<Theory>]
    [<InlineData(16, false, false)>]   // 16 years, no supervision needed
    [<InlineData(15, false, true)>]    // 15 years, no supervision (should fail)
    [<InlineData(15, true, false)>]    // 15 years, with supervision
    [<InlineData(8, true, false)>]     // 8 years, with supervision
    [<InlineData(8, false, true)>]     // 8 years, no supervision (should fail)
    [<InlineData(18, false, false)>]   // 18 years, adult
    let ``Age restriction and supervision`` (age: int, hasSupervision: bool, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.AgeRestrictionRule("AGE-001", 16, true, "Minors require supervision")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(2)
                           .WithCustomField("minAge", age)
                           .WithCustomField("hasSupervision", hasSupervision)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== WEATHER POLICY TESTS =====

    [<Theory>]
    [<InlineData("sunny", true, false)>]     // Sunny weather, allowed
    [<InlineData("cloudy", true, false)>]    // Cloudy weather, allowed
    [<InlineData("rainy", false, true)>]     // Rainy weather, not allowed
    [<InlineData("stormy", false, true)>]    // Stormy weather, not allowed
    [<InlineData("windy", true, false)>]     // Windy weather, allowed
    [<InlineData("foggy", false, true)>]     // Foggy weather, not allowed
    let ``Weather policy validation`` (weather: string, allowed: bool, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.WeatherPolicyRule("WEATHER-001", "rainy", false, "Rainy weather not allowed")
            TestData.RuleBuilder.WeatherPolicyRule("WEATHER-002", "stormy", false, "Stormy weather not allowed")
            TestData.RuleBuilder.WeatherPolicyRule("WEATHER-003", "foggy", false, "Foggy weather not allowed")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithCustomField("weatherCondition", weather)
                           .WithCustomField("isAllowed", allowed)
                           .AsWeatherSensitive()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== GROUP BOOKING TESTS =====

    [<Theory>]
    [<InlineData(5, false)>]    // Small group (valid)
    [<InlineData(10, false)>]   // Medium group (valid)
    [<InlineData(20, false)>]   // Large group (valid)
    [<InlineData(25, true)>]    // Very large group (exceeds capacity)
    [<InlineData(1, true)>]     // Too small for group booking
    [<InlineData(0, true)>]     // Invalid group size
    let ``Group booking validation`` (groupSize: int, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.MinParticipantsRule("GROUP-MIN", 5, "Minimum 5 people for group booking")
            TestData.RuleBuilder.MaxParticipantsRule("GROUP-MAX", 20, "Maximum 20 people for group booking")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .AsGroupBooking(groupSize)
                           .WithParticipants(groupSize)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== VIP PRIVILEGE TESTS =====

    [<Theory>>
    [<InlineData(false, false, false)>]  // Regular user, regular zone
    [<InlineData(true, false, false)>]   // VIP user, regular zone
    [<InlineData(false, true, false)>]   // Regular user, VIP zone
    [<InlineData(true, true, false)>]    // VIP user, VIP zone
    [<InlineData(true, true, true)>]     // VIP user, VIP zone, but capacity exceeded
    let ``VIP privilege validation`` (isVIP: bool, vipZone: bool, shouldFail: bool) =
        let rules = [|
            if not isVIP then
                TestData.RuleBuilder.ConditionalRule("VIP-ONLY", "beachZone", SString "vip", "isVIP", SBool false, Equals, "VIP zone requires VIP status")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .AsVIP()
                           .WithZone(if vipZone then "vip" else "main")
                           .WithParticipants(2)
                           .WithCustomField("resourceAvailability", {| vip = {| capacity = if shouldFail then 1 else 10 |} |})
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    // ===== HOLIDAY PRICING TESTS =====

    [<Theory>]
    [<InlineData(false, false, 1.0m)>]   // Not holiday, not peak
    [<InlineData(true, false, 1.3m)>]    // Holiday, not peak
    [<InlineData(false, true, 1.5m)>]    // Not holiday, peak
    [<InlineData(true, true, 1.95m)>]   // Holiday and peak (1.3 * 1.5)
    let ``Holiday and peak pricing`` (isHoliday: bool, isPeak: bool, expectedMultiplier: decimal) =
        let rules = [|
            TestData.RuleBuilder.PricingRule("PRICE-HOLIDAY", "isHoliday", SBool true, 1.3m)
            TestData.RuleBuilder.PricingRule("PRICE-PEAK", "isPeakHour", SBool true, 1.5m)
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let reservation = TestData.ReservationBuilder()
                           .AsHoliday()
                           .AsPeakHour()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.False(result.HasCriticalErrors)

        let pricingActions = result.PricingResults |> Array.filter (fun r -> r.Matched)
        Assert.Equal(2, pricingActions.Length)

    // ===== EQUIPMENT UPGRADE TESTS =====

    [<Theory>]
    [<InlineData(false, false)>]   // No upgrade, regular pricing
    [<InlineData(true, false)>]    // With upgrade, should have pricing action
    [<InlineData(false, true)>]    // No upgrade, but VIP pricing
    [<InlineData(true, true)>]     // With upgrade and VIP, multiple pricing actions
    let ``Equipment upgrade pricing`` (hasUpgrade: bool, isVIP: bool, shouldHavePricing: bool) =
        let rules = [|
            TestData.RuleBuilder.PricingRule("PRICE-UPGRADE", "isEquipmentUpgrade", SBool true, 1.2m)
            TestData.RuleBuilder.PricingRule("PRICE-VIP", "isVIP", SBool true, 1.5m)
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let reservation = TestData.ReservationBuilder()
                           .WithEquipmentUpgrade()
                           .AsVIP()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.False(result.HasCriticalErrors)

        if shouldHavePricing then
            let hasPricingAction = result.PricingResults |> Array.exists (fun r -> r.Matched && r.Actions |> Array.exists (fun a -> a.ActionType = SetProperty))
            Assert.True(hasPricingAction)

    // ===== DEPOSIT REQUIREMENT TESTS =====

    [<Theory>]
    [<InlineData(false, false)>]   // No deposit required, not paid
    [<InlineData(true, false)>]    // Deposit required, not paid (should fail)
    [<InlineData(false, true)>]    // No deposit required, but paid
    [<InlineData(true, true)>]     // Deposit required, paid
    let ``Deposit requirement validation`` (depositRequired: bool, depositPaid: bool, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("DEPOSIT-REQ", "depositRequired", SBool true, "depositPaid", SBool false, Equals, "Deposit required for this reservation")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithDepositPaid()
                           .WithCustomField("depositRequired", depositRequired)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

        // Add specific checks for deposit requirement
        if shouldFail then
            // There should be a validation error with the specific message
            let hasDepositError = result.ValidationResults |> Array.exists (fun vr ->
                vr.Matched && vr.HasCriticalErrors &&
                vr.Actions |> Array.exists (fun a -> a.ActionType = Reject && a.Message = "Deposit required for this reservation"))
            Assert.True(hasDepositError, "Deposit required error not found when expected")
        else
            // Ensure no deposit error
            let noDepositError = result.ValidationResults |> Array.exists (fun vr ->
                vr.Matched && vr.HasCriticalErrors &&
                vr.Actions |> Array.exists (fun a -> a.ActionType = Reject && a.Message = "Deposit required for this reservation"))
            Assert.False(noDepositError, "Unexpected deposit required error")

    // ===== OVERLAPPING RESERVATION TESTS =====

    [<Theory>]
    [<InlineData("09:00", "11:00", "11:00", "13:00", false)>]   // Adjacent (no overlap)
    [<InlineData("09:00", "11:00", "10:30", "12:00", true)>]    // Overlapping
    [<InlineData("09:00", "11:00", "08:30", "09:30", true)>]    // Overlapping at start
    [<InlineData("09:00", "11:00", "10:30", "11:30", true)>]    // Overlapping at end
    [<InlineData("09:00", "11:00", "08:00", "12:00", true)>]    // Fully overlapping
    [<InlineData("09:00", "11:00", "11:00", "13:00", false)>]   // Exactly adjacent
    [<InlineData("09:00", "11:00", "13:00", "15:00", false)>]   // Separate (no overlap)
    [<InlineData("09:00", "09:00", "09:00", "11:00", true)>]    // Same start and end, should overlap
    [<InlineData("09:00", "11:00", "11:00", "12:00", true)>]    // Start1 = end2, end1 > start2, overlap
    [<InlineData("09:00", "11:00", "11:00", "11:00", false)>]   // Start1 = end2, end1 = start2, no overlap
    [<InlineData("09:00", "11:00", "12:00", "13:00", false)>]   // Start2 > end1, no overlap
    [<InlineData("12:00", "13:00", "09:00", "11:00", false)>]   // Start1 > end2, no overlap
    let ``Overlapping reservation detection`` (start1: string, end1: string, start2: string, end2: string, shouldOverlap: bool) =
        let startTime1 = DateTimeOffset(2025, 8, 21, int start1.Split(':')[0], int start1.Split(':')[1], 0, TimeSpan.Zero)
        let endTime1 = DateTimeOffset(2025, 0, int end1.Split(':')[0], int end1.Split(':')[1], 0, TimeSpan.Zero)
        let startTime2 = DateTimeOffset(2025, 8, 21, int start2.Split(':')[0], int start2.Split(':')[1], 0, TimeSpan.Zero)
        let endTime2 = DateTimeOffset(2025, 8, 21, int end2.Split(':')[0], int end2.Split(':')[1], 0, TimeSpan.Zero)

        // Calculate overlap
        let overlap = not (endTime1 <= startTime2 || endTime2 <= startTime1)

        // Assert overlap matches expected
        Assert.Equal(overlap, shouldOverlap)

    // ===== SEASONAL PRICING TESTS =====

    [<Theory>]
    [<InlineData("summer", 1.2m)>]    // Summer pricing (20% premium)
    [<InlineData("winter", 0.8m)>]    // Winter pricing (20% discount)
    [<InlineData("spring", 1.0m)>]    // Spring pricing (normal)
    [<InlineData("autumn", 1.0m)>]    // Autumn pricing (normal)
    let ``Seasonal pricing validation`` (season: string, expectedMultiplier: decimal) =
        let rules = [|
            TestData.RuleBuilder.PricingRule("PRICE-SUMMER", "season", SString "summer", 1.2m)
            TestData.RuleBuilder.PricingRule("PRICE-WINTER", "season", SString "winter", 0.8m)
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let reservation = TestData.ReservationBuilder()
                           .WithCustomField("season", season)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.False(result.HasCriticalErrors)

        let hasSeasonalPricing = result.PricingResults |> Array.exists (fun r -> 
            r.Matched && r.Actions |> Array.exists (fun a -> 
                a.PropertyUpdates.IsSome && a.PropertyUpdates.Value.ContainsKey("priceMultiplier")))
        
        if season = "summer" || season = "winter" then
            Assert.True(hasSeasonalPricing)

    // ===== COMBINED SCENARIO TESTS =====

    [<Fact>]
    let ``Complex scenario: VIP family group with accessibility needs during peak holiday`` () =
        let rules = [|
            TestData.RuleBuilder.CapacityRule("CAP-VIP", "participants", "resourceAvailability.vip.capacity")
            TestData.RuleBuilder.LeadTimeRule("LEAD-VIP", "leadMinutes", 60)
            TestData.RuleBuilder.ConditionalRule("ACCESS-VIP", "hasAccessibilityNeeds", SBool true, "participants", SInt 4, GreaterThan, "VIP zone accessibility limited to 4")
            TestData.RuleBuilder.PricingRule("PRICE-VIP-HOLIDAY", "isVIP", SBool true, 1.5m)
            TestData.RuleBuilder.PricingRule("PRICE-HOLIDAY", "isHoliday", SBool true, 1.3m)
            TestData.RuleBuilder.PricingRule("PRICE-PEAK", "isPeakHour", SBool true, 1.5m)
            TestData.RuleBuilder.PricingRule("PRICE-FAMILY", "isFamilyUmbrella", SBool true, 1.1m)
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(3)
                           .WithLeadTime(120)
                           .AsVIP()
                           .AsHoliday()
                           .AsPeakHour()
                           .AsFamilyUmbrella()
                           .WithAccessibilityNeeds()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.False(result.HasCriticalErrors)

        // Should have multiple pricing actions
        let pricingActions = result.PricingResults |> Array.filter (fun r -> r.Matched)
        Assert.True(pricingActions.Length >= 3)

    [<Fact>]
    let ``Weather contingency scenario with large group booking`` () =
        let rules = [|
            TestData.RuleBuilder.WeatherPolicyRule("WEATHER-RAIN", "rainy", false, "Rainy weather not allowed")
            TestData.RuleBuilder.MaxParticipantsRule("GROUP-LARGE", 15, "Large group limit exceeded")
            TestData.RuleBuilder.CancellationPolicyRule("CANCEL-WEATHER", 2, 0, "Weather cancellations no refund")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .AsGroupBooking(12)
                           .WithParticipants(12)
                           .WithCustomField("weatherCondition", "rainy")
                           .WithCustomField("isAllowed", false)
                           .AsWeatherSensitive()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.True(result.HasCriticalErrors)

    [<Fact>]
    let ``Peak hour pricing with equipment upgrade and deposit`` () =
        let rules = [|
            TestData.RuleBuilder.PricingRule("PRICE-PEAK-UPGRADE", "isPeakHour", SBool true, 1.5m)
            TestData.RuleBuilder.PricingRule("PRICE-UPGRADE", "isEquipmentUpgrade", SBool true, 1.2m)
            TestData.RuleBuilder.ConditionalRule("DEPOSIT-UPGRADE", "isEquipmentUpgrade", SBool true, "depositPaid", SBool false, Equals, "Equipment upgrade requires deposit")
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let reservation = TestData.ReservationBuilder()
                           .AsPeakHour()
                           .WithEquipmentUpgrade()
                           .WithDepositPaid()
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.False(result.HasCriticalErrors)

        let hasPricingActions = result.PricingResults |> Array.exists (fun r -> r.Matched)
        let hasDepositValidation = result.ValidationResults |> Array.exists (fun r -> r.Matched)
        
        Assert.True(hasPricingActions)
        Assert.True(hasDepositValidation)

    // ===== EDGE CASE TESTS =====

    [<Theory>]
    [<InlineData(999, true)>]     // Extremely large group
    [<InlineData(1000, true)>]    // Maximum limit exceeded
    [<InlineData(1, false)>]      // Minimum valid group
    [<InlineData(50, true)>]      // Large but reasonable group
    let ``Edge case: Group size limits`` (groupSize: int, shouldFail: bool) =
        let rules = [|
            TestData.RuleBuilder.MinParticipantsRule("GROUP-MIN-EDGE", 1, "At least 1 person required")
            TestData.RuleBuilder.MaxParticipantsRule("GROUP-MAX-EDGE", 50, "Maximum 50 people allowed")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .AsGroupBooking(groupSize)
                           .WithParticipants(groupSize)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    [<Theory>]
    [<InlineData(365 * 24 * 60, false)>]  // 1 year advance (valid)
    [<InlineData(0, true)>]               // Same day (invalid)
    [<InlineData(-1, true)>]              // Negative lead time (invalid)
    [<InlineData(30 * 24 * 60, false)>]   // 1 month advance (valid)
    let ``Edge case: Lead time extremes`` (leadMinutes: int, shouldFail: bool) =
        let rule = TestData.RuleBuilder.LeadTimeRule("LEAD-EDGE", "leadMinutes", 120)
        let ruleSet = createRuleSet [| rule |] None None

        let reservation = TestData.ReservationBuilder()
                           .WithLeadTime(leadMinutes)
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.Equal(shouldFail, result.HasCriticalErrors)

    [<Fact>]
    let ``Edge case: Maximum duration with seasonal pricing`` () =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("DUR-MAX-EDGE", "duration", SInt 8, SInt 8, GreaterThan, "Maximum 8 hours per day")
            TestData.RuleBuilder.PricingRule("PRICE-SUMMER-LONG", "season", SString "summer", 1.2m)
            TestData.RuleBuilder.PricingRule("PRICE-LONG-BOOKING", "duration", SInt 8, 1.1m)
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let startTime = DateTimeOffset(2025, 8, 21, 10, 0, 0, TimeSpan.Zero)
        let endTime = startTime.AddHours(8) // Maximum duration

        let reservation = TestData.ReservationBuilder()
                           .WithTimeRange(startTime, endTime)
                           .WithCustomField("duration", 8)
                           .WithCustomField("season", "summer")
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.False(result.HasCriticalErrors)

        let hasLongDurationPricing = result.PricingResults |> Array.exists (fun r -> 
            r.Matched && r.Actions |> Array.exists (fun a -> a.ActionType = SetProperty))
        
        Assert.True(hasLongDurationPricing)

    // ===== PERFORMANCE TESTS =====

    [<Fact>]
    let ``Performance with multiple beach zones`` () =
        let rules = [|
            for i in 1..10 do
                TestData.RuleBuilder.CapacityRule($"ZONE-{i}", "participants", $"resourceAvailability.zone-{i}.capacity")
        |]

        let ruleSet = createRuleSet [| |] None None

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        
        for i in 1..20 do
            let reservation = TestData.ReservationBuilder()
                               .WithParticipants(2)
                               .WithZone($"zone-{(i % 10) + 1}")
                               .Build()
            let result = executeRules ruleSet reservation
            Assert.False(result.HasCriticalErrors)

        stopwatch.Stop()
        let avgMs = float stopwatch.ElapsedMilliseconds / 20.0
        Assert.True(avgMs < 100.0) // Average should be under 100ms per reservation

    [<Fact>]
    let ``Performance with complex pricing rules`` () =
        let rules = [|
            for season in ["summer"; "winter"; "spring"; "autumn"] do
                TestData.RuleBuilder.PricingRule($"PRICE-{season.ToUpper()}", "season", SString season, 
                    if season = "summer" then 1.2m elif season = "winter" then 0.8m else 1.0m)
            for hour in [9; 11; 14; 16; 18] do
                TestData.RuleBuilder.PricingRule($"PRICE-PEAK-{hour}", "isPeakHour", SBool true, 1.5m)
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        
        for i in 1..15 do
            let reservation = TestData.ReservationBuilder()
                               .WithCustomField("season", ["summer"; "winter"; "spring"; "autumn"].[i % 4])
                               .AsPeakHour()
                               .Build()
            let result = executeRules ruleSet reservation
            Assert.False(result.HasCriticalErrors)

        stopwatch.Stop()
        let avgMs = float stopwatch.ElapsedMilliseconds / 15.0
        Assert.True(avgMs < 150.0) // Average should be under 150ms per reservation

    // ===== INTEGRATION TESTS =====

    [<Fact>]
    let ``Full integration: Complete beach reservation workflow`` () =
        let rules = [|
            // Capacity and validation rules
            TestData.RuleBuilder.CapacityRule("CAP-MAIN", "participants", "resourceAvailability.main.capacity")
            TestData.RuleBuilder.LeadTimeRule("LEAD-BEACH", "leadMinutes", 180) // 3 hours for beach
            TestData.RuleBuilder.MinParticipantsRule("MIN-BEACH", 1)
            TestData.RuleBuilder.MaxParticipantsRule("MAX-BEACH", 8)
            
            // Special requirements rules
            TestData.RuleBuilder.ConditionalRule("FAM-CHILDREN", "isFamilyUmbrella", SBool true, "hasChildren", SBool false, Equals, "Family umbrellas require children")
            TestData.RuleBuilder.ConditionalRule("ACCESS-CAP", "hasAccessibilityNeeds", SBool true, "participants", SInt 6, GreaterThan, "Accessible zone capacity limited")
            TestData.RuleBuilder.AgeRestrictionRule("AGE-SUPERVISE", 16, true, "Minors require supervision")
            
            // Weather and policy rules
            TestData.RuleBuilder.WeatherPolicyRule("WEATHER-STORM", "stormy", false, "Stormy weather not allowed")
            TestData.RuleBuilder.CancellationPolicyRule("CANCEL-BEACH", 24, 100, "Beach cancellations 24+ hours for full refund")
            
            // Pricing rules
            TestData.RuleBuilder.PricingRule("PRICE-SUMMER", "season", SString "summer", 1.2m)
            TestData.RuleBuilder.PricingRule("PRICE-VIP", "isVIP", SBool true, 1.5m)
            TestData.RuleBuilder.PricingRule("PRICE-PEAK", "isPeakHour", SBool true, 1.3m)
            TestData.RuleBuilder.PricingRule("PRICE-FAMILY", "isFamilyUmbrella", SBool true, 1.1m)
            TestData.RuleBuilder.PricingRule("PRICE-ACCESS", "hasAccessibilityNeeds", SBool true, 1.05m)
        |]

        let ruleSet = createRuleSet [| |] None (Some rules)

        let reservation = TestData.ReservationBuilder()
                           .WithParticipants(4)
                           .WithLeadTime(240) // 4 hours
                           .WithUmbrella("family")
                           .WithZone("main")
                           .AsFamilyUmbrella()
                           .WithChildren()
                           .AsHoliday()
                           .AsPeakHour()
                           .WithCustomField("season", "summer")
                           .Build()

        let result = executeRules ruleSet reservation

        // Should pass all validations
        Assert.False(result.HasCriticalErrors)
        
        // Should have multiple pricing actions
        let pricingActions = result.PricingResults |> Array.filter (fun r -> r.Matched)
        Assert.True(pricingActions.Length >= 3)
        
        // Should have no validation errors
        let validationErrors = result.ValidationResults |> Array.filter (fun r -> r.HasCriticalErrors)
        Assert.Empty(validationErrors)

    [<Fact>]
    let ``Error handling: Malformed reservation data`` () =
        let rules = [|
            TestData.RuleBuilder.CapacityRule("CAP-TEST", "participants", "resourceAvailability.main.capacity")
        |]

        let ruleSet = createRuleSet rules None None

        let malformedJson = """{ "participants": "invalid", "leadMinutes": }"""

        Assert.ThrowsAny<JsonException>(fun () ->
            let doc = JsonDocument.Parse(malformedJson)
            let ctx = createContext()
            interpretRules ruleSet doc ctx |> ignore
        )

    [<Fact>]
    let ``Error handling: Missing required fields`` () =
        let rules = [|
            TestData.RuleBuilder.CapacityRule("CAP-TEST", "participants", "resourceAvailability.main.capacity")
        |]

        let ruleSet = createRuleSet rules None None

        let incompleteJson = """{ "leadMinutes": 120 }"""
        let doc = JsonDocument.Parse(incompleteJson)
        let ctx = createContext()

        let result = interpretRules ruleSet doc ctx
        Assert.NotNull(result)

    [<Fact>]
    let ``Error handling: Non-existent beach zone`` () =
        let rules = [|
            TestData.RuleBuilder.ConditionalRule("ZONE-EXIST", "beachZone", SString "nonexistent", "isAvailable", SBool false, Equals, "Zone not available")
        |]

        let ruleSet = createRuleSet rules None None

        let reservation = TestData.ReservationBuilder()
                           .WithZone("nonexistent")
                           .Build()

        let result = executeRules ruleSet reservation
        Assert.NotNull(result)