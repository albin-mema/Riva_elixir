namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open ReservationService.Core
open ReservationService.Core.RuleTypes

/// Common builders and helpers for scenario tests
module ScenarioBuilders =

    type ReservationBuilder() =
        let mutable participants = 4
        let mutable leadMinutes = 120
        let mutable startTime = DateTimeOffset(2025,8,21,19,0,0,TimeSpan.Zero)
        let mutable endTime = DateTimeOffset(2025,8,21,20,0,0,TimeSpan.Zero)
        let mutable customFields = Map.empty<string, obj>

        member this.WithParticipants(count: int) =
            participants <- count; this

        member this.WithLeadTime(minutes: int) =
            leadMinutes <- minutes; this

        member this.WithTimeRange(startT: DateTimeOffset, endT: DateTimeOffset) =
            startTime <- startT; endTime <- endT; this

        member this.WithField(key: string, value: obj) =
            customFields <- customFields.Add(key, value); this

        member this.AsVIP() = this.WithField("isVIP", true)
        member this.AsHoliday() = this.WithField("isHoliday", true)
        member this.AsPeakHour() = this.WithField("isPeakHour", true)
        member this.AsWeekend() = this.WithField("isWeekend", true)
        member this.WithAccessibilityNeeds() = this.WithField("hasAccessibilityNeeds", true)
        member this.WithSpecialDietary() = this.WithField("hasSpecialDietary", true)
        member this.AsWalkIn() = this.WithField("isWalkIn", true) |> ignore; leadMinutes <- 0; this
        member this.AsSpecialEvent(authorized: bool) = this.WithField("isSpecialEvent", true).WithField("hasAuthorization", authorized)

        member this.Build() =
            // Build JSON as an object graph
            let fields = Dictionary<string, obj>()
            fields["participants"] <- box participants
            fields["leadMinutes"] <- box leadMinutes
            let timeRange = Dictionary<string, obj>()
            timeRange["start"] <- box (startTime.ToString("o"))
            timeRange["end"] <- box (endTime.ToString("o"))
            timeRange["timeZone"] <- box "UTC"
            fields["timeRange"] <- box timeRange
            // copy custom fields
            customFields |> Map.iter (fun k v -> fields[k] <- v)
            JsonSerializer.Serialize(fields)

    type ResourceBuilder() =
        let resources = Dictionary<string, obj>()

        member this.AddTable(name: string, capacity: int, ?isAccessible: bool, ?isOutdoor: bool, ?isPrivate: bool) =
            let table =
                {| capacity = capacity
                   isAccessible = defaultArg isAccessible false
                   isOutdoor = defaultArg isOutdoor false
                   isPrivate = defaultArg isPrivate false |}
            resources[name] <- table :> obj
            this

        member this.AddTotalCapacity(total: int) =
            resources["totalCapacity"] <- box total
            this

        member this.Build() =
            let outer = Dictionary<string, obj>()
            outer["resourceAvailability"] <- box resources
            JsonSerializer.Serialize(outer)

    type RuleBuilder() =
        static member CapacityRule(id: string, field: string, referenceField: string, ?message: string) : ValidationRule =
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

        static member LeadTimeRule(id: string, field: string, minMinutes: int, ?message: string) : ValidationRule =
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
                Message = message |> Option.orElse (Some (sprintf "Minimum %d minutes lead time required" minMinutes))
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

        static member ConditionalRule(id: string, conditionField: string, conditionValue: RuleValue, targetField: string, targetValue: RuleValue, operator: ComparisonOperator, ?message: string) : ValidationRule =
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

        static member MinParticipantsRule(id: string, minCount: int, ?message: string) : ValidationRule =
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

        static member MaxParticipantsRule(id: string, maxCount: int, ?message: string) : ValidationRule =
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

    let createRuleSet (validationRules: ValidationRule[]) (businessRulesOpt: BusinessRule[] option) (pricingRulesOpt: PricingRule[] option) : RuleSet =
        {
            ValidationRules = validationRules
            BusinessRules = defaultArg businessRulesOpt [||]
            ConflictRules = [||]
            PricingRules = defaultArg pricingRulesOpt [||]
            ApprovalRules = [||]
            Metadata = Map.empty
        }

    let interpret (ruleSet: RuleSet) (reservationJson: string) (resourceJson: string) =
        let reservationDoc = JsonDocument.Parse(reservationJson)
        let resourceDoc = JsonDocument.Parse(resourceJson)
        let combined = Dictionary<string, JsonElement>()
        for p in reservationDoc.RootElement.EnumerateObject() do combined[p.Name] <- p.Value
        combined["resourceAvailability"] <- resourceDoc.RootElement.GetProperty("resourceAvailability")
        let combinedJson = JsonSerializer.Serialize(combined)
        let doc = JsonDocument.Parse(combinedJson)
        let ctx = { Timestamp = DateTimeOffset.UtcNow; ExecutedBy = None; ContextData = Map.empty; Parameters = Map.empty; TraceEnabled = false }
        RuleEngine.interpretRules ruleSet doc ctx

