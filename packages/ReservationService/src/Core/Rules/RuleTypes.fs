namespace ReservationService.Core

open System
open ReservationService.Core.RuleDomain

/// <summary>
/// Core types for the rule engine.
/// Defines the structure of rules, conditions, and actions following functional-first principles.
/// </summary>
module RuleTypes =

    // Forward-compatible categories with Unknown fallbacks
    type ValidationType = | Reservation | Customer | Resource | Payment | Unknown of string
    type BusinessArea = | Operations | Finance | Compliance | Sales | Unknown of string
    type ConflictCategory = | DoubleBooking | Capacity | TimeOverlap | ResourceUnavailable | Unknown of string
    type PricingType = | Base | Discount | Tax | Fee | Surcharge | Unknown of string
    type ApprovalRequirement = | Manager | Finance | Security | Compliance | Custom of string | Unknown of string

    /// <summary>
    /// Rule action that can be executed when a rule matches.
    /// Contains all information needed to perform the action.
    /// </summary>
    type RuleAction = {
        /// <summary>Type of action to perform</summary>
        Type: RuleActionType
        /// <summary>Human-readable message describing the action</summary>
        Message: string
        /// <summary>Severity level of the action</summary>
        Severity: RuleSeverity
        /// <summary>Additional parameters for the action</summary>
        Parameters: Map<string, string>
    }

    /// <summary>
    /// Simple rule condition for comparing field values.
    /// Represents a single comparison operation.
    /// </summary>
    type RuleCondition = {
        /// <summary>Field path to extract value from</summary>
        Field: FieldPath
        /// <summary>Comparison operator to use</summary>
        Operator: ComparisonOperator
        /// <summary>Static value to compare against (optional)</summary>
        Value: RuleValue option
        /// <summary>Reference field to compare against (optional)</summary>
        ReferenceField: FieldPath option
    }

    /// <summary>
    /// Complex condition that can be nested and combined.
    /// Supports both simple conditions and composite logical operations.
    /// </summary>
    type ComplexCondition =
        /// <summary>Simple condition with single comparison</summary>
        | Simple of RuleCondition
        /// <summary>Composite condition with logical operator and sub-conditions</summary>
        | Composite of LogicalOperator * ComplexCondition list

    /// <summary>
    /// Base rule definition containing all common rule properties.
    /// Follows functional-first design with immutable data.
    /// </summary>
    type IRule = {
        /// <summary>Unique identifier for the rule</summary>
        Id: Types.RuleId
        /// <summary>Human-readable name</summary>
        Name: string
        /// <summary>Detailed description of the rule's purpose</summary>
        Description: string
        /// <summary>Condition that must be met for the rule to trigger</summary>
        Condition: ComplexCondition option
        /// <summary>Actions to execute when the rule matches</summary>
        Actions: RuleAction[]
        /// <summary>Priority for rule execution (higher numbers execute first)</summary>
        Priority: int
        /// <summary>Whether the rule is currently enabled</summary>
        Enabled: bool
        /// <summary>Tags for categorizing and filtering rules</summary>
        Tags: string[]
    }

    /// <summary>
    /// Validation rule for data validation.
    /// Extends base rule with validation-specific properties.
    /// </summary>
    type ValidationRule = {
        Rule: IRule
        ValidationType: ValidationType
        ErrorMessage: string
    }

    type BusinessRule = {
        Rule: IRule
        BusinessArea: BusinessArea
        Impact: string
    }

    type ConflictRule = {
        Rule: IRule
        ConflictType: ConflictCategory
        Resolution: string
    }

    type PricingRule = {
        Rule: IRule
        PricingType: PricingType
        Formula: string
    }

    type ApprovalRule = {
        Rule: IRule
        ApprovalType: ApprovalRequirement
        RequiredApprovals: int
    }

    /// <summary>
    /// Rule execution context containing configuration and state for rule evaluation.
    /// Provides deterministic and configurable rule execution environment.
    /// </summary>
    type RuleExecutionContext = {
        /// <summary>Whether to enable detailed tracing</summary>
        TraceEnabled: bool
        /// <summary>ID of the user executing the rules</summary>
        UserId: string option
        /// <summary>Session ID for correlation</summary>
        SessionId: string option
        /// <summary>Additional context data</summary>
        AdditionalData: Map<string, string>
        /// <summary>Whether string comparisons should be case-insensitive</summary>
        CaseInsensitive: bool
        /// <summary>Target time zone for normalizing DateTime values</summary>
        TargetTimeZoneId: string option
        /// <summary>Whether to collect trace messages in results</summary>
        CollectTrace: bool
    }

    /// <summary>
    /// Complete rule set containing all types of rules.
    /// Organizes rules by category for efficient processing.
    /// </summary>
    type RuleSet = {
        /// <summary>Rules for data validation</summary>
        ValidationRules: ValidationRule[]
        /// <summary>Rules for business logic enforcement</summary>
        BusinessRules: BusinessRule[]
        /// <summary>Rules for conflict detection</summary>
        ConflictRules: ConflictRule[]
        /// <summary>Rules for pricing calculations</summary>
        PricingRules: PricingRule[]
        /// <summary>Rules for approval workflows</summary>
        ApprovalRules: ApprovalRule[]
        /// <summary>Additional metadata about the rule set</summary>
        Metadata: Map<string, string>
    }

    /// <summary>
    /// Helper functions for creating rules with validation and error handling.
    /// Follows functional-first principles with Result types for error handling.
    /// </summary>
    module RuleHelpers =

        /// <summary>
        /// Create a simple rule condition with validation.
        /// </summary>
        /// <param name="field">Field path to evaluate</param>
        /// <param name="operator">Comparison operator</param>
        /// <param name="value">Value to compare against</param>
        /// <returns>Result containing RuleCondition or error message</returns>
        let createCondition (field: string) (operator: ComparisonOperator) (value: RuleValue option) : Result<RuleCondition, string> =
            if String.IsNullOrWhiteSpace(field) then
                Result.Error "Field path cannot be empty"
            else
                Result.Ok {
                    Field = field
                    Operator = operator
                    Value = value
                    ReferenceField = None
                }

        /// <summary>
        /// Create a rule action with validation.
        /// </summary>
        /// <param name="actionType">Type of action</param>
        /// <param name="message">Action message</param>
        /// <param name="severity">Action severity</param>
        /// <returns>Result containing RuleAction or error message</returns>
        let createAction (actionType: RuleActionType) (message: string) (severity: RuleSeverity) : Result<RuleAction, string> =
            if String.IsNullOrWhiteSpace(message) then
                Result.Error "Action message cannot be empty"
            else
                Result.Ok {
                    Type = actionType
                    Message = message
                    Severity = severity
                    Parameters = Map.empty
                }

        /// <summary>
        /// Create a basic rule with validation.
        /// </summary>
        /// <param name="id">Unique rule identifier</param>
        /// <param name="name">Rule name</param>
        /// <param name="description">Rule description</param>
        /// <param name="condition">Rule condition (optional)</param>
        /// <param name="actions">Rule actions</param>
        /// <returns>Result containing IRule or error message</returns>
        let createRuleResult (id: Types.RuleId) (name: string) (description: string) (condition: ComplexCondition option) (actions: RuleAction[]) : Result<IRule, string> =
            if String.IsNullOrWhiteSpace(name) then
                Result.Error "Rule name cannot be empty"
            // Allow empty description for brevity in tests and simple rules
            elif Array.isEmpty actions then
                Result.Error "Rule must have at least one action"
            else
                Result.Ok {
                    Id = id
                    Name = name
                    Description = description
                    Condition = condition
                    Actions = actions
                    Priority = 100
                    Enabled = true
                    Tags = [||]
                }

        /// <summary>
        /// Back-compat helper: create rule and unwrap, raising on invalid input.
        /// Tests and callers that expect IRule can use this.
        /// </summary>
        let createRule (id: Types.RuleId) (name: string) (description: string) (condition: ComplexCondition option) (actions: RuleAction[]) : IRule =
            match createRuleResult id name description condition actions with
            | Result.Ok rule -> rule
            | Result.Error msg -> failwithf "Invalid rule: %s" msg

        /// <summary>
        /// Create a default rule execution context.
        /// </summary>
        /// <returns>Default RuleExecutionContext</returns>
        let createDefaultContext () : RuleExecutionContext =
            {
                TraceEnabled = false
                UserId = None
                SessionId = None
                AdditionalData = Map.empty
                CaseInsensitive = false
                TargetTimeZoneId = None
                CollectTrace = false
            }

        /// <summary>
        /// Create an empty rule set.
        /// </summary>
        /// <returns>Empty RuleSet</returns>
        let createEmptyRuleSet () : RuleSet =
            {
                ValidationRules = [||]
                BusinessRules = [||]
                ConflictRules = [||]
                PricingRules = [||]
                ApprovalRules = [||]
                Metadata = Map.empty
            }