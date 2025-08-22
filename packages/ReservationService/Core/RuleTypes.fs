namespace ReservationService.Core

open System
open System.Text.Json.Serialization

// Core rule definition types for the rule-based interpreter service
// These types define how business logic is passed as data rather than hardcoded

// ========== BASIC VALUE TYPES ==========

/// Represents a field path in the data (e.g., "timeRange.start", "participants", "resource.capacity")
type FieldPath = string

/// Represents a comparison operator for rule conditions
type ComparisonOperator =
    | Equals
    | NotEquals
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Between
    | NotBetween
    | Contains
    | NotContains
    | In
    | NotIn
    | Matches // For regex patterns
    | IsNull
    | IsNotNull

/// Represents a logical operator for combining conditions
type LogicalOperator =
    | And
    | Or
    | Not

/// Represents a value that can be used in rule conditions
/// Closed, generic value space for FSCheck-friendly rule evaluation
type RuleValue = SimpleValue

// ========== RULE CONDITIONS ==========

/// Represents a single condition in a rule
type RuleCondition = {
    /// The field to evaluate (e.g., "timeRange.start")
    Field: FieldPath
    /// The comparison operator
    Operator: ComparisonOperator
    /// The value(s) to compare against
    Value: RuleValue option
    /// Alternative values for operators like Between, In
    Values: RuleValue[] option
    /// For referencing other fields (e.g., "resource.capacity")
    ReferenceField: FieldPath option
}

/// Represents a complex condition that can combine multiple conditions
type ComplexCondition =
    | Simple of RuleCondition
    | Composite of LogicalOperator * ComplexCondition list

// ========== RULE ACTIONS ==========

/// Severity level for rule actions
type RuleSeverity =
    | Info
    | Warning
    | Error
    | Critical

/// Types of actions a rule can take
type RuleActionType =
    | Accept
    | Reject
    | RequireApproval
    | ModifyRequest
    | AddWarning
    | AddInfo
    | CalculatePrice
    | SuggestAlternative
    | SetProperty
    | InvokeExternalValidation

/// Represents an action to take when a rule condition is met
type RuleAction = {
    /// The type of action to take
    ActionType: RuleActionType
    /// Severity level of the action
    Severity: RuleSeverity
    /// Message to include with the action
    Message: string option
    /// Additional parameters for the action
    Parameters: Map<string, RuleValue>
    /// Properties to set on the request (for ModifyRequest actions)
    PropertyUpdates: Map<FieldPath, RuleValue> option
    /// Alternative suggestions (for SuggestAlternative actions)
    Suggestions: Map<string, RuleValue> option
}

// ========== CORE RULE TYPES ==========

/// Base interface for all rule types
type IRule = {
    /// Unique identifier for the rule
    Id: string
    /// Human-readable name for the rule
    Name: string
    /// Description of what the rule does
    Description: string option
    /// Priority for rule execution (higher numbers execute first)
    Priority: int
    /// Whether this rule is enabled
    Enabled: bool
    /// Tags for categorizing rules
    Tags: string[]
    /// Condition that must be met for the rule to apply
    Condition: ComplexCondition option
    /// Actions to take when the rule applies
    Actions: RuleAction[]
}

/// Rule for validating individual reservation requests
type ValidationRule = {
    /// Base rule properties
    Rule: IRule
    /// Scope of validation (e.g., "timeRange", "participants", "resource")
    ValidationScope: string
    /// Whether this validation is required or optional
    IsRequired: bool
    /// Dependencies on other validations
    Dependencies: string[] // Rule IDs that must pass first
}

/// Rule for business logic and constraints
type BusinessRule = {
    /// Base rule properties  
    Rule: IRule
    /// Category of business rule (e.g., "capacity", "timing", "financial")
    Category: string
    /// Whether this rule can be overridden with approval
    CanOverride: bool
    /// Required approval level for overrides
    ApprovalLevel: string option
}

/// Rule for detecting and resolving conflicts
type ConflictRule = {
    /// Base rule properties
    Rule: IRule
    /// Type of conflict this rule detects
    ConflictType: string
    /// Strategy for resolving conflicts
    ResolutionStrategy: string
    /// Whether conflicts can be auto-resolved
    AutoResolve: bool
}

/// Rule for calculating prices and costs
type PricingRule = {
    /// Base rule properties
    Rule: IRule
    /// Pricing component this rule calculates
    PricingComponent: string
    /// Formula or calculation method
    CalculationMethod: string
    /// Base price or rate
    BaseAmount: decimal option
    /// Multipliers and modifiers
    Modifiers: Map<string, decimal>
}

/// Rule for determining what requires manual approval
type ApprovalRule = {
    /// Base rule properties
    Rule: IRule
    /// Type of approval required
    ApprovalType: string
    /// Required approval level or role
    ApprovalLevel: string
    /// Timeout for approval decisions
    ApprovalTimeout: TimeSpan option
    /// What happens if approval times out
    TimeoutAction: RuleAction option
}

// ========== RULE COLLECTIONS ==========

/// Collection of all rules for processing requests
type RuleSet = {
    /// Validation rules
    ValidationRules: ValidationRule[]
    /// Business rules
    BusinessRules: BusinessRule[]
    /// Conflict detection rules
    ConflictRules: ConflictRule[]
    /// Pricing rules
    PricingRules: PricingRule[]
    /// Approval rules
    ApprovalRules: ApprovalRule[]
    /// Metadata about the rule set
    Metadata: Map<string, string>
}

// ========== RULE EXECUTION CONTEXT ==========

/// Context information available during rule execution
type RuleExecutionContext = {
    /// Current timestamp
    Timestamp: DateTimeOffset
    /// User or system executing the rules
    ExecutedBy: string option
    /// Additional context data
    ContextData: Map<string, RuleValue>
    /// Configuration parameters
    Parameters: Map<string, RuleValue>
    /// Trace information for debugging
    TraceEnabled: bool
}

// ========== HELPER FUNCTIONS ==========

module RuleTypes =
    
    /// Create a simple rule condition
    let createCondition field operator value =
        {
            Field = field
            Operator = operator
            Value = Some value
            Values = None
            ReferenceField = None
        }
    
    /// Create a condition that compares two fields
    let createFieldComparison field operator referenceField =
        {
            Field = field
            Operator = operator
            Value = None
            Values = None
            ReferenceField = Some referenceField
        }
    
    /// Create a simple rule action
    let createAction actionType severity message =
        {
            ActionType = actionType
            Severity = severity
            Message = Some message
            Parameters = Map.empty
            PropertyUpdates = None
            Suggestions = None
        }
    
    /// Create a base rule structure
    let createBaseRule id name priority =
        {
            Id = id
            Name = name
            Description = None
            Priority = priority
            Enabled = true
            Tags = [||]
            Condition = None
            Actions = [||]
        }
