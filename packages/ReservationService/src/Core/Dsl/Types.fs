namespace ReservationService.Core.Dsl

open System

// DTOs for the JSON DSL (wire format) — compiled into internal RuleTypes before evaluation
module Types =
    // Top-level request
    type Request = {
        SchemaVersion: string
        Settings: Settings option
        Domain: DomainDef option
        Data: System.Text.Json.JsonElement // canonical reservation facts object (opaque to engine)
        Functions: FunctionLib option
        Rules: RuleDef array
        Messages: Map<string, string> option // messageCode -> template, i18n handled by caller
    }

    and Settings = {
        Timezone: string option // IANA TZ
        Locale: string option   // BCP 47
        Evaluation: EvaluationSettings option
        Explain: bool option
    }

    and EvaluationSettings = {
        ShortCircuit: bool option
        FailOnUnknownField: bool option
        UnknownOperatorPolicy: string option // "error" | "ignore"
        MaxDepth: int option
        MaxRules: int option
        MaxArray: int option
        TimeBudgetMs: int option
    }

    // Domain description (optional but recommended)
    and DomainDef = {
        Types: DomainTypeDef array option
        Enums: EnumDef array option
        Derived: DerivedFieldDef array option
        Aliases: Map<string,string> option // alt name -> canonical path
    }

    and DomainTypeDef = {
        Name: string
        Fields: DomainFieldDef array
    }

    and DomainFieldDef = {
        Name: string
        Type: string // String|Int|Decimal|Bool|Date|Time|DateTime|Duration|Array[T]|Object
        Required: bool option
        Unit: string option
    }

    and EnumDef = {
        Name: string
        Values: string array
    }

    and DerivedFieldDef = {
        Name: string // canonical path under Data
        Expression: Expr // pure expression over Data
    }

    // Functions/macros (pure)
    and FunctionLib = {
        Macros: Map<string, Expr> option // name -> expression
        Allow: string array option // whitelist of builtins/operators
    }

    // Rules
    and RuleDef = {
        Id: string
        Name: string
        Description: string option
        Priority: int option
        Enabled: bool option
        Tags: string array option
        Condition: Cond
        Actions: ActionDef array
        Kind: string option // Validation|Business|Conflict|Pricing|Approval|Notification|Custom
        Scope: string option
    }

    and ActionDef = {
        Type: string // Validation|BusinessLogic|ConflictDetection|Pricing|Approval|Notification|Custom
        Severity: string option // Info|Warning|Error|Critical
        Code: string // message code
        Params: Map<string, DslValue> option
        PropertyUpdates: Map<string, DslValue> option
    }

    // Conditions/Expressions
    and SimpleCond = { Field: string; Operator: string; Value: DslValue option; ReferenceField: string option }
    and CompositeCond = { Op: string; Children: Cond array }
    and Cond =
        | Simple of SimpleCond
        | Composite of CompositeCond

    and CallExpr = { Name: string; Args: Map<string, Expr> }
    and Expr =
        | Value of DslValue
        | Field of string
        | Call of CallExpr

    and DslValue =
        | VString of string
        | VInt of int
        | VDecimal of decimal
        | VBool of bool
        | VDate of DateOnly
        | VTime of TimeOnly
        | VDateTime of DateTime
        | VDuration of TimeSpan
        | VArray of DslValue array
        | VObject of Map<string, DslValue>
        | VNull

