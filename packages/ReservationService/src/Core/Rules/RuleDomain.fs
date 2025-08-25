namespace ReservationService.Core

open System
open System.Text.Json

/// <summary>
/// Core domain types for the rule engine.
/// Contains pure types without business logic, following functional-first principles.
/// </summary>
module RuleDomain =

    /// <summary>
    /// Field path for accessing nested properties in JSON using dot notation.
    /// Example: "customer.address.city"
    /// </summary>
    type FieldPath = string

    /// <summary>
    /// Rule value types that can be used in rule conditions and actions.
    /// Designed to be simple and serializable for rule evaluation.
    /// </summary>
    type RuleValue =
        /// <summary>String value</summary>
        | SString of string
        /// <summary>Integer value</summary>
        | SInt of int
        /// <summary>Decimal value for precise numeric calculations</summary>
        | SDecimal of decimal
        /// <summary>Boolean value</summary>
        | SBool of bool
        /// <summary>DateTimeOffset value (timezone-aware)</summary>
        | SDateTimeOffset of DateTimeOffset
        /// <summary>Array of RuleValue items</summary>
        | SArray of RuleValue array
        /// <summary>Null value</summary>
        | SNull

    /// <summary>
    /// Comparison operators for rule conditions.
    /// Supports common comparison operations for different data types.
    /// </summary>
    type ComparisonOperator =
        /// <summary>Equality comparison</summary>
        | Equal
        /// <summary>Inequality comparison</summary>
        | NotEqual
        /// <summary>Less than comparison</summary>
        | LessThan
        /// <summary>Less than or equal comparison</summary>
        | LessThanOrEqual
        /// <summary>Greater than comparison</summary>
        | GreaterThan
        /// <summary>Greater than or equal comparison</summary>
        | GreaterThanOrEqual
        /// <summary>String contains operation</summary>
        | Contains
        /// <summary>String starts with operation</summary>
        | StartsWith
        /// <summary>String ends with operation</summary>
        | EndsWith
        /// <summary>Value is in a collection</summary>
        | In
        /// <summary>Value is not in a collection</summary>
        | NotIn
        /// <summary>Value is null</summary>
        | IsNull
        /// <summary>Value is not null</summary>
        | IsNotNull

    /// <summary>
    /// Logical operators for combining conditions.
    /// Used to build complex rule conditions.
    /// </summary>
    type LogicalOperator =
        /// <summary>Logical AND operation</summary>
        | And
        /// <summary>Logical OR operation</summary>
        | Or
        /// <summary>Logical NOT operation</summary>
        | Not

    /// <summary>
    /// Types of actions that rules can trigger.
    /// Categorizes rules by their purpose and processing requirements.
    /// </summary>
    type RuleActionType =
        /// <summary>Data validation action</summary>
        | Validation
        /// <summary>Business logic enforcement action</summary>
        | BusinessLogic
        /// <summary>Conflict detection action</summary>
        | ConflictDetection
        /// <summary>Pricing calculation action</summary>
        | Pricing
        /// <summary>Approval requirement action</summary>
        | Approval
        /// <summary>Notification sending action</summary>
        | Notification
        /// <summary>Custom action with specified name</summary>
        | Custom of string
        /// <summary>Unknown action type for forward compatibility</summary>
        | Unknown of string

    /// <summary>
    /// Severity levels for rule actions and messages.
    /// Used to prioritize and categorize rule outcomes.
    /// </summary>
    type RuleSeverity =
        /// <summary>Informational message</summary>
        | Info
        /// <summary>Warning that doesn't prevent processing</summary>
        | Warning
        /// <summary>Error that may prevent processing</summary>
        | Error
        /// <summary>Critical error that stops processing</summary>
        | Critical
        /// <summary>Unknown severity for forward compatibility</summary>
        | Unknown of string

    /// <summary>
    /// Helper module for RuleValue operations.
    /// Contains pure functions for working with RuleValue types.
    /// </summary>
    module SimpleValueOps =

        /// <summary>
        /// Attempts to parse a string as a DateTime using common formats.
        /// Returns Some DateTime if successful, None otherwise.
        /// </summary>
        /// <param name="s">String to parse</param>
        /// <returns>Parsed DateTimeOffset option</returns>
        let private tryParseDateTimeOffset (s: string) : DateTimeOffset option =
            let formats = [| "o"; "yyyy-MM-dd"; "yyyy-MM-ddTHH:mm:ss"; "yyyy-MM-ddTHH:mm:ss.fffK" |]
            match DateTimeOffset.TryParseExact(s, formats, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.RoundtripKind) with
            | true, dto -> Some dto
            | false, _ -> None

        /// <summary>
        /// Convert JsonElement to RuleValue recursively.
        /// Handles all JSON value kinds and attempts intelligent type conversion.
        /// </summary>
        /// <param name="element">JsonElement to convert</param>
        /// <returns>Corresponding RuleValue</returns>
        let rec ofJsonElement (element: JsonElement) : RuleValue =
            match element.ValueKind with
            | JsonValueKind.String ->
                let s = element.GetString()
                match tryParseDateTimeOffset s with
                | Some dto -> SDateTimeOffset dto
                | None -> SString s
            | JsonValueKind.Number ->
                match element.TryGetInt32() with
                | true, i -> SInt i
                | false, _ ->
                    match element.TryGetDecimal() with
                    | true, d -> SDecimal d
                    | false, _ -> SString (element.ToString())
            | JsonValueKind.True -> SBool true
            | JsonValueKind.False -> SBool false
            | JsonValueKind.Null -> SNull
            | JsonValueKind.Array ->
                element.EnumerateArray()
                |> Seq.map ofJsonElement
                |> Seq.toArray
                |> SArray
            | _ -> SString (element.ToString())

        /// <summary>
        /// Convert RuleValue to string representation for display.
        /// Handles nested arrays with proper formatting.
        /// </summary>
        /// <param name="value">RuleValue to convert</param>
        /// <returns>String representation</returns>
        let rec toString (value: RuleValue) : string =
            match value with
            | SString s -> s
            | SInt i -> string i
            | SDecimal d -> string d
            | SBool b -> string b
            | SDateTimeOffset dto -> dto.ToString("yyyy-MM-dd HH:mm:ss K")
            | SArray arr ->
                let items =
                    arr
                    |> Array.map (function
                        | SArray _ -> "[...]"  // Prevent deep nesting in display
                        | other -> toString other)
                    |> String.concat ","
                "[" + items + "]"
            | SNull -> "null"

        /// <summary>
        /// Safely extract string value from RuleValue.
        /// Returns Some string if value is SString, None otherwise.
        /// </summary>
        /// <param name="value">RuleValue to extract from</param>
        /// <returns>String option</returns>
        let tryGetString (value: RuleValue) : string option =
            match value with
            | SString s -> Some s
            | _ -> None

        /// <summary>
        /// Safely extract integer value from RuleValue.
        /// Returns Some int if value is SInt, None otherwise.
        /// </summary>
        /// <param name="value">RuleValue to extract from</param>
        /// <returns>Integer option</returns>
        let tryGetInt (value: RuleValue) : int option =
            match value with
            | SInt i -> Some i
            | _ -> None

        /// <summary>
        /// Safely extract decimal value from RuleValue.
        /// Returns Some decimal if value is SDecimal or SInt, None otherwise.
        /// </summary>
        /// <param name="value">RuleValue to extract from</param>
        /// <returns>Decimal option</returns>
        let tryGetDecimal (value: RuleValue) : decimal option =
            match value with
            | SDecimal d -> Some d
            | SInt i -> Some (decimal i)
            | _ -> None

        /// <summary>
        /// Safely extract boolean value from RuleValue.
        /// Returns Some bool if value is SBool, None otherwise.
        /// </summary>
        /// <param name="value">RuleValue to extract from</param>
        /// <returns>Boolean option</returns>
        let tryGetBool (value: RuleValue) : bool option =
            match value with
            | SBool b -> Some b
            | _ -> None

        /// <summary>
        /// Check if RuleValue is null.
        /// </summary>
        /// <param name="value">RuleValue to check</param>
        /// <returns>True if value is SNull</returns>
        let isNull (value: RuleValue) : bool =
            match value with
            | SNull -> true
            | _ -> false