namespace ReservationService.Core

open System
open System.Text.Json

/// <summary>
/// Generic, closed set of simple values for rules and context.
/// Designed to be FSCheck-friendly and support JSON serialization.
/// Used for representing dynamic data in rule evaluation contexts.
/// </summary>
type SimpleValue =
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
    /// <summary>Array of SimpleValue items</summary>
    | SArray of SimpleValue[]
    /// <summary>Object represented as a map of key-value pairs</summary>
    | SObject of Map<string, SimpleValue>
    /// <summary>Null value</summary>
    | SNull

/// <summary>
/// Operations for working with SimpleValue types.
/// Provides safe conversion and extraction functions.
/// </summary>
[<RequireQualifiedAccess>]
module SimpleValueOps =
    /// <summary>
    /// Attempts to convert a decimal to an integer if it represents a whole number.
    /// </summary>
    /// <param name="d">The decimal value to convert</param>
    /// <returns>Some integer if conversion is lossless, None otherwise</returns>
    let inline private tryIntFromDecimal (d: decimal) : int option =
        let i = int d
        if decimal i = d then Some i else None

    /// <summary>Extracts string value if the SimpleValue is SString</summary>
    let tryAsString = function SString s -> Some s | _ -> None

    /// <summary>Extracts integer value, converting from decimal if lossless</summary>
    let tryAsInt = function
        | SInt i -> Some i
        | SDecimal d -> tryIntFromDecimal d
        | _ -> None

    /// <summary>Extracts decimal value, converting from integer if needed</summary>
    let tryAsDecimal = function
        | SDecimal d -> Some d
        | SInt i -> Some (decimal i)
        | _ -> None

    /// <summary>Extracts boolean value if the SimpleValue is SBool</summary>
    let tryAsBool = function SBool b -> Some b | _ -> None

    /// <summary>Extracts DateTimeOffset value if the SimpleValue is SDateTimeOffset</summary>
    let tryAsDateTimeOffset = function SDateTimeOffset dto -> Some dto | _ -> None

    /// <summary>Extracts array value if the SimpleValue is SArray</summary>
    let tryAsArray = function SArray a -> Some a | _ -> None

    /// <summary>Extracts object map if the SimpleValue is SObject</summary>
    let tryAsObject = function SObject o -> Some o | _ -> None

    /// <summary>
    /// Converts a JsonElement to a SimpleValue recursively.
    /// Handles all JSON value kinds and preserves structure.
    /// </summary>
    /// <param name="el">The JsonElement to convert</param>
    /// <returns>SimpleValue representation of the JSON element</returns>
    let rec ofJsonElement (el: JsonElement) : SimpleValue =
        match el.ValueKind with
        | JsonValueKind.String -> SString (el.GetString())
        | JsonValueKind.Number ->
            let mutable i = 0
            if el.TryGetInt32(&i) then SInt i else SDecimal (el.GetDecimal())
        | JsonValueKind.True -> SBool true
        | JsonValueKind.False -> SBool false
        | JsonValueKind.Null -> SNull
        | JsonValueKind.Array ->
            [| for i in 0 .. el.GetArrayLength() - 1 do
                yield ofJsonElement el.[i] |]
            |> SArray
        | JsonValueKind.Object ->
            let mutable map = Map.empty
            for p in el.EnumerateObject() do
                map <- map.Add(p.Name, ofJsonElement p.Value)
            SObject map
        | _ -> SString (el.GetRawText())

    /// <summary>
    /// Converts a .NET object to a SimpleValue recursively.
    /// Handles common .NET types and collections.
    /// Falls back to string representation for unknown types.
    /// </summary>
    /// <param name="value">The object to convert</param>
    /// <returns>SimpleValue representation of the object</returns>
    let rec ofObj (value: obj) : SimpleValue =
        match value with
        | null -> SNull
        | :? string as s -> SString s
        | :? int as i -> SInt i
        | :? decimal as d -> SDecimal d
        | :? bool as b -> SBool b
        | :? DateTime as dt -> SDateTimeOffset (DateTimeOffset(dt))
        | :? DateTimeOffset as dto -> SDateTimeOffset dto
        | :? (string array) as sa -> sa |> Array.map SString |> SArray
        | :? (int array) as ia -> ia |> Array.map SInt |> SArray
        | :? (decimal array) as da -> da |> Array.map SDecimal |> SArray
        | :? (obj array) as oa -> oa |> Array.map ofObj |> SArray
        | :? Map<string, obj> as m ->
            m |> Map.map (fun _ v -> ofObj v) |> SObject
        | _ -> SString (value.ToString())

