namespace ReservationService.Core

open System
open System.Text.Json

/// Generic, closed set of simple values for rules and context (FSCheck-friendly)
type SimpleValue =
    | SString of string
    | SInt of int
    | SDecimal of decimal
    | SBool of bool
    | SDateTime of DateTime
    | SArray of SimpleValue[]
    | SObject of Map<string, SimpleValue>
    | SNull

[<RequireQualifiedAccess>]
module SimpleValueOps =
    let inline private tryIntFromDecimal (d: decimal) : int option =
        let i = int d
        if decimal i = d then Some i else None

    let tryAsString = function SString s -> Some s | _ -> None
    let tryAsInt = function | SInt i -> Some i | SDecimal d -> tryIntFromDecimal d | _ -> None
    let tryAsDecimal = function | SDecimal d -> Some d | SInt i -> Some (decimal i) | _ -> None
    let tryAsBool = function SBool b -> Some b | _ -> None
    let tryAsDateTime = function SDateTime dt -> Some dt | _ -> None
    let tryAsArray = function SArray a -> Some a | _ -> None
    let tryAsObject = function SObject o -> Some o | _ -> None

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
            [| for i in 0 .. el.GetArrayLength() - 1 do yield ofJsonElement el.[i] |] |> SArray
        | JsonValueKind.Object ->
            let mutable map = Map.empty
            for p in el.EnumerateObject() do
                map <- map.Add(p.Name, ofJsonElement p.Value)
            SObject map
        | _ -> SString (el.GetRawText())

    let rec ofObj (value: obj) : SimpleValue =
        match value with
        | null -> SNull
        | :? string as s -> SString s
        | :? int as i -> SInt i
        | :? decimal as d -> SDecimal d
        | :? bool as b -> SBool b
        | :? DateTime as dt -> SDateTime dt
        | :? (string array) as sa -> sa |> Array.map SString |> SArray
        | :? (int array) as ia -> ia |> Array.map SInt |> SArray
        | :? (decimal array) as da -> da |> Array.map SDecimal |> SArray
        | :? (obj array) as oa -> oa |> Array.map ofObj |> SArray
        | :? Map<string, obj> as m -> m |> Map.map (fun _ v -> ofObj v) |> SObject
        | _ -> SString (value.ToString())

