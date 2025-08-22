namespace ReservationService.Tests.Tests

module RuleEnginePropertyTests =
    open System
    open System.Text.Json
    open global.Xunit
    open global.FsCheck.Xunit

    open ReservationService.Core
    open ReservationService.Core.RuleEngine

    // Helper: write a SimpleValue into a Utf8JsonWriter
    let rec writeJson (w: Utf8JsonWriter) (v: SimpleValue) : unit =
        match v with
        | SString s -> w.WriteStringValue s
        | SInt i -> w.WriteNumberValue i
        | SDecimal d -> w.WriteNumberValue d
        | SBool b -> w.WriteBooleanValue b
        | SDateTime dt -> w.WriteStringValue (dt.ToString("o"))
        | SNull -> w.WriteNullValue()
        | SArray arr ->
            w.WriteStartArray()
            for x in arr do writeJson w x
            w.WriteEndArray()
        | SObject m ->
            w.WriteStartObject()
            for KeyValue(k, x) in m do
                w.WritePropertyName k
                writeJson w x
            w.WriteEndObject()

    let jsonOf (rootName:string) (sv: SimpleValue) : string =
        let buf = new System.Buffers.ArrayBufferWriter<byte>()
        use w = new Utf8JsonWriter(buf)
        w.WriteStartObject()
        w.WritePropertyName rootName
        writeJson w sv
        w.WriteEndObject()
        w.Flush()
        System.Text.Encoding.UTF8.GetString(buf.WrittenSpan)

    // ========== PROPERTIES (no custom generators needed) ==========

    [<Property(MaxTest = 200)>]
    let ``extractFieldValue roundtrips string`` (nameRaw:string) (value:string) =
        let name = if String.IsNullOrWhiteSpace nameRaw then "field" else nameRaw.Replace(".", "_")
        let json = jsonOf name (SString value)
        let doc = JsonDocument.Parse(json)
        extractFieldValue name doc = Some (SString value)

    [<Property(MaxTest = 200)>]
    let ``extractFieldValue roundtrips int`` (nameRaw:string) (value:int) =
        let name = if String.IsNullOrWhiteSpace nameRaw then "field" else nameRaw.Replace(".", "_")
        let json = jsonOf name (SInt value)
        let doc = JsonDocument.Parse(json)
        extractFieldValue name doc = Some (SInt value)

    [<Property(MaxTest = 200)>]
    let ``extractFieldValue roundtrips decimal`` (nameRaw:string) (value:decimal) =
        let name = if String.IsNullOrWhiteSpace nameRaw then "field" else nameRaw.Replace(".", "_")
        let json = jsonOf name (SDecimal value)
        let doc = JsonDocument.Parse(json)
        extractFieldValue name doc = Some (SDecimal value)

    [<Property(MaxTest = 400)>]
    let ``compareValues antisymmetry for < and > (ints)`` (a:int) (b:int) =
        let lt = compareValues LessThan (Some (SInt a)) (Some (SInt b))
        let gt = compareValues GreaterThan (Some (SInt a)) (Some (SInt b))
        not (lt && gt)

    [<Property(MaxTest = 400)>]
    let ``compareValues equality reflexive (ints)`` (a:int) =
        compareValues Equals (Some (SInt a)) (Some (SInt a)) = true

    [<Property(MaxTest = 200)>]
    let ``Contains works for arrays (membership of ints)`` (arr:int[]) (probe:int) =
        let left = arr |> Array.map SInt |> SArray
        if arr.Length = 0 then
            compareValues Contains (Some left) (Some (SInt probe)) = false
        else
            let idx = abs probe % arr.Length
            let memberVal = arr.[idx]
            compareValues Contains (Some left) (Some (SInt memberVal)) = true

    [<Property(MaxTest = 200)>]
    let ``IsNull behaves as expected`` () =
        let isNullNone = compareValues IsNull None (Some SNull)
        let isNotNullNone = compareValues IsNotNull None (Some SNull)
        let isNullSome = compareValues IsNull (Some (SString "x")) None
        let isNotNullSome = compareValues IsNotNull (Some (SString "x")) None
        isNullNone && not isNotNullNone && not isNullSome && isNotNullSome

