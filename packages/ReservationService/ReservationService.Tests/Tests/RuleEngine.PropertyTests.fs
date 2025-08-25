namespace ReservationService.Tests.Tests

module RuleEnginePropertyTests =
    open System
    open System.Text.Json
    open global.Xunit
    open global.FsCheck.Xunit

    open ReservationService.Core
    open ReservationService.Core.RuleDomain
    open ReservationService.Core.RuleEngine

    open ReservationService.Tests
    // Helper: write a RuleValue into a Utf8JsonWriter
    let rec writeJson (w: Utf8JsonWriter) (v: RuleValue) : unit =
        match v with
        | SString s -> w.WriteStringValue s
        | SInt i -> w.WriteNumberValue i
        | SDecimal d -> w.WriteNumberValue d
        | SBool b -> w.WriteBooleanValue b
        | SDateTime dt -> w.WriteStringValue (dt.ToString("o"))
        | SNull -> w.WriteNullValue()

    let jsonOf (rootName:string) (sv: RuleValue) : string =
        let buf = new System.Buffers.ArrayBufferWriter<byte>()
        use w = new Utf8JsonWriter(buf)
        w.WriteStartObject()
        w.WritePropertyName rootName
        writeJson w sv
        w.WriteEndObject()
        w.Flush()
        System.Text.Encoding.UTF8.GetString(buf.WrittenSpan)

    // ========== PROPERTIES (no custom generators needed) ==========

    [<ConfigurableProperty>]
    let ``extractFieldValue roundtrips string`` (nameRaw:string) (value:string) =
        let name = if String.IsNullOrWhiteSpace nameRaw then "field" else nameRaw.Replace(".", "_")
        let json = jsonOf name (SString value)
        let doc = JsonDocument.Parse(json)
        extractFieldValue name doc = Some (SString value)

    [<ConfigurableProperty>]
    let ``extractFieldValue roundtrips int`` (nameRaw:string) (value:int) =
        let name = if String.IsNullOrWhiteSpace nameRaw then "field" else nameRaw.Replace(".", "_")
        let json = jsonOf name (SInt value)
        let doc = JsonDocument.Parse(json)
        extractFieldValue name doc = Some (SInt value)

    [<ConfigurableProperty>]
    let ``extractFieldValue roundtrips decimal`` (nameRaw:string) (value:decimal) =
        let name = if String.IsNullOrWhiteSpace nameRaw then "field" else nameRaw.Replace(".", "_")
        let json = jsonOf name (SDecimal value)
        let doc = JsonDocument.Parse(json)
        extractFieldValue name doc = Some (SDecimal value)

    [<ConfigurableProperty>]
    let ``compareValues antisymmetry for < and > (ints)`` (a:int) (b:int) =
        let dummyCtx : ReservationService.Core.RuleTypes.RuleExecutionContext = { TraceEnabled=false; UserId=None; SessionId=None; AdditionalData=Map.empty; CaseInsensitive=true; TargetTimeZoneId=None; CollectTrace=false }
        let lt = compareValues dummyCtx LessThan (Some (SInt a)) (Some (SInt b))
        let gt = compareValues dummyCtx GreaterThan (Some (SInt a)) (Some (SInt b))
        not (lt && gt)

    [<ConfigurableProperty>]
    let ``compareValues equality reflexive (ints)`` (a:int) =
        let dummyCtx : ReservationService.Core.RuleTypes.RuleExecutionContext = { TraceEnabled=false; UserId=None; SessionId=None; AdditionalData=Map.empty; CaseInsensitive=true; TargetTimeZoneId=None; CollectTrace=false }
        compareValues dummyCtx Equal (Some (SInt a)) (Some (SInt a)) = true

    [<ConfigurableProperty>]
    let ``In works for array membership (ints)`` (arr:int[]) (probe:int) =
        let distinct = arr |> Array.distinct
        let exists = distinct |> Array.exists ((=) probe)
        // In expects Right to be SArray; Left is the item
        let right = distinct |> Array.map SInt |> SArray
        let dummyCtx : ReservationService.Core.RuleTypes.RuleExecutionContext = { TraceEnabled=false; UserId=None; SessionId=None; AdditionalData=Map.empty; CaseInsensitive=true; TargetTimeZoneId=None; CollectTrace=false }
        compareValues dummyCtx In (Some (SInt probe)) (Some right) = exists

    [<ConfigurableProperty>]
    let ``IsNull behaves as expected`` () =
        let dummyCtx : ReservationService.Core.RuleTypes.RuleExecutionContext = { TraceEnabled=false; UserId=None; SessionId=None; AdditionalData=Map.empty; CaseInsensitive=true; TargetTimeZoneId=None; CollectTrace=false }
        let isNullNone = compareValues dummyCtx IsNull None (Some SNull)
        let isNotNullNone = compareValues dummyCtx IsNotNull None (Some SNull)
        let isNullSome = compareValues dummyCtx IsNull (Some (SString "x")) None
        let isNotNullSome = compareValues dummyCtx IsNotNull (Some (SString "x")) None
        isNullNone && not isNotNullNone && not isNullSome && isNotNullSome

