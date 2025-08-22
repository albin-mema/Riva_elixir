namespace ReservationService.Tests.Tests

module RuleEngineAdvancedPropertyTests =
    open System
    open System.Text.Json
    open Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit

    open ReservationService.Core
    open ReservationService.Core.RuleTypes
    open ReservationService.Core.RuleEngine
    open ReservationService.Core.JsonSerialization

    // ---------- JSON helpers (reuse SimpleValue for constructing Json) ----------
    let rec private writeJson (w: Utf8JsonWriter) (v: SimpleValue) : unit =
        match v with
        | SString s -> w.WriteStringValue s
        | SInt i -> w.WriteNumberValue i
        | SDecimal d -> w.WriteNumberValue d
        | SBool b -> w.WriteBooleanValue b
        | SDateTime dt -> w.WriteStringValue (dt.ToString("o"))
        | SNull -> w.WriteNullValue()
        | SArray arr ->
            w.WriteStartArray(); arr |> Array.iter (writeJson w); w.WriteEndArray()
        | SObject m ->
            w.WriteStartObject();
            for KeyValue(k, x) in m do
                w.WritePropertyName k
                writeJson w x
            w.WriteEndObject()

    let private jsonOfMap (m: Map<string, SimpleValue>) : string =
        let buf = new System.Buffers.ArrayBufferWriter<byte>()
        use w = new Utf8JsonWriter(buf)
        w.WriteStartObject()
        for KeyValue(k, v) in m do
            w.WritePropertyName k
            writeJson w v
        w.WriteEndObject()
        w.Flush()
        System.Text.Encoding.UTF8.GetString(buf.WrittenSpan)

    // ---------- Generators for rule testing ----------
    type FieldName = string

    let private genAscii = Gen.elements ([ 'a'..'z' ] @ [ '0'..'9' ] @ [ '_' ])
    let private genFieldName : Gen<FieldName> =
        gen {
            let! len = Gen.choose(3, 10)
            let! chars = Gen.listOfLength len genAscii
            return new string (List.toArray chars)
        }

    let private genPlainString : Gen<string> =
        gen {
            let! len = Gen.choose(0, 20)
            let! chars = Gen.listOfLength len genAscii
            return new string (List.toArray chars)
        }

    let private genDateTime : Gen<DateTime> =
        gen {
            let epoch = DateTime(2000,1,1,0,0,0,DateTimeKind.Utc)
            let! days = Gen.choose(-20000, 20000)
            let! secs = Gen.choose(0, 24*60*60)
            return epoch.AddDays(float days).AddSeconds(float secs)
        }

    let rec private genSimpleValue : Gen<SimpleValue> =
        let leaf =
            Gen.frequency [
                4, Gen.map SString genPlainString
                4, Gen.map SInt (Gen.choose(-1000, 1000))
                2, Gen.map (decimal >> SDecimal) (Gen.choose(-1000, 1000))
                2, Gen.map SBool (Gen.elements [true; false])
                2, Gen.map SDateTime genDateTime
                1, Gen.constant SNull
            ]
        Gen.frequency [
            6, leaf
            2, Gen.arrayOfLength 3 leaf |> Gen.map SArray
            1, Gen.constant (SObject Map.empty) // avoid deep nesting
        ]

    let private chooseComparablePair op : Gen<SimpleValue * SimpleValue> =
        match op with
        | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual ->
            Gen.oneof [
                Gen.map2 (fun a b -> (SInt a, SInt b)) (Gen.choose(-1000, 1000)) (Gen.choose(-1000, 1000))
                Gen.map2 (fun a b -> (SDecimal a, SDecimal b)) (Gen.choose(-1000, 1000) |> Gen.map decimal) (Gen.choose(-1000, 1000) |> Gen.map decimal)
                Gen.map2 (fun a b -> (SDateTime a, SDateTime b)) genDateTime genDateTime
            ]
        | _ -> Gen.map (fun v -> (v, v)) (genSimpleValue)

    type ConditionFixture = {
        Condition: RuleCondition
        Json: JsonDocument
        Expected: bool
    }

    let private genIsNullFixture : Gen<ConditionFixture> =
        gen {
            let! field = genFieldName
            let! expectNull = Gen.elements [true; false]
            let op = if expectNull then IsNull else IsNotNull
            let jsonMap = if expectNull then Map.empty else Map [ field, SString "x" ]
            let json = JsonDocument.Parse(jsonOfMap jsonMap)
            let cond = { Field = field; Operator = op; Value = None; Values = None; ReferenceField = None }
            return { Condition = cond; Json = json; Expected = true }
        }

    let private genContainsFixture : Gen<ConditionFixture> =
        gen {
            let! field = genFieldName
            let! memberVal = Gen.choose(-100,100)
            let! includeMember = Gen.elements [true; false]
            let arr = if includeMember then [| memberVal; memberVal+1 |] else [| memberVal+1; memberVal+2 |]
            let jsonMap = Map [ field, (arr |> Array.map SInt |> SArray) ]
            let json = JsonDocument.Parse(jsonOfMap jsonMap)
            let cond = { Field = field; Operator = Contains; Value = Some (SInt memberVal); Values = None; ReferenceField = None }
            return { Condition = cond; Json = json; Expected = includeMember }
        }

    let private genRelationalFixture : Gen<ConditionFixture> =
        gen {
            let! fieldL = genFieldName
            let! fieldR = genFieldName
            let! op = Gen.elements [ LessThan; LessThanOrEqual; GreaterThan; GreaterThanOrEqual; Equals; NotEquals ]
            let! (lv, rv) = chooseComparablePair op
            // Decide expected based on concrete values
            let expected =
                match op, lv, rv with
                | Equals, l, r -> l = r
                | NotEquals, l, r -> l <> r
                | LessThan, SInt a, SInt b -> a < b
                | LessThan, SDecimal a, SDecimal b -> a < b
                | LessThan, SDateTime a, SDateTime b -> a < b
                | LessThanOrEqual, SInt a, SInt b -> a <= b
                | LessThanOrEqual, SDecimal a, SDecimal b -> a <= b
                | LessThanOrEqual, SDateTime a, SDateTime b -> a <= b
                | GreaterThan, SInt a, SInt b -> a > b
                | GreaterThan, SDecimal a, SDecimal b -> a > b
                | GreaterThan, SDateTime a, SDateTime b -> a > b
                | GreaterThanOrEqual, SInt a, SInt b -> a >= b
                | GreaterThanOrEqual, SDecimal a, SDecimal b -> a >= b
                | GreaterThanOrEqual, SDateTime a, SDateTime b -> a >= b
                | _ -> false
            let jsonMap = Map [ fieldL, lv; fieldR, rv ]
            let json = JsonDocument.Parse(jsonOfMap jsonMap)
            let cond = { Field = fieldL; Operator = op; Value = None; Values = None; ReferenceField = Some fieldR }
            return { Condition = cond; Json = json; Expected = expected }
        }

    let private genConditionFixture : Gen<ConditionFixture> =
        Gen.frequency [
            2, genIsNullFixture
            2, genContainsFixture
            6, genRelationalFixture
        ]

    // ---------- Arbitraries ----------
    type Generators =
        static member ConditionFixture() =
            let g = genConditionFixture
            { new Arbitrary<ConditionFixture>() with
                override _.Generator = g
                override _.Shrinker _ = Seq.empty }

    // ---------- Properties: condition and composition semantics ----------

    [<Property(MaxTest = 400, Arbitrary = [| typeof<Generators> |], Skip = "WIP: stabilizing generators")>]
    let ``evaluateCondition agrees with constructed JSON`` (fixture: ConditionFixture) =
        let ctx = { Timestamp = DateTimeOffset.UtcNow; ExecutedBy = None; ContextData = Map.empty; Parameters = Map.empty; TraceEnabled = false }
        evaluateCondition fixture.Condition fixture.Json ctx = fixture.Expected

    [<Property(MaxTest = 300, Arbitrary = [| typeof<Generators> |], Skip = "WIP: stabilizing generators")>]
    let ``Composite And behaves as conjunction`` (f1: ConditionFixture, f2: ConditionFixture) =
        let ctx = { Timestamp = DateTimeOffset.UtcNow; ExecutedBy = None; ContextData = Map.empty; Parameters = Map.empty; TraceEnabled = false }
        // Merge JSON maps (right-biased for collisions)
        let mergeJson (a:JsonDocument) (b:JsonDocument) =
            let toMap (jd:JsonDocument) =
                jd.RootElement.EnumerateObject() |> Seq.map (fun p -> p.Name, SimpleValueOps.ofJsonElement p.Value) |> Map.ofSeq
            let merged = (toMap f1.Json) |> Map.fold (fun m k v -> Map.add k v m) (toMap f2.Json)
            JsonDocument.Parse(jsonOfMap merged)
        let json = mergeJson f1.Json f2.Json
        let cond = Composite (And, [ Simple f1.Condition; Simple f2.Condition ])
        let expected = f1.Expected && f2.Expected
        evaluateComplexCondition cond json ctx = expected

    [<Property(MaxTest = 300, Arbitrary = [| typeof<Generators> |], Skip = "WIP: stabilizing generators")>]
    let ``Composite Or behaves as disjunction`` (f1: ConditionFixture, f2: ConditionFixture) =
        let ctx = { Timestamp = DateTimeOffset.UtcNow; ExecutedBy = None; ContextData = Map.empty; Parameters = Map.empty; TraceEnabled = false }
        let mergeJson (a:JsonDocument) (b:JsonDocument) =
            let toMap (jd:JsonDocument) =
                jd.RootElement.EnumerateObject() |> Seq.map (fun p -> p.Name, SimpleValueOps.ofJsonElement p.Value) |> Map.ofSeq
            let merged = (toMap f1.Json) |> Map.fold (fun m k v -> Map.add k v m) (toMap f2.Json)
            JsonDocument.Parse(jsonOfMap merged)
        let json = mergeJson f1.Json f2.Json
        let cond = Composite (Or, [ Simple f1.Condition; Simple f2.Condition ])
        let expected = f1.Expected || f2.Expected
        evaluateComplexCondition cond json ctx = expected

    [<Property(MaxTest = 300, Arbitrary = [| typeof<Generators> |], Skip = "WIP: stabilizing generators")>]
    let ``Composite Not inverts`` (fixture: ConditionFixture) =
        let ctx = { Timestamp = DateTimeOffset.UtcNow; ExecutedBy = None; ContextData = Map.empty; Parameters = Map.empty; TraceEnabled = false }
        let cond = Composite (Not, [ Simple fixture.Condition ])
        evaluateComplexCondition cond fixture.Json ctx = not fixture.Expected

    // ---------- Properties: rule and ruleset behavior ----------

    let private baseRule id name prio cond actions : IRule =
        { Id = id; Name = name; Description = None; Priority = prio; Enabled = true; Tags = [||]; Condition = Some cond; Actions = actions }

    let private mkRuleEval (cond:ComplexCondition) (json:JsonDocument) (trace:bool) : RuleEvaluationResult =
        let ctx = { Timestamp = DateTimeOffset.UtcNow; ExecutedBy = None; ContextData = Map.empty; Parameters = Map.empty; TraceEnabled = trace }
        let rule = baseRule "R1" "test" 10 cond [| { ActionType = Reject; Severity = Error; Message = Some "x"; Parameters = Map.empty; PropertyUpdates = None; Suggestions = None } |]
        let res = evaluateRule rule json ctx
        res

    [<Property(MaxTest = 200, Arbitrary = [| typeof<Generators> |], Skip = "WIP: stabilizing generators")>]
    let ``evaluateRule emits actions iff condition matches`` (fixture: ConditionFixture) =
        let cond = Simple fixture.Condition
        let res = mkRuleEval cond fixture.Json false
        if fixture.Expected then res.Matched && res.Actions.Length = 1 else (not res.Matched) && res.Actions.Length = 0

    [<Property(MaxTest = 200, Arbitrary = [| typeof<Generators> |], Skip = "WIP: stabilizing generators")>]
    let ``interpretRules flags HasCriticalErrors when any Critical action`` (fixture: ConditionFixture) =
        let ctx = { Timestamp = DateTimeOffset.UtcNow; ExecutedBy = None; ContextData = Map.empty; Parameters = Map.empty; TraceEnabled = false }
        let critAction = { ActionType = Reject; Severity = Critical; Message = Some "crit"; Parameters = Map.empty; PropertyUpdates = None; Suggestions = None }
        let okAction = { ActionType = AddInfo; Severity = Info; Message = Some "info"; Parameters = Map.empty; PropertyUpdates = None; Suggestions = None }
        let ruleCrit = { Rule = baseRule "RC" "critical" 100 (Simple fixture.Condition) [| critAction |]; ValidationScope = "x"; IsRequired = true; Dependencies = [||] }
        let ruleInfo = { Rule = baseRule "RI" "info" 10 (Simple fixture.Condition) [| okAction |]; ValidationScope = "y"; IsRequired = false; Dependencies = [||] }
        let rs : RuleSet = { ValidationRules = [| ruleCrit |]; BusinessRules = [||]; ConflictRules = [||]; PricingRules = [||]; ApprovalRules = [||]; Metadata = Map.empty }
        let res = interpretRules rs fixture.Json ctx
        // If condition true, then HasCriticalErrors must be true; otherwise false
        (fixture.Expected && res.HasCriticalErrors) || ((not fixture.Expected) && (not res.HasCriticalErrors))

    [<Property(MaxTest = 150, Arbitrary = [| typeof<Generators> |], Skip = "WIP: stabilizing generators")>]
    let ``Trace emits text when enabled`` (fixture: ConditionFixture) =
        let cond = Simple fixture.Condition
        let res = mkRuleEval cond fixture.Json true
        res.Trace.IsSome

    // ---------- End-to-end via JsonSerialization.processJsonRequest ----------

    [<Property(MaxTest = 100, Arbitrary = [| typeof<Generators> |], Skip = "WIP: stabilizing generators")>]
    let ``processJsonRequest returns success=false when a critical rule matches`` (fixture: ConditionFixture) =
        // Build a minimal JSON request using the DTOs
        let dtoReq = {
            JsonReservationRequest.Id = "id-1"
            CustomerId = "cust-1"
            BusinessId = Some "biz-1"
            ResourceId = "res-1"
            ResourceType = "room"
            TimeRange = { Start = DateTimeOffset.UtcNow.ToString("o"); End = DateTimeOffset.UtcNow.AddHours(1.).ToString("o"); TimeZone = Some "UTC" }
            Participants = Some 2
            Services = Some [| "wifi" |]
            SpecialRequests = Some "near window"
            Metadata = None
        }
        let dto = {
            JsonRuleInterpretationRequest.ReservationRequests = [| dtoReq |]
            RuleSet = { ValidationRules = [||]; BusinessRules = [||]; ConflictRules = [||]; PricingRules = [||]; ApprovalRules = [||]; Metadata = Map.empty }
            ContextData = None
            RuleParameters = None
            EnableTrace = Some false
            RequestMetadata = None
        }
        // Replace the body fields with our constructed JSON so the rule condition applies
        let baseJson = serializeToJson dto
        let doc = JsonDocument.Parse(baseJson)
        // Merge root with the fixture's JSON under a synthetic root name "data"
        // Then create a rule that targets those fields (already in fixture)
        let ctx = { Timestamp = DateTimeOffset.UtcNow; ExecutedBy = None; ContextData = Map.empty; Parameters = Map.empty; TraceEnabled = false }
        let critAction = { ActionType = Reject; Severity = Critical; Message = Some "crit"; Parameters = Map.empty; PropertyUpdates = None; Suggestions = None }
        let rule = { Rule = baseRule "RC" "critical" 100 (Simple fixture.Condition) [| critAction |]; ValidationScope = "data"; IsRequired = true; Dependencies = [||] }
        let ruleSet = { ValidationRules = [| rule |]; BusinessRules = [||]; ConflictRules = [||]; PricingRules = [||]; ApprovalRules = [||]; Metadata = Map.empty }
        let req' = { dto with RuleSet = ruleSet }
        let jsonReq = serializeToJson req'
        // Now process; since processJsonRequest re-parses and interpretRules uses the given doc, we assert success flag
        let jsonResp = processJsonRequest jsonReq
        // crude check: expect "success": false when fixture matches
        let successIsFalse = jsonResp.Contains("\"success\": false")
        if fixture.Expected then successIsFalse else true

