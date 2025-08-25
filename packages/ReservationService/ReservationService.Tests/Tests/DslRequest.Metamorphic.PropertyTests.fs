namespace ReservationService.Tests.Tests

module DslRequestMetamorphicPropertyTests =
    open System
    open System.Text.Json
    open System.Text.Json.Nodes
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open FsCheck.FSharp

    open ReservationService.Core.Dsl.Types
    open ReservationService.Core.JsonSerialization

    // Bring in generators from fuzz suite for data
    module Fuzz =
        open DslRequestFuzzPropertyTests
        let genData = DslRequestFuzzPropertyTests.G.genData

    // Small helpers
    let jv (s:string) : JsonNode = JsonValue.Create(s) :> JsonNode

    let deepClone (o: JsonObject) : JsonObject =
        JsonNode.Parse(o.ToJsonString()) :?> JsonObject

    type Canon = { HasCritical: bool; Phase: string; Req:Set<string>; Rem:Set<string>; Comp:Set<string> }
    let canonicalize (json:string) : Canon =
        use doc = JsonDocument.Parse(json)
        let root = doc.RootElement
        let hasCrit = root.GetProperty("hasCriticalErrors").GetBoolean()
        let phase = root.GetProperty("phase").GetProperty("Case").GetString()
        let setIds (prop:string) =
            root.GetProperty(prop).EnumerateArray() |> Seq.map (fun e -> e.GetProperty("id").GetString()) |> Set.ofSeq
        let req = setIds "requiredValidations"
        let rem = setIds "remainingValidations"
        let comp =
            root.GetProperty("completedValidations").EnumerateArray()
            |> Seq.map (fun e -> e.GetProperty("descriptor").GetProperty("id").GetString())
            |> Set.ofSeq
        { HasCritical = hasCrit; Phase = phase; Req=req; Rem=rem; Comp=comp }

    let mkRule (id:string) (name:string) (cond:Cond) (severity:string) : RuleDef =
        let action : ActionDef = { Type = "Validation"; Severity = Some severity; Code = id; Params=None; PropertyUpdates=None }
        { Id = id; Name = name; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond; Actions=[| action |]; Kind=Some "Validation"; Scope=Some "reservation" }

    let serializeReq (data: JsonObject) (rules: RuleDef array) : string =
        let req : Request = { SchemaVersion = "1.0"; Settings=None; Domain=None; Data = data; Functions=None; Rules=rules; Messages=None }
        JsonSerialization.serializeToJson req

    let messagesForRuleId (jsonResp:string) (ruleId:string) : int * bool * Set<string> =
        use doc = JsonDocument.Parse(jsonResp)
        let root = doc.RootElement
        let msgs = root.GetProperty("messages").EnumerateArray()
        let filtered = msgs |> Seq.filter (fun m -> let mutable tmp = Unchecked.defaultof<JsonElement> in m.TryGetProperty("reservationId", &tmp) && m.GetProperty("reservationId").GetString() = ruleId)
        let count = filtered |> Seq.length
        let hasCrit =
            filtered
            |> Seq.exists (fun m -> let mutable tmp = Unchecked.defaultof<JsonElement> in m.TryGetProperty("severity", &tmp) && m.GetProperty("severity").GetProperty("Case").GetString() = "Critical")
        let sevSet = filtered |> Seq.map (fun m -> m.GetProperty("severity").GetProperty("Case").GetString()) |> Set.ofSeq
        count, hasCrit, sevSet

    // 1) Commutativity: permuting children of And/Or does not change match result for that rule
    [<Property(MaxTest=500)>]
    let ``Permuting children of And/Or yields same match`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                // Build three simple conditions over fields present in data
                let c1 = Simple { Field = "reservation.partySize"; Operator = ">="; Value = Some (VInt 1); ReferenceField = None }
                let c2 = Simple { Field = "status"; Operator = "IsNotNull"; Value = None; ReferenceField = None }
                let c3 = Simple { Field = "selectedZones"; Operator = "Contains"; Value = Some (VString "vip"); ReferenceField = None }
                let! op = Gen.elements ["And"; "Or"]
                let children = [| c1; c2; c3 |]
                let perm = [| c2; c3; c1 |]
                let cond1 = Composite { Op = op; Children = children }
                let cond2 = Composite { Op = op; Children = perm }
                let r1 = mkRule "META-ORDER" "order" cond1 "Info"
                let r2 = mkRule "META-ORDER" "order" cond2 "Info"
                let req1 = serializeReq data [| r1 |]
                let req2 = serializeReq data [| r2 |]
                return req1, req2
            }
        let arb = Arb.fromGen gen
        Prop.forAll arb (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            let c1, _, _ = messagesForRuleId j1 "META-ORDER"
            let c2, _, _ = messagesForRuleId j2 "META-ORDER"
            c1 = c2
        )

    // 2) De Morgan: Not(And(a,b)) == Or(Not a, Not b)
    [<Property(MaxTest=500)>]
    let ``De Morgan equivalence holds`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                let a = Simple { Field = "reservation.partySize"; Operator = ">="; Value = Some (VInt 2); ReferenceField = None }
                let b = Simple { Field = "reservation.partySize"; Operator = "<"; Value = Some (VInt 10); ReferenceField = None }
                let lhs = Composite { Op = "Not"; Children = [| Composite { Op = "And"; Children = [| a; b |] } |] }
                let rhs = Composite { Op = "Or"; Children = [| Composite { Op = "Not"; Children = [| a |] }; Composite { Op = "Not"; Children = [| b |] } |] }
                let r1 = mkRule "META-DEMORGAN" "dm" lhs "Info"
                let r2 = mkRule "META-DEMORGAN" "dm" rhs "Info"
                let req1 = serializeReq data [| r1 |]
                let req2 = serializeReq data [| r2 |]
                return req1, req2
            }
        let arb = Arb.fromGen gen
        Prop.forAll arb (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            let c1, _, _ = messagesForRuleId j1 "META-DEMORGAN"
            let c2, _, _ = messagesForRuleId j2 "META-DEMORGAN"
            c1 = c2
        )

    // 3) Monotonicity: duplicating a rule does not change hasCriticalErrors truth value
    [<Property(MaxTest=500)>]
    let ``Duplicating a rule preserves hasCriticalErrors truth value`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                // Rule might match or not; we only check monotonicity of the boolean
                let cond = Simple { Field = "selectedZones"; Operator = "Contains"; Value = Some (VString "vip"); ReferenceField = None }
                let r = mkRule "META-DUP" "dup" cond "Critical"
                let req1 = serializeReq data [| r |]
                let req2 = serializeReq data [| r; r |]
                return req1, req2
            }
        let arb = Arb.fromGen gen
        Prop.forAll arb (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            let hasCrit1 = j1.Contains("\"hasCriticalErrors\": true")
            let hasCrit2 = j2.Contains("\"hasCriticalErrors\": true")
            hasCrit1 = hasCrit2
        )

    // 4) Idempotence: processing same request yields equivalent canonical output (not raw JSON)
    [<Property(MaxTest=500)>]
    let ``Processing is idempotent (canonical equivalence)`` () =
        let arb = Arb.fromGen (Fuzz.genData |> Gen.map (fun data -> serializeReq data [||]))
        Prop.forAll arb (fun req ->
            let c1 = JsonSerialization.processJsonRequest req |> canonicalize
            let c2 = JsonSerialization.processJsonRequest req |> canonicalize
            c1 = c2
        )

    // 5) Moving a validation from remaining -> completed updates output sets accordingly
    [<Property(MaxTest=500)>]
    let ``Moving validation from remaining to completed updates sets`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                // Deep clone to safely mutate
                let data1 = deepClone data
                let remaining = data1["remainingValidations"] :?> JsonArray
                let completed = data1["completedValidations"] :?> JsonArray
                // Start state uses PaymentCheck in remaining
                let req1 = serializeReq data1 [||]
                // Mutate: remove PaymentCheck from remaining and add to completed
                let idx = remaining |> Seq.cast<JsonNode> |> Seq.tryFindIndex (fun n -> (n :?> JsonValue).GetValue<string>() = "PaymentCheck")
                match idx with
                | Some i -> remaining.RemoveAt(i)
                | None -> ()
                completed.Add(jv "PaymentCheck")
                let req2 = serializeReq data1 [||]
                return req1, req2
            }
        let arb = Arb.fromGen gen
        Prop.forAll arb (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            // After move, remaining should not contain PaymentCheck and completed should
            let contains (json:string) (path:string) (value:string) =
                use doc = JsonDocument.Parse(json)
                let root = doc.RootElement
                let arr = root.GetProperty(path).EnumerateArray()
                arr |> Seq.exists (fun e -> e.GetProperty("id").GetString() = value)
            let rem1 = contains j1 "remainingValidations" "PaymentCheck"
            let rem2 = contains j2 "remainingValidations" "PaymentCheck"
            let comp2 =
                use doc = JsonDocument.Parse(j2)
                let root = doc.RootElement
                root.GetProperty("completedValidations").EnumerateArray()
                |> Seq.exists (fun e -> e.GetProperty("descriptor").GetProperty("id").GetString() = "PaymentCheck")
            rem1 && (not rem2) && comp2
        )



    // 6) Double negation: Not(Not c) ≡ c
    [<Property(MaxTest=500)>]
    let ``Double negation equivalence holds`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                let c = Simple { Field = "status"; Operator = "IsNotNull"; Value=None; ReferenceField=None }
                let lhs = Composite { Op = "Not"; Children = [| Composite { Op = "Not"; Children = [| c |] } |] }
                let rhs = c
                let r1 = mkRule "META-DOUBLE-NOT" "dn" lhs "Info"
                let r2 = mkRule "META-DOUBLE-NOT" "dn" rhs "Info"
                let req1 = serializeReq data [| r1 |]
                let req2 = serializeReq data [| r2 |]
                return req1, req2
            }
        Prop.forAll (Arb.fromGen gen) (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            let c1,_,_ = messagesForRuleId j1 "META-DOUBLE-NOT"
            let c2,_,_ = messagesForRuleId j2 "META-DOUBLE-NOT"
            c1 = c2)

    // 7) De Morgan second direction: Not(Or(a,b)) == And(Not a, Not b)
    [<Property(MaxTest=500)>]
    let ``De Morgan (Or) equivalence holds`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                let a = Simple { Field = "reservation.partySize"; Operator = ">="; Value = Some (VInt 2); ReferenceField = None }
                let b = Simple { Field = "reservation.partySize"; Operator = "<"; Value = Some (VInt 10); ReferenceField = None }
                let lhs = Composite { Op = "Not"; Children = [| Composite { Op = "Or"; Children = [| a; b |] } |] }
                let rhs = Composite { Op = "And"; Children = [| Composite { Op = "Not"; Children = [| a |] }; Composite { Op = "Not"; Children = [| b |] } |] }
                let r1 = mkRule "META-DEMORGAN-OR" "dm2" lhs "Info"
                let r2 = mkRule "META-DEMORGAN-OR" "dm2" rhs "Info"
                let req1 = serializeReq data [| r1 |]
                let req2 = serializeReq data [| r2 |]
                return req1, req2
            }
        Prop.forAll (Arb.fromGen gen) (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            let c1,_,_ = messagesForRuleId j1 "META-DEMORGAN-OR"
            let c2,_,_ = messagesForRuleId j2 "META-DEMORGAN-OR"
            c1 = c2)

    // 8) Associativity: And(And(a,b),c) == And(a,And(b,c))
    [<Property(MaxTest=500)>]
    let ``Associativity for And holds`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                let a = Simple { Field = "status"; Operator = "IsNotNull"; Value=None; ReferenceField=None }
                let b = Simple { Field = "selectedZones"; Operator = "Contains"; Value = Some (VString "vip"); ReferenceField=None }
                let c = Simple { Field = "reservation.partySize"; Operator = ">="; Value = Some (VInt 1); ReferenceField=None }
                let lhs = Composite { Op = "And"; Children = [| Composite { Op = "And"; Children = [| a; b |] }; c |] }
                let rhs = Composite { Op = "And"; Children = [| a; Composite { Op = "And"; Children = [| b; c |] } |] }
                let r1 = mkRule "META-ASSOC-AND" "assoc" lhs "Info"
                let r2 = mkRule "META-ASSOC-AND" "assoc" rhs "Info"
                let req1 = serializeReq data [| r1 |]
                let req2 = serializeReq data [| r2 |]
                return req1, req2
            }
        Prop.forAll (Arb.fromGen gen) (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            let c1,_,_ = messagesForRuleId j1 "META-ASSOC-AND"
            let c2,_,_ = messagesForRuleId j2 "META-ASSOC-AND"
            c1 = c2)

    // 9) Associativity: Or(Or(a,b),c) == Or(a,Or(b,c))
    [<Property(MaxTest=500)>]
    let ``Associativity for Or holds`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                let a = Simple { Field = "status"; Operator = "IsNotNull"; Value=None; ReferenceField=None }
                let b = Simple { Field = "selectedZones"; Operator = "Contains"; Value = Some (VString "vip"); ReferenceField=None }
                let c = Simple { Field = "reservation.partySize"; Operator = ">="; Value = Some (VInt 1); ReferenceField=None }
                let lhs = Composite { Op = "Or"; Children = [| Composite { Op = "Or"; Children = [| a; b |] }; c |] }
                let rhs = Composite { Op = "Or"; Children = [| a; Composite { Op = "Or"; Children = [| b; c |] } |] }
                let r1 = mkRule "META-ASSOC-OR" "assoc" lhs "Info"
                let r2 = mkRule "META-ASSOC-OR" "assoc" rhs "Info"
                let req1 = serializeReq data [| r1 |]
                let req2 = serializeReq data [| r2 |]
                return req1, req2
            }
        Prop.forAll (Arb.fromGen gen) (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            let c1,_,_ = messagesForRuleId j1 "META-ASSOC-OR"
            let c2,_,_ = messagesForRuleId j2 "META-ASSOC-OR"
            c1 = c2)

    // 10) Order-insensitive arrays for set operators: permuting selectedZones keeps canonical output the same
    [<Property(MaxTest=500)>]
    let ``Permuting selectedZones order keeps evaluation semantics`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                let rule = mkRule "META-SET-ORDER" "set" (Simple { Field = "selectedZones"; Operator = "Contains"; Value=Some (VString "vip"); ReferenceField=None }) "Critical"
                let req1 = serializeReq data [| rule |]
                // reverse selectedZones
                let data2 = deepClone data
                let sel = data2["selectedZones"] :?> JsonArray
                let rev = JsonArray()
                sel |> Seq.cast<JsonNode> |> Seq.rev |> Seq.iter (fun n -> rev.Add(JsonNode.Parse(n.ToJsonString())))
                data2["selectedZones"] <- rev
                let req2 = serializeReq data2 [| rule |]
                return req1, req2
            }
        Prop.forAll (Arb.fromGen gen) (fun (req1, req2) ->
            let c1 = JsonSerialization.processJsonRequest req1 |> canonicalize
            let c2 = JsonSerialization.processJsonRequest req2 |> canonicalize
            c1 = c2)

    // 11) Value vs ReferenceField equivalence when reference resolves to same literal value
    [<Property(MaxTest=500)>]
    let ``Value equals ReferenceField when alias field mirrors the value`` () =
        let gen =
            gen {
                let! data = Fuzz.genData
                // clone and add alias field mirroring partySize
                let data1 = deepClone data
                let res = data1["reservation"] :?> JsonObject
                let party = (res["partySize"] :?> JsonValue).GetValue<int>()
                res["aliasPartySize"] <- JsonValue.Create(party) :> JsonNode
                let rValue = mkRule "META-VS-REF" "vr" (Simple { Field = "reservation.partySize"; Operator = "=="; Value=Some (VInt party); ReferenceField=None }) "Info"
                let rRef = mkRule "META-VS-REF" "vr" (Simple { Field = "reservation.partySize"; Operator = "=="; Value=None; ReferenceField=Some "reservation.aliasPartySize" }) "Info"
                let req1 = serializeReq data1 [| rValue |]
                let req2 = serializeReq data1 [| rRef |]
                return req1, req2
            }
        Prop.forAll (Arb.fromGen gen) (fun (req1, req2) ->
            let j1 = JsonSerialization.processJsonRequest req1
            let j2 = JsonSerialization.processJsonRequest req2
            let c1,_,_ = messagesForRuleId j1 "META-VS-REF"
            let c2,_,_ = messagesForRuleId j2 "META-VS-REF"
            c1 = c2)
