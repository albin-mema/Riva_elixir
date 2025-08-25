namespace ReservationService.Tests.Tests

module DslRequestPropertyTests =
    open System
    open System.Text.Json
    open System.Text.Json.Nodes
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open FsCheck.FSharp

    open ReservationService.Core.Dsl.Types
    open ReservationService.Core.JsonSerialization
    open ReservationService.Tests

    // ---------- Helpers to build JSON nodes ----------
    let private jv (s:string) : JsonNode = JsonValue.Create(s) :> JsonNode
    let private jvi (i:int) : JsonNode = JsonValue.Create(i) :> JsonNode
    let private jvd (d:decimal) : JsonNode = JsonValue.Create(d) :> JsonNode
    let private jvb (b:bool) : JsonNode = JsonValue.Create(b) :> JsonNode

    let private mkTimeRange (startDt: DateTime) (endDt: DateTime) : JsonObject =
        let tr = JsonObject()
        tr["start"] <- jv (startDt.ToString("o"))
        tr["end"] <- jv (endDt.ToString("o"))
        tr["timeZone"] <- jv "UTC"
        tr

    // ---------- Generators ----------
    module GenDsl =
        let genAsciiChar : Gen<char> = Gen.elements ([ 'a'..'z' ] @ [ 'A'..'Z' ] @ [ '0'..'9' ] @ [ '_'; '-' ])
        let genFieldName : Gen<string> =
            gen {
                let! len = Gen.choose(3, 12)
                let! chars = Gen.listOfLength len genAsciiChar
                return new string (List.toArray chars)
            }
        let genPlainString : Gen<string> =
            gen {
                let! len = Gen.choose(1, 12)
                let! chars = Gen.listOfLength len genAsciiChar
                return new string (List.toArray chars)
            }
        let genDateTime : Gen<DateTime> =
            gen {
                let epoch = DateTime(2024,1,1,0,0,0,DateTimeKind.Utc)
                let! days = Gen.choose(0, 365)
                let! mins = Gen.choose(0, 8*60)
                return epoch.AddDays(float days).AddMinutes(float mins)
            }
        let genZone : Gen<string> = Gen.elements [ "main"; "vip"; "family"; "accessible" ]
        let genSize : Gen<string> = Gen.elements [ "standard"; "vip"; "family"; "accessible" ]

        let genDslValueBasic : Gen<DslValue> =
            Gen.frequency [
                4, Gen.map VString genPlainString
                3, Gen.map VInt (Gen.choose(0, 500))
                2, Gen.map (decimal >> VDecimal) (Gen.choose(0, 10000))
                2, Gen.map VBool (Gen.elements [true; false])
                2, Gen.map VDateTime genDateTime
                1, Gen.constant VNull
            ]

        let genTimeRanges : Gen<JsonArray * (DateTime*DateTime)[]> =
            gen {
                let! n = Gen.choose(1, 4)
                let! starts = Gen.arrayOfLength n genDateTime
                let! durations' = Gen.arrayOfLength n (Gen.choose(30, 480))
                let durations : int[] = durations'
                let pairs : (DateTime*DateTime)[] =
                    Array.mapi (fun i (s:DateTime) -> let e = s.AddMinutes(float durations.[i]) in (s,e)) starts
                let arr = JsonArray()
                pairs |> Array.iter (fun (s,e) -> arr.Add(mkTimeRange s e))
                return arr, pairs
            }

        let genPeople : Gen<JsonArray * int> =
            gen {
                let! n = Gen.choose(1, 6)
                let arr = JsonArray()
                for _ in 1..n do
                    let! age = Gen.choose(1, 90)
                    let! hasSup = Gen.elements [true; false]
                    let! vip = Gen.elements [true; false]
                    let! acc = Gen.elements [true; false]
                    let p = JsonObject()
                    p["minAge"] <- jvi age
                    p["hasSupervision"] <- jvb hasSup
                    p["isVIP"] <- jvb vip
                    p["hasAccessibilityNeeds"] <- jvb acc
                    arr.Add(p)
                return arr, n
            }

        let genSelectedZones : Gen<JsonArray * string[]> =
            gen {
                let! n = Gen.choose(1, 3)
                let! zones = Gen.arrayOfLength n genZone
                let arr = JsonArray()
                zones |> Array.iter (fun z -> arr.Add(jv z))
                return arr, zones
            }

        let genUmbrellas : Gen<JsonArray> =
            gen {
                let! n = Gen.choose(1, 4)
                let arr = JsonArray()
                for _ in 1..n do
                    let! size = genSize
                    let! zone = genZone
                    let o = JsonObject()
                    o["size"] <- jv size
                    o["zone"] <- jv zone
                    arr.Add(o)
                return arr
            }

        // Build data JsonObject with multiple fields
        let genData : Gen<JsonObject * (DateTime*DateTime)[] * string[]> =
            gen {
                let o = JsonObject()
                let! (timeRanges, pairs) = genTimeRanges
                let! (people, peopleCount) = genPeople
                let! (zonesArr, zones) = genSelectedZones
                let! umbrellas = genUmbrellas
                let lead = JsonValue.Create(120) :> JsonNode
                o["timeRanges"] <- timeRanges
                // mirror first timeRange for legacy rules
                let (s0,e0) = pairs.[0]
                o["timeRange"] <- mkTimeRange s0 e0
                o["people"] <- people
                o["participants"] <- jvi peopleCount
                o["leadMinutes"] <- lead
                o["selectedZones"] <- zonesArr
                o["umbrellas"] <- umbrellas
                // resourceAvailability
                let ra = JsonObject()
                let mkZone cap = let z = JsonObject() in z["capacity"] <- jvi cap; z
                ra["main"] <- mkZone 20; ra["vip"] <- mkZone 8; ra["family"] <- mkZone 12; ra["accessible"] <- mkZone 6
                ra["totalCapacity"] <- jvi 46
                o["resourceAvailability"] <- ra
                return o, pairs, zones
            }

        // Build a DSL Request JSON string where a Critical rule will match (selectedZones contains a chosen zone)
        let genMatchingCriticalJson : Gen<string> =
            gen {
                let! (data, _pairs, zones) = genData
                let chosen = zones.[0]
                let rule : RuleDef =
                    let cond = Simple { Field = "selectedZones"; Operator = "Contains"; Value = Some (VString chosen); ReferenceField = None }
                    { Id = "CRIT-A"; Name = "zones must include chosen"; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond;
                      Actions=[| { Type = "Validation"; Severity=Some "Critical"; Code="CRIT-A"; Params=None; PropertyUpdates=None } |]; Kind=Some "Validation"; Scope=Some "reservation" }
                let req : Request = { SchemaVersion = "1.0"; Settings=None; Domain=None; Data = data; Functions=None; Rules=[| rule |]; Messages=None }
                return JsonSerialization.serializeToJson req
            }

        // Build a DSL Request JSON string where a Critical rule will NOT match (use a zone not present)
        let genNonMatchingCriticalJson : Gen<string> =
            gen {
                let! (data, _pairs, zones) = genData
                // pick a value not in zones
                let allZones = [| "main"; "vip"; "family"; "accessible" |]
                let absent = allZones |> Array.find (fun z -> zones |> Array.contains z |> not)
                let rule : RuleDef =
                    let cond = Simple { Field = "selectedZones"; Operator = "Contains"; Value = Some (VString absent); ReferenceField = None }
                    { Id = "CRIT-NO"; Name = "zones do not include absent"; Description=None; Priority=Some 100; Enabled=Some true; Tags=None; Condition=cond;
                      Actions=[| { Type = "Validation"; Severity=Some "Critical"; Code="CRIT-NO"; Params=None; PropertyUpdates=None } |]; Kind=Some "Validation"; Scope=Some "reservation" }
                let req : Request = { SchemaVersion = "1.0"; Settings=None; Domain=None; Data = data; Functions=None; Rules=[| rule |]; Messages=None }
                return JsonSerialization.serializeToJson req
            }

    // ---------- Properties ----------
    [<Property(MaxTest = 500)>]
    let ``Matching critical rule on array field yields hasCriticalErrors true`` () =
        let arb = Arb.fromGen GenDsl.genMatchingCriticalJson
        Prop.forAll arb (fun jsonReq ->
            let jsonResp = ReservationService.Tests.TestDebug.processWithDebug "Prop: MatchingCritical-Match" jsonReq
            jsonResp.Contains("\"hasCriticalErrors\": true")
        )

    [<Property(MaxTest = 500)>]
    let ``Non-matching critical rule yields hasCriticalErrors false`` () =
        let arb = Arb.fromGen GenDsl.genNonMatchingCriticalJson
        Prop.forAll arb (fun jsonReq ->
            let jsonResp = ReservationService.Tests.TestDebug.processWithDebug "Prop: MatchingCritical-NonMatch" jsonReq
            jsonResp.Contains("\"hasCriticalErrors\": false")
        )

