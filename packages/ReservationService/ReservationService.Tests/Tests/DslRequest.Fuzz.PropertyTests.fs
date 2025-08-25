namespace ReservationService.Tests.Tests

module DslRequestFuzzPropertyTests =
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

    // ---------- Small JSON helpers ----------
    let private jv (s:string) : JsonNode = JsonValue.Create(s) :> JsonNode
    let private jvi (i:int) : JsonNode = JsonValue.Create(i) :> JsonNode
    let private jvd (d:decimal) : JsonNode = JsonValue.Create(d) :> JsonNode
    let private jvb (b:bool) : JsonNode = JsonValue.Create(b) :> JsonNode

    // ---------- Generators ----------
    module G =
        let genAsciiChar : Gen<char> = Gen.elements ([ 'a'..'z' ] @ [ 'A'..'Z' ] @ [ '0'..'9' ] @ [ '_'; '-' ])
        let genFieldName : Gen<string> =
            gen {
                let! len = Gen.choose(3, 10)
                let! cs = Gen.listOfLength len genAsciiChar
                return new string (List.toArray cs)
            }
        let genString : Gen<string> =
            gen {
                let! len = Gen.choose(1, 18)
                let! cs = Gen.listOfLength len genAsciiChar
                return new string (List.toArray cs)
            }
        let genBool : Gen<bool> = Gen.elements [true; false]
        let genInt : Gen<int> = Gen.choose(0, 10000)
        let genDec : Gen<decimal> = Gen.map decimal (Gen.choose(0, 100000))
        let genDateTime : Gen<DateTime> =
            gen {
                let baseDt = DateTime(2024,1,1,0,0,0,DateTimeKind.Utc)
                let! d = Gen.choose(0, 365)
                let! m = Gen.choose(0, 24*60)
                return baseDt.AddDays(float d).AddMinutes(float m)
            }

        let rec genDslValue depth : Gen<DslValue> =
            let scalars = [
                Gen.map VString genString
                Gen.map VInt genInt
                Gen.map VDecimal genDec
                Gen.map VBool genBool
                Gen.map VDateTime genDateTime
                Gen.constant VNull
            ]
            if depth <= 0 then Gen.oneof scalars else
            Gen.frequency [
                (5, Gen.oneof scalars)
                (3, gen {
                    let! n = Gen.choose(0, 5)
                    let! arr = Gen.arrayOfLength n (genDslValue (depth-1))
                    return VArray arr })
                (2, gen {
                    let! n = Gen.choose(0, 5)
                    let! keys = Gen.arrayOfLength n genFieldName
                    let! vals = Gen.arrayOfLength n (genDslValue (depth-1))
                    let mp = Array.zip keys vals |> Map.ofArray
                    return VObject mp })
            ]

        // Generate a simple or composite condition with some chance of referenceField
        let rec genCond depth : Gen<Cond> =
            let genSimple = gen {
                let! field = Gen.elements [
                    "selectedZones";
                    "reservation.partySize";
                    "status"; "nullable"; "price";
                    "resources.labels"; "resources.freeTables";
                    "reservation.end"; "reservation.start";
                    // deeper paths ensured by genData
                    "resources.areas.0.tables.1.seats";
                    "resources.areas.1.labels";
                    "resources.matrix.1.0";
                    "resources.meta.path.segment1.segment2";
                    "reservation.guests.0.age";
                    "reservation.guests.1.flags.0"
                ]
                let! op = Gen.elements [ "=="; "!="; ">"; ">="; "<"; "<="; "Contains"; "StartsWith"; "EndsWith"; "In"; "NotIn"; "IsNull"; "IsNotNull" ]
                let! byRef = Gen.elements [true; false]
                if op = "IsNull" || op = "IsNotNull" then
                    return Simple { Field = field; Operator = op; Value = None; ReferenceField = None }
                else if byRef then
                    let! refField = Gen.elements [
                        "selectedZones"; "allowedZones"; "threshold"; "thresholdMax";
                        "reservation.start"; "reservation.end";
                        "resources.labels"; "resources.freeTables";
                        "resources.areas.1.labels"; "resources.matrix.0"
                    ]
                    return Simple { Field = field; Operator = op; Value = None; ReferenceField = Some refField }
                else
                    let! v = genDslValue 2
                    return Simple { Field = field; Operator = op; Value = Some v; ReferenceField = None }
            }
            if depth <= 0 then genSimple else
            Gen.frequency [
                (5, genSimple)
                (3, gen {
                    let! c1 = genCond (depth-1)
                    let! c2 = genCond (depth-1)
                    return Composite { Op = "And"; Children = [| c1; c2 |] } })
                (2, gen {
                    let! c1 = genCond (depth-1)
                    let! c2 = genCond (depth-1)
                    return Composite { Op = "Or"; Children = [| c1; c2 |] } })
                (2, gen {
                    let! c1 = genCond (depth-1)
                    return Composite { Op = "Not"; Children = [| c1 |] } })
            ]

        let genAction : Gen<ActionDef> =
            gen {
                let! sev = Gen.elements [ None; Some "Info"; Some "Warning"; Some "Error"; Some "Critical" ]
                let! code = genString
                return { Type = "Validation"; Severity = sev; Code = code; Params=None; PropertyUpdates=None }
            }

        let genRule : Gen<RuleDef> =
            gen {
                let! id = genString
                let! name = genString
                let! prio = Gen.choose(1, 200)
                let! enabled = genBool
                let! cond = genCond 3
                let! nActs = Gen.choose(1, 3)
                let! acts = Gen.arrayOfLength nActs genAction
                return { Id = id; Name = name; Description=None; Priority=Some prio; Enabled=Some enabled; Tags=None; Condition=cond; Actions=acts; Kind=Some "Validation"; Scope=Some "reservation" }
            }

        // Data generator producing necessary fields plus random extras
        let genData : Gen<JsonObject> =
            gen {
                let o = JsonObject()
                // Ensure presence of fields many rules reference
                let! party = Gen.choose(0, 12)
                let! selected = Gen.arrayOfLength 3 (Gen.elements ["main"; "vip"; "family"; "accessible"]) 
                let! allowed = Gen.arrayOfLength 3 (Gen.elements ["main"; "vip"; "family"; "accessible"]) 
                let! labels = Gen.arrayOfLength 3 (Gen.elements ["T-1"; "T-2"; "VIP-3"; "A-4"]) 
                let! freeTables = Gen.arrayOfLength 4 (Gen.choose(0, 10))
                let! price = genDec
                let! threshold = Gen.choose(0, 10)
                let! thresholdMax = Gen.choose(5, 20)
                // Prepare deeper structures sizes (ensure >= 2 for index 1 references)
                let! nAreas = Gen.choose(2, 3)
                let! nTablesPerArea = Gen.choose(2, 4)
                let! nFlags = Gen.choose(2, 3)
                let! nTags = Gen.choose(2, 3)
                let! nRows = Gen.choose(2, 3)
                let! nCols = Gen.choose(2, 3)
                let! nGuests = Gen.choose(2, 4)
                let baseDt = DateTime(2025,8,1,0,0,0,DateTimeKind.Utc)
                let! plusMins = Gen.choose(0, 240)
                let startDt = baseDt.AddMinutes(float plusMins)
                let endDt = startDt.AddMinutes(float (30 + (plusMins % 60)))
                let res = JsonObject()
                res["start"] <- jv (startDt.ToString("o"))
                res["end"] <- jv (endDt.ToString("o"))
                res["timeZone"] <- jv "UTC"
                res["partySize"] <- jvi party
                res["notes"] <- jv "note"
                o["reservation"] <- res
                let selArr = JsonArray()
                selected |> Array.iter (fun s -> selArr.Add(jv s))
                o["selectedZones"] <- selArr
                let allowedArr = JsonArray()
                allowed |> Array.iter (fun s -> allowedArr.Add(jv s))
                o["allowedZones"] <- allowedArr
                let labelsArr = JsonArray()
                labels |> Array.iter (fun s -> labelsArr.Add(jv s))
                o["resources"] <- JsonObject()
                let resObj = (o["resources"] :?> JsonObject)
                resObj["labels"] <- labelsArr
                let ftArr = JsonArray()
                freeTables |> Array.iter (fun n -> ftArr.Add(jvi n))
                resObj["freeTables"] <- ftArr
                // Deep: areas with tables, nested flags/tags
                let areas = JsonArray()
                for ai in 0 .. (nAreas-1) do
                    let area = JsonObject()
                    area["name"] <- jv (sprintf "area-%d" ai)
                    // tables
                    let tables = JsonArray()
                    for ti in 0 .. (nTablesPerArea-1) do
                        let tbl = JsonObject()
                        tbl["id"] <- jvi (ai*10 + ti)
                        let! seats = Gen.choose(1, 10)
                        tbl["seats"] <- jvi seats
                        tables.Add(tbl)
                    area["tables"] <- tables
                    // labels per area
                    let alabels = JsonArray()
                    labels |> Array.iter (fun s -> alabels.Add(jv s))
                    area["labels"] <- alabels
                    // nested flags/tags
                    let nested = JsonObject()
                    let flags = JsonArray()
                    for fi in 0 .. (nFlags-1) do flags.Add(jvb (fi % 2 = 0))
                    let tags = JsonArray()
                    for ti in 0 .. (nTags-1) do tags.Add(jv (sprintf "tag-%d" ti))
                    nested["flags"] <- flags
                    nested["tags"] <- tags
                    area["nested"] <- nested
                    areas.Add(area)
                resObj["areas"] <- areas
                // matrix: array of int arrays
                let matrix = JsonArray()
                for r in 0 .. (nRows-1) do
                    let row = JsonArray()
                    for c in 0 .. (nCols-1) do row.Add(jvi (r*10 + c))
                    matrix.Add(row)
                resObj["matrix"] <- matrix
                // meta deep
                let meta = JsonObject()
                let path = JsonObject()
                let seg1 = JsonObject()
                seg1["segment2"] <- jv "leaf"
                path["segment1"] <- seg1
                meta["path"] <- path
                meta["version"] <- jvi 1
                resObj["meta"] <- meta
                // Deep guests under reservation
                let guests = JsonArray()
                for gi in 0 .. (nGuests-1) do
                    let g = JsonObject()
                    let! age = Gen.choose(1, 90)
                    g["age"] <- jvi age
                    let gflags = JsonArray()
                    for fi in 0 .. (nFlags-1) do gflags.Add(jvb (fi % 2 = 1))
                    g["flags"] <- gflags
                    guests.Add(g)
                res["guests"] <- guests
                o["threshold"] <- jvi threshold
                o["thresholdMax"] <- jvi thresholdMax
                o["status"] <- jv "PENDING"
                o["nullable"] <- null
                o["price"] <- jvd price
                // Validation IO lists
                o["requiredValidations"] <- JsonArray([| jv "AvailabilityCheck"; jv "PaymentCheck" |])
                o["completedValidations"] <- JsonArray([| jv "AvailabilityCheck" |])
                o["remainingValidations"] <- JsonArray([| jv "PaymentCheck" |])
                // ticketIds and tickets
                o["ticketIds"] <- JsonArray([| jv "TKT-"; jv "TKT-XYZ" |])
                let t1 = JsonObject()
                t1["id"] <- jv "ALT-1"
                let t2 = JsonObject()
                t2["id"] <- jv "ALT-2"
                let tickets = JsonArray()
                tickets.Add(t1)
                tickets.Add(t2)
                o["tickets"] <- tickets
                // Random extra fields
                let! extras = Gen.choose(0, 5)
                let! keys = Gen.arrayOfLength extras genString
                let! vals = Gen.arrayOfLength extras genString
                Array.zip keys vals |> Array.iter (fun (k,v) -> o[k] <- (JsonValue.Create(v) :> JsonNode))
                return o
            }

        let genRequest : Gen<string> =
            gen {
                let! data = genData
                let! nRules = Gen.choose(1, 10)
                let! rules = Gen.arrayOfLength nRules genRule
                let req : Request = { SchemaVersion = "1.0"; Settings=None; Domain=None; Data = data; Functions=None; Rules=rules; Messages=None }
                return JsonSerialization.serializeToJson req
            }

    // ---------- Properties ----------

    // Invariant: processing a random request never throws and returns a valid output with consistent validation lists
    [<Property(MaxTest = 1500, EndSize = 300)>]
    let ``Fuzz: processing returns valid JSON and consistent lists`` () =
        let arb = Arb.fromGen G.genRequest
        Prop.forAll arb (fun jsonReq ->
            let jsonResp = ReservationService.Tests.TestDebug.processWithDebug "Fuzz: Consistency" (jsonReq : string)
            try
                use doc = JsonDocument.Parse(jsonResp)
                let root = doc.RootElement
                let hasProp (name:string) =
                    let mutable p = Unchecked.defaultof<JsonElement>
                    root.TryGetProperty(name, &p)
                hasProp "phase" && hasProp "requiredValidations" && hasProp "remainingValidations" && hasProp "completedValidations"
            with _ -> false
        )

    // Invariant: remaining/completed subset of required and disjoint
    [<Property(MaxTest = 1500, EndSize = 300)>]
    let ``Fuzz: remaining and completed are subsets of required, and disjoint`` () =
        let arb = Arb.fromGen G.genRequest
        Prop.forAll arb (fun jsonReq ->
            let jsonResp = ReservationService.Tests.TestDebug.processWithDebug "Fuzz: SubsetDisjoint" (jsonReq : string)
            try
                use doc = JsonDocument.Parse(jsonResp)
                let root = doc.RootElement
                let idsOf (arr: JsonElement) (selector: JsonElement -> string) : Set<string> =
                    arr.EnumerateArray() |> Seq.map selector |> Set.ofSeq
                let reqIds : Set<string> =
                    root.GetProperty("requiredValidations")
                    |> fun e -> idsOf e (fun v -> v.GetProperty("id").GetString())
                let remIds : Set<string> =
                    root.GetProperty("remainingValidations")
                    |> fun e -> idsOf e (fun v -> v.GetProperty("id").GetString())
                let compIds : Set<string> =
                    root.GetProperty("completedValidations")
                    |> fun e -> idsOf e (fun v -> v.GetProperty("descriptor").GetProperty("id").GetString())
                Set.isSubset remIds reqIds && Set.isSubset compIds reqIds && Set.isEmpty (Set.intersect remIds compIds)
            with _ -> false
        )

