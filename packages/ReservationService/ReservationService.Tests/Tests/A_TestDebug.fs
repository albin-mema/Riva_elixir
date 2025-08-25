namespace ReservationService.Tests

module TestDebug =
    open System
    open System.IO
    open System.Threading

    // Environment switches (set any to enable)
    //   FS_DEBUG_JSON_ALL=1        -> print both input and output, limited by FS_DEBUG_JSON_MAX
    //   FS_DEBUG_JSON_INPUT=1      -> print input JSON only
    //   FS_DEBUG_JSON_OUTPUT=1     -> print output JSON only
    //   FS_DEBUG_JSON_DIR=path     -> if set, also dump inputs/outputs as files under this directory
    //   FS_DEBUG_JSON_MAX=N        -> max number of debug print events per test run (default 5)

    let private envSet (name:string) : bool =
        match Environment.GetEnvironmentVariable(name) with
        | null | "" -> false
        | v when v = "1" || v.Equals("true", StringComparison.OrdinalIgnoreCase) -> true
        | _ -> false

    let private envStr (name:string) : string option =
        match Environment.GetEnvironmentVariable(name) with
        | null | "" -> None
        | v -> Some v

    let private envInt (name:string) (def:int) : int =
        match Environment.GetEnvironmentVariable(name) with
        | null | "" -> def
        | s -> match Int32.TryParse s with | true, v -> v | _ -> def

    // Limit noisy output in property runs
    let private printCounter = ref 0
    let private maxPrints () = envInt "FS_DEBUG_JSON_MAX" 5
    let private canPrint () =
        let i = Interlocked.Increment printCounter
        i <= maxPrints()

    let private dumpToDir (label:string) (kind:string) (json:string) =
        match envStr "FS_DEBUG_JSON_DIR" with
        | None -> ()
        | Some dir ->
            try
                Directory.CreateDirectory(dir) |> ignore
                let safeLabel = label.Replace(" ", "_")
                let ts = DateTimeOffset.UtcNow.ToString("yyyyMMdd_HHmmss_fffffff")
                let path = Path.Combine(dir, sprintf "%s_%s_%s.json" safeLabel kind ts)
                File.WriteAllText(path, json)
            with _ -> ()

    let processWithDebug (label:string) (jsonReq:string) : string =
        let showAll = envSet "FS_DEBUG_JSON_ALL"
        let showIn  = envSet "FS_DEBUG_JSON_INPUT"
        let showOut = envSet "FS_DEBUG_JSON_OUTPUT"
        let any = showAll || showIn || showOut || envStr "FS_DEBUG_JSON_DIR" |> Option.isSome
        if any && (showAll || showIn) && canPrint() then
            Console.WriteLine(sprintf "--- DEBUG JSON INPUT [%s] ---" label)
            Console.WriteLine(jsonReq)
            dumpToDir label "in" jsonReq
        let resp = ReservationService.Core.JsonSerialization.JsonSerialization.processJsonRequest jsonReq
        if any && (showAll || showOut) && canPrint() then
            Console.WriteLine(sprintf "--- DEBUG JSON OUTPUT [%s] ---" label)
            Console.WriteLine(resp)
            dumpToDir label "out" resp
        resp

