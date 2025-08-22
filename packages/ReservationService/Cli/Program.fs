namespace ReservationService.Cli

open System
open ReservationService.Core.ReservationServiceMain
open ReservationService.Core.JsonSerialization

module Program =
    [<EntryPoint>]
    let main argv =
        printfn "ReservationService - Rule-Based Interpreter"
        printfn "=========================================="

        match argv with
        | [| "health" |] ->
            printfn "Service Health:"
            let health = getServiceHealth()
            printfn "%s" health
            0

        | [| "example" |] ->
            printfn "Example Request Format:"
            let example = getExampleRequest()
            printfn "%s" example
            0

        | [| "process"; jsonFile |] ->
            try
                let jsonContent = System.IO.File.ReadAllText jsonFile
                // Expect the provided JSON to include the rule set and parameters; use defaults only for tracing/batching flags
                let config = defaultConfiguration

                printfn "Processing reservation request from: %s" jsonFile
                let result = processJsonRequest jsonContent
                printfn "Result:"
                printfn "%s" result
                0
            with
            | ex ->
                printfn "Error processing file: %s" ex.Message
                1

        | _ ->
            printfn "Usage:"
            printfn "  ReservationService health              - Check service health"
            printfn "  ReservationService example             - Show example request format"
            printfn "  ReservationService process <file.json> - Process reservation request"
            printfn ""
            printfn "This service is a stateless rule-based interpreter for reservation validation."
            1
