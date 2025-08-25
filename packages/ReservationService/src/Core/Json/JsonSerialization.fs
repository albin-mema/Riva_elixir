namespace ReservationService.Core.JsonSerialization

open System
open System.Text.Json
open System.Text.Json.Serialization
open ReservationService.Core

open ReservationService.Core.RuleDomain
open ReservationService.Core.RuleTypes
open ReservationService.Core.OutputTypes
open ReservationService.Core.Types

/// JSON serialization handling for the rule-based interpreter service
module JsonSerialization =

    // ========== INPUT REQUEST TYPES ==========
    open ReservationService.Core.Json.JsonConverters

    /// A single reservation request in the input JSON
    type JsonReservationRequest = {
        /// Unique identifier for this request
        Id: string
        /// Customer making the reservation
        CustomerId: string
        /// Business/organization the reservation is for
        BusinessId: string option
        /// Resource being reserved (room, equipment, etc.)
        ResourceId: string
        /// Type of resource
        ResourceType: string
        /// Requested time range
        TimeRange: JsonTimeRange
        /// Number of participants
        Participants: int option
        /// Services requested (catering, AV, etc.)
        Services: string[] option
        /// Special requests or notes
        SpecialRequests: string option
        /// Additional metadata
        Metadata: Map<string, JsonElement> option
    }

    /// Time range in JSON format
    and JsonTimeRange = {
        /// Start time (ISO 8601 format)
        Start: string
        /// End time (ISO 8601 format)
        End: string
        /// Timezone identifier
        TimeZone: string option
    }

    /// Context data available during rule processing
    type JsonContextData = {
        /// Existing reservations that might conflict
        ExistingReservations: JsonReservationRequest[] option
        /// Resource availability information
        ResourceAvailability: Map<string, JsonElement> option
        /// Business calendar (holidays, blackouts, etc.)
        BusinessCalendar: Map<string, JsonElement> option
        /// Participant restrictions and information
        ParticipantRestrictions: Map<string, JsonElement> option
        /// Current resource utilization
        ResourceUtilization: Map<string, JsonElement> option
        /// Any other context data
        AdditionalContext: Map<string, JsonElement> option
    }

    /// Complete input request to the rule interpreter
    type JsonRuleInterpretationRequest = {
        /// One or more reservation requests to process
        ReservationRequests: JsonReservationRequest[]
        /// Rule set to apply
        RuleSet: RuleTypes.RuleSet
        /// Context data for rule processing
        ContextData: JsonContextData option
        /// Configuration parameters
        RuleParameters: Map<string, JsonElement> option
        /// Whether to enable tracing
        EnableTrace: bool option
        /// Request metadata
        RequestMetadata: Map<string, JsonElement> option
    }

    // ========== JSON SERIALIZATION OPTIONS ==========

    /// JSON serializer options for the service
    let jsonOptions =
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        options.WriteIndented <- true
        options.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
        options.Converters.Add(JsonFSharpConverter())
        // Register custom converters that enforce constrained IO types on the wire
        ReservationService.Core.Json.JsonConverters.registerAll options
        options

    // ========== SERIALIZATION FUNCTIONS ==========

    /// Serialize an object to JSON string
    let serializeToJson<'T> (obj: 'T) : string =
        JsonSerializer.Serialize(obj, jsonOptions)

    /// Deserialize JSON string to object
    let deserializeFromJson<'T> (json: string) : Result<'T, string> =
        try
            let result = JsonSerializer.Deserialize<'T>(json, jsonOptions)
            Ok result
        with
        | ex -> Result.Error ex.Message

    // ========== RULE SERIALIZATION ==========

    /// Serialize a rule to JSON string
    let serializeRule (rule: RuleTypes.IRule) : string =
        JsonSerializer.Serialize(rule, jsonOptions)

    /// Deserialize a rule from JSON string
    let deserializeRule (json: string) : Result<RuleTypes.IRule, string> =
        try
            let result = JsonSerializer.Deserialize<RuleTypes.IRule>(json, jsonOptions)
            Ok result
        with
        | ex -> Result.Error ex.Message

    /// Parse JSON string to JsonDocument for rule processing
    let parseJsonDocument (json: string) : Result<JsonDocument, string> =
        try
            let document = JsonDocument.Parse(json)
            Ok document
        with
        | ex -> Result.Error ex.Message

    // ========== CONVERSION FUNCTIONS ==========

    /// Convert JsonTimeRange to DateTimeOffset range
    let convertTimeRange (timeRange: JsonTimeRange) : Result<DateTimeOffset * DateTimeOffset, string> =
        try
            let startTime = DateTimeOffset.Parse(timeRange.Start)
            let endTime = DateTimeOffset.Parse(timeRange.End)
            Ok (startTime, endTime)
        with
        | ex -> Result.Error ($"Invalid time range: {ex.Message}")

    /// Convert JsonReservationRequest to a format suitable for rule processing
    let convertReservationRequest (request: JsonReservationRequest) : Result<Map<string, obj>, string> =
        try
            let timeRangeResult = convertTimeRange request.TimeRange
            match timeRangeResult with
            | Ok (startTime, endTime) ->
                let converted = Map [
                    ("id", request.Id :> obj)
                    ("customerId", request.CustomerId :> obj)
                    ("resourceId", request.ResourceId :> obj)
                    ("resourceType", request.ResourceType :> obj)
                    ("timeRange", Map [
                        ("start", startTime :> obj)
                        ("end", endTime :> obj)
                        ("timeZone", request.TimeRange.TimeZone |> Option.defaultValue "UTC" :> obj)
                    ] :> obj)
                    ("participants", request.Participants |> Option.defaultValue 0 :> obj)
                    ("services", request.Services |> Option.defaultValue [||] :> obj)
                    ("specialRequests", request.SpecialRequests |> Option.defaultValue "" :> obj)
                ]

                // Add optional fields
                let withBusiness =
                    match request.BusinessId with
                    | Some businessId -> Map.add "businessId" (businessId :> obj) converted
                    | None -> converted

                let withMetadata =
                    match request.Metadata with
                    | Some metadata -> Map.add "metadata" (metadata :> obj) withBusiness
                    | None -> withBusiness

                Ok withMetadata
            | Result.Error msg -> Result.Error msg
        with
        | ex -> Result.Error ex.Message

    /// Convert a boxed value to RuleDomain.RuleValue (minimal support for tests)
    let convertRuleValueObj (v: obj) : RuleDomain.RuleValue =
        match v with
        | :? int as i -> RuleDomain.SInt i
        | :? decimal as d -> RuleDomain.SDecimal d
        | :? bool as b -> RuleDomain.SBool b
        | :? string as s -> RuleDomain.SString s
        | :? DateTime as dt -> RuleDomain.SDateTimeOffset (DateTimeOffset(dt))
        | :? DateTimeOffset as dto -> RuleDomain.SDateTimeOffset dto
        | null -> RuleDomain.SNull
        | _ -> RuleDomain.SString (v.ToString())

    // No SharedCore conversion here; RuleSets are constructed directly in tests for now.

    /// Create rule execution context from JSON request
    let createRuleExecutionContext (request: JsonRuleInterpretationRequest) : RuleTypes.RuleExecutionContext =
        let contextData =
            match request.ContextData with
            | Some context ->
                let mutable contextMap = Map.empty<string, string>

                // Add existing reservations
                match context.ExistingReservations with
                | Some reservations ->
                    let json = serializeToJson reservations
                    contextMap <- Map.add "existingReservations" json contextMap
                | None -> ()

                // Add resource availability
                match context.ResourceAvailability with
                | Some availability ->
                    let json = serializeToJson availability
                    contextMap <- Map.add "resourceAvailability" json contextMap
                | None -> ()

                // Add business calendar
                match context.BusinessCalendar with
                | Some calendar ->
                    let json = serializeToJson calendar
                    contextMap <- Map.add "businessCalendar" json contextMap
                | None -> ()

                contextMap
            | None -> Map.empty

        {
            TraceEnabled = request.EnableTrace |> Option.defaultValue false
            UserId = None
            SessionId = None
            AdditionalData = contextData
            CaseInsensitive = true
            TargetTimeZoneId = None
            CollectTrace = false
        }

    // ========== OUTPUT CONVERSION ==========

    /// Convert rule interpretation output to JSON response
    let convertOutputToJson (output: RuleInterpretationOutput) : string =
        serializeToJson output

    // Helper: extract ticket identifiers from the request data to echo in Metadata
    let private extractTicketIds (jsonData: JsonDocument) : string[] =
        try
            let root = jsonData.RootElement
            let hasProp, _ = root.TryGetProperty("ticketIds")
            if hasProp then
                let arr = root.GetProperty("ticketIds")
                if arr.ValueKind = JsonValueKind.Array then
                    arr.EnumerateArray()
                    |> Seq.choose (fun e -> if e.ValueKind = JsonValueKind.String then Some (e.GetString()) else None)
                    |> Seq.toArray
                else [||]
            else
                let hasTicketsProp, _ = root.TryGetProperty("tickets")
                if hasTicketsProp then
                    let arr = root.GetProperty("tickets")
                    if arr.ValueKind = JsonValueKind.Array then
                        arr.EnumerateArray()
                        |> Seq.choose (fun e ->
                            if e.ValueKind = JsonValueKind.Object then
                                let ok, idProp = e.TryGetProperty("id")
                                if ok && idProp.ValueKind = JsonValueKind.String then Some (idProp.GetString()) else None
                            else None)
                        |> Seq.toArray
                    else [||]
                else [||]
        with _ -> [||]

    // ========== MAIN PROCESSING FUNCTION ==========

    // Helper: try get string[] from a property on the root data
    let private tryGetStringArray (root: JsonElement) (propName: string) : string[] =
        match root.TryGetProperty(propName) with
        | true, arr when arr.ValueKind = JsonValueKind.Array ->
            arr.EnumerateArray()
            |> Seq.choose (fun e -> if e.ValueKind = JsonValueKind.String then Some (e.GetString()) else None)
            |> Seq.toArray
        | _ -> [||]

    // Build typed validation DTOs from input data
    let private toSeverityDto (s: MessageSeverity) : ValidationIo.SeverityDto =
        match s with
        | MessageSeverity.Info -> ValidationIo.SeverityDto.Info
        | MessageSeverity.Warning -> ValidationIo.SeverityDto.Warning
        | MessageSeverity.Error -> ValidationIo.SeverityDto.Error
        | MessageSeverity.Critical -> ValidationIo.SeverityDto.Critical

    let private tryNes (s:string) : NonEmptyString option =
        match NonEmptyString.Create s with
        | Result.Ok v -> Some v
        | Result.Error _ -> None

    let private toNesOpt (sOpt:string option) : NonEmptyString option =
        sOpt |> Option.bind tryNes

    let private mkDescriptor (name:string) (required:bool) : ValidationIo.ValidationDescriptorDto option =
        match tryNes name with
        | Some nes ->
            Some { Id = nes; Name = nes; Kind = None; Scope = None; Required = required; Dependencies = [||]; RuleRefs = [||]; DefaultSeverity = None }
        | None -> None

    let private mkInstanceSucceeded (name:string) : ValidationIo.ValidationInstanceDto option =
        mkDescriptor name true
        |> Option.map (fun d -> { Descriptor = d; Status = ValidationIo.ValidationStatusDto.Succeeded; Messages = [||]; LastUpdated = None })

    let private computeValidationDtos (jsonData: JsonDocument) : ValidationIo.ValidationDescriptorDto[] * ValidationIo.ValidationDescriptorDto[] * ValidationIo.ValidationInstanceDto[] =
        let root = jsonData.RootElement
        let requiredNames = tryGetStringArray root "requiredValidations"
        let completedNames = tryGetStringArray root "completedValidations"
        let requiredDescs = requiredNames |> Array.choose (fun n -> mkDescriptor n true)
        let remainingDescs =
            if requiredNames.Length = 0 then
                tryGetStringArray root "remainingValidations" |> Array.choose (fun n -> mkDescriptor n true)
            else
                let completedSet = completedNames |> Set.ofArray
                requiredNames
                |> Array.filter (fun n -> not (completedSet.Contains n))
                |> Array.choose (fun n -> mkDescriptor n true)
        let completedInst = completedNames |> Array.choose mkInstanceSucceeded
        (requiredDescs, remainingDescs, completedInst)

    let private buildErrorDtos (msgs: ValidationMessage[]) : ValidationIo.ValidationErrorDto[] =
        msgs
        |> Array.choose (fun m ->
            match m.Severity with
            | MessageSeverity.Error | MessageSeverity.Critical ->
                match tryNes m.Message with
                | Some nmsg ->
                    let code = NonEmptyString.CreateUnsafe "VALIDATION_ERROR"
                    let field = toNesOpt m.Field
                    let ruleId = m.RuleId |> Option.map (fun (Types.RuleId g) -> g)
                    Some { Code = code; Message = nmsg; Severity = toSeverityDto m.Severity; Field = field; RuleId = ruleId }
                | None -> None
            | _ -> None)

    /// Process a JSON request and return JSON response
    let processJsonRequest (jsonRequest: string) : string =
        try
            // New path: treat input as DSL JSON request
            let (rules, jsonData, ctx) = Dsl.Parser.compileRequest jsonRequest
            let results = RuleEngine.interpretRules rules jsonData ctx
            // Build messages from matched actions, preserving severities
            let toMsgSeverity = function
                | RuleSeverity.Info -> MessageSeverity.Info
                | RuleSeverity.Warning -> MessageSeverity.Warning
                | RuleSeverity.Error -> MessageSeverity.Error
                | RuleSeverity.Critical -> MessageSeverity.Critical
                | RuleSeverity.Unknown _ -> MessageSeverity.Warning
            let toMsgType = function
                | RuleSeverity.Info -> MessageType.Information
                | RuleSeverity.Warning -> MessageType.ValidationWarning
                | RuleSeverity.Error -> MessageType.ValidationFailure
                | RuleSeverity.Critical -> MessageType.ValidationFailure
                | RuleSeverity.Unknown _ -> MessageType.Information
            let msgs =
                results.ValidationResults
                |> Array.collect (fun r ->
                    if r.Matched then
                        r.Actions
                        |> Array.map (fun a -> OutputTypes.createMessageUsing (fun () -> DateTimeOffset.UtcNow) (toMsgType a.Severity) (toMsgSeverity a.Severity) None a.Message)
                    else [||])
            let phase = if results.HasCriticalErrors then ValidationIo.ValidationPhase.Invalid else ValidationIo.ValidationPhase.Validated
            let nextReq : ValidationIo.PipelineStep[] = if results.HasCriticalErrors then [||] else [| ValidationIo.PipelineStep.Pricing |]
            let ticketIds = extractTicketIds jsonData
            // Build typed validation IO lists from request data
            let requiredDescs, remainingDescs, completedInst = computeValidationDtos jsonData
            let errorDtos = buildErrorDtos msgs
            let output = {
                Commands = [||]
                Messages = msgs
                Conflicts = [||]
                Suggestions = [||]
                ApprovalRequests = [||]
                PricingDetails = [||]
                Success = not results.HasCriticalErrors
                HasCriticalErrors = results.HasCriticalErrors
                ProcessingTime = results.TotalExecutionTime
                Phase = phase
                NextRequired = nextReq
                RequiredValidations = requiredDescs
                RemainingValidations = remainingDescs
                CompletedValidations = completedInst
                Errors = errorDtos
                Trace = None
                Metadata = if ticketIds.Length = 0 then Map.empty else Map [ ("ticketIds", SimpleValue.SArray (ticketIds |> Array.map (fun s -> SimpleValue.SString s))) ]
            }
            convertOutputToJson output
        with ex ->
            let errorOutput = {
                Commands = [||]
                Messages = [| OutputTypes.createMessageUsing (fun () -> DateTimeOffset.UtcNow) MessageType.ValidationFailure MessageSeverity.Critical None ($"Invalid DSL request: {ex.Message}") |]
                Conflicts = [||]
                Suggestions = [||]
                ApprovalRequests = [||]
                PricingDetails = [||]
                Success = false
                HasCriticalErrors = true
                ProcessingTime = TimeSpan.Zero
                Phase = ValidationIo.ValidationPhase.Invalid
                NextRequired = [||]
                RequiredValidations = [||]
                RemainingValidations = [||]
                CompletedValidations = [||]
                Errors = [| { ValidationIo.ValidationErrorDto.Code = NonEmptyString.CreateUnsafe "INVALID_DSL"; Message = NonEmptyString.CreateUnsafe ($"Invalid DSL request: {ex.Message}"); Severity = ValidationIo.SeverityDto.Critical; Field = None; RuleId = None } |]
                Trace = None
                Metadata = Map.empty
            }
            convertOutputToJson errorOutput