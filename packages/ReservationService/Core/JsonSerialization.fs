namespace ReservationService.Core

open System
open System.Text.Json
open System.Text.Json.Serialization
open ReservationService.Core.RuleTypes
open ReservationService.Core

/// JSON serialization handling for the rule-based interpreter service
module JsonSerialization =

    // ========== INPUT REQUEST TYPES ==========

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
        Metadata: Map<string, obj> option
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
        ResourceAvailability: Map<string, obj> option
        /// Business calendar (holidays, blackouts, etc.)
        BusinessCalendar: Map<string, obj> option
        /// Participant restrictions and information
        ParticipantRestrictions: Map<string, obj> option
        /// Current resource utilization
        ResourceUtilization: Map<string, obj> option
        /// Any other context data
        AdditionalContext: Map<string, obj> option
    }

    /// Complete input request to the rule interpreter
    type JsonRuleInterpretationRequest = {
        /// One or more reservation requests to process
        ReservationRequests: JsonReservationRequest[]
        /// Rule set to apply
        RuleSet: RuleSet
        /// Context data for rule processing
        ContextData: JsonContextData option
        /// Configuration parameters
        RuleParameters: Map<string, obj> option
        /// Whether to enable tracing
        EnableTrace: bool option
        /// Request metadata
        RequestMetadata: Map<string, obj> option
    }

    // ========== JSON SERIALIZATION OPTIONS ==========

    /// JSON serializer options for the service
    let jsonOptions = 
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        options.WriteIndented <- true
        options.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
        options.Converters.Add(JsonFSharpConverter())
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

    /// Create rule execution context from JSON request
    let createRuleExecutionContext (request: JsonRuleInterpretationRequest) : RuleExecutionContext =
        let contextData = 
            match request.ContextData with
            | Some context ->
                let mutable contextMap = Map.empty<string, RuleValue>
                
                // Add existing reservations
                match context.ExistingReservations with
                | Some reservations ->
                    contextMap <- Map.add "existingReservations" (SimpleValueOps.ofObj (box reservations)) contextMap
                | None -> ()

                // Add resource availability
                match context.ResourceAvailability with
                | Some availability ->
                    contextMap <- Map.add "resourceAvailability" (SimpleValueOps.ofObj (box availability)) contextMap
                | None -> ()

                // Add business calendar
                match context.BusinessCalendar with
                | Some calendar ->
                    contextMap <- Map.add "businessCalendar" (SimpleValueOps.ofObj (box calendar)) contextMap
                | None -> ()
                
                contextMap
            | None -> Map.empty
        
        let parameters =
            match request.RuleParameters with
            | Some p -> p |> Map.map (fun _ v -> SimpleValueOps.ofObj v)
            | None -> Map.empty

        {
            Timestamp = DateTimeOffset.UtcNow
            ExecutedBy = None
            ContextData = contextData
            Parameters = parameters
            TraceEnabled = request.EnableTrace |> Option.defaultValue false
        }

    // ========== OUTPUT CONVERSION ==========

    /// Convert rule interpretation output to JSON response
    let convertOutputToJson (output: RuleInterpretationOutput) : string =
        serializeToJson output

    // ========== MAIN PROCESSING FUNCTION ==========

    /// Process a JSON request and return JSON response
    let processJsonRequest (jsonRequest: string) : string =
        match deserializeFromJson<JsonRuleInterpretationRequest> jsonRequest with
        | Result.Error errorMsg ->
            let errorOutput = {
                Commands = [||]
                Messages = [| OutputTypes.createMessage ValidationFailure Critical "" $"Invalid JSON request: {errorMsg}" |]
                Conflicts = [||]
                Suggestions = [||]
                ApprovalRequests = [||]
                PricingDetails = [||]
                Success = false
                HasCriticalErrors = true
                ProcessingTime = TimeSpan.Zero
                Trace = None
                Metadata = Map.empty
            }
            convertOutputToJson errorOutput

        | Result.Ok request ->
            match parseJsonDocument jsonRequest with
            | Result.Error parseError ->
                let errorOutput = {
                    Commands = [||]
                    Messages = [| OutputTypes.createMessage ValidationFailure Critical "" $"JSON parsing error: {parseError}" |]
                    Conflicts = [||]
                    Suggestions = [||]
                    ApprovalRequests = [||]
                    PricingDetails = [||]
                    Success = false
                    HasCriticalErrors = true
                    ProcessingTime = TimeSpan.Zero
                    Trace = None
                    Metadata = Map.empty
                }
                convertOutputToJson errorOutput

            | Result.Ok jsonDocument ->
                let context = createRuleExecutionContext request
                let ruleResult = RuleEngine.interpretRules request.RuleSet jsonDocument context
                
                // Convert rule evaluation results to output format
                let messages = 
                    [|
                        yield! ruleResult.ValidationResults |> Array.map (fun r -> 
                            let messageType = if r.Matched then ValidationSuccess else ValidationFailure
                            let severity = if r.Error.IsSome then Critical else Info
                            { OutputTypes.createMessage messageType severity "" r.RuleName with RuleId = Some r.RuleId })
                        
                        yield! ruleResult.BusinessRuleResults |> Array.map (fun r ->
                            let messageType = if r.Matched then BusinessRuleViolation else Information
                            let severity = if r.Error.IsSome then Critical else Warning
                            { OutputTypes.createMessage messageType severity "" r.RuleName with RuleId = Some r.RuleId })
                    |]
                
                let commands = 
                    ruleResult.ValidationResults
                    |> Array.filter (fun r -> r.Matched && r.Actions.Length > 0)
                    |> Array.collect (fun r -> 
                        r.Actions |> Array.map (fun action ->
                            match action.ActionType with
                            | RuleActionType.Accept -> OutputTypes.createCommand CommandType.CreateReservation r.RuleId (Some ReservationStatus.Approved)
                            | RuleActionType.Reject -> OutputTypes.createCommand CommandType.CancelReservation r.RuleId (Some ReservationStatus.Rejected)
                            | RuleActionType.RequireApproval -> OutputTypes.createCommand CommandType.RequireApproval r.RuleId (Some ReservationStatus.PendingApproval)
                            | _ -> OutputTypes.createCommand CommandType.LogEvent r.RuleId None))
                
                let output = {
                    Commands = commands
                    Messages = messages
                    Conflicts = [||] // Will be populated by conflict rules
                    Suggestions = [||] // Will be populated by suggestion rules
                    ApprovalRequests = [||] // Will be populated by approval rules
                    PricingDetails = [||] // Will be populated by pricing rules
                    Success = not ruleResult.HasCriticalErrors
                    HasCriticalErrors = ruleResult.HasCriticalErrors
                    ProcessingTime = ruleResult.TotalExecutionTime
                    Trace = None // Will add trace information if enabled
                    Metadata = Map.empty
                }
                
                convertOutputToJson output
