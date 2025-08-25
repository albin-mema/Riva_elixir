namespace ReservationService.Core

open System
open System.Text.Json.Serialization
open ReservationService.Core.Types

// Output types for the rule-based interpreter service.
// These define the commands and messages returned to the calling system.
// Follows the CQRS pattern where the engine emits commands as data.

// ========== COMMAND TYPES ==========

/// <summary>
/// Types of commands the service can return to the calling system.
/// Each command represents an action that should be performed externally.
/// </summary>
type CommandType =
    /// <summary>Create a new reservation</summary>
    | CreateReservation
    /// <summary>Update an existing reservation</summary>
    | UpdateReservation
    /// <summary>Cancel a reservation</summary>
    | CancelReservation
    /// <summary>Require manual approval</summary>
    | RequireApproval
    /// <summary>Calculate pricing information</summary>
    | CalculatePrice
    /// <summary>Check resource availability</summary>
    | CheckAvailability
    /// <summary>Send notification to stakeholders</summary>
    | SendNotification
    /// <summary>Log an event for audit purposes</summary>
    | LogEvent
    /// <summary>Invoke an external service</summary>
    | InvokeExternalService
    /// <summary>Schedule a follow-up action</summary>
    | ScheduleFollowUp

/// <summary>
/// Status values for reservation commands.
/// Represents the desired state after command execution.
/// </summary>
type CommandStatus =
    /// <summary>Command has been approved</summary>
    | Approved
    /// <summary>Command has been rejected</summary>
    | Rejected
    /// <summary>Command is pending approval</summary>
    | PendingApproval
    /// <summary>Command requires modification before proceeding</summary>
    | RequiresModification
    /// <summary>Command needs additional information</summary>
    | NeedsMoreInfo

/// <summary>
/// A command to be executed by the calling system.
/// Contains all necessary information for the caller to perform the action.
/// </summary>
type ReservationCommand = {
    /// <summary>Unique identifier for this command</summary>
    CommandId: Types.CommandId
    /// <summary>Type of command to execute</summary>
    CommandType: CommandType
    /// <summary>ID of the reservation this command relates to</summary>
    ReservationId: Types.ReservationId
    /// <summary>Status to set (for reservation commands)</summary>
    Status: CommandStatus option
    /// <summary>Final calculated price</summary>
    FinalPrice: decimal option
    /// <summary>Currency for the price</summary>
    Currency: string option
    /// <summary>Properties to update on the reservation</summary>
    PropertyUpdates: Map<string, SimpleValue> option
    /// <summary>Parameters for the command</summary>
    Parameters: Map<string, SimpleValue>
    /// <summary>When this command should be executed</summary>
    ExecuteAt: DateTimeOffset option
    /// <summary>Priority for command execution (higher numbers = higher priority)</summary>
    Priority: int
    /// <summary>Whether this command must succeed for others to proceed</summary>
    IsBlocking: bool
}

// ========== MESSAGE TYPES ==========

/// <summary>
/// Types of messages the service can return.
/// Used to communicate validation results and processing information.
/// </summary>
type MessageType =
    /// <summary>Validation completed successfully</summary>
    | ValidationSuccess
    /// <summary>Validation failed</summary>
    | ValidationFailure
    /// <summary>Validation warning (non-blocking)</summary>
    | ValidationWarning
    /// <summary>Business rule was violated</summary>
    | BusinessRuleViolation
    /// <summary>Conflict was detected</summary>
    | ConflictDetected
    /// <summary>Price was calculated</summary>
    | PriceCalculated
    /// <summary>Manual approval is required</summary>
    | ApprovalRequired
    /// <summary>Informational message</summary>
    | Information
    /// <summary>Debug information</summary>
    | Debug

/// <summary>
/// Severity levels for messages.
/// Used to indicate the importance and impact of messages.
/// </summary>
type MessageSeverity =
    /// <summary>Informational message</summary>
    | Info
    /// <summary>Warning that doesn't prevent processing</summary>
    | Warning
    /// <summary>Error that may prevent processing</summary>
    | Error
    /// <summary>Critical error that stops processing</summary>
    | Critical

/// <summary>
/// A message about the validation process.
/// Contains detailed information about what happened during processing.
/// </summary>
type ValidationMessage = {
    /// <summary>Unique identifier for this message</summary>
    MessageId: Types.MessageId
    /// <summary>Type of message</summary>
    MessageType: MessageType
    /// <summary>Severity level</summary>
    Severity: MessageSeverity
    /// <summary>ID of the reservation this message relates to</summary>
    ReservationId: Types.ReservationId option
    /// <summary>ID of the rule that generated this message</summary>
    RuleId: Types.RuleId option
    /// <summary>Human-readable message</summary>
    Message: string
    /// <summary>Detailed explanation</summary>
    Details: string option
    /// <summary>Field or property this message relates to</summary>
    Field: string option
    /// <summary>Additional context data</summary>
    Context: Map<string, obj> option
    /// <summary>Timestamp when message was generated</summary>
    Timestamp: DateTimeOffset
}

// ========== CONFLICT TYPES ==========

/// Types of conflicts that can be detected
type ConflictType =
    | ResourceDoubleBooking
    | CapacityExceeded
    | TimeOverlap
    | ParticipantConflict
    | ServiceUnavailable
    | BusinessRuleConflict
    | PricingConflict

/// A detected conflict between reservations or rules
type ConflictDetails = {
    /// Unique identifier for this conflict
    ConflictId: string
    /// Type of conflict
    ConflictType: ConflictType
    /// Severity of the conflict
    Severity: MessageSeverity
    /// Primary reservation involved in conflict
    PrimaryReservationId: Types.ReservationId
    /// Other reservations involved in conflict
    ConflictingReservationIds: Types.ReservationId[]
    /// Description of the conflict
    Description: string
    /// Suggested resolution strategies
    ResolutionStrategies: string[]
    /// Whether this conflict can be auto-resolved
    CanAutoResolve: bool
    /// Rule that detected this conflict
    DetectedByRule: string option
}

/// A suggestion for resolving conflicts or improving requests
type ConflictSuggestion = {
    /// Unique identifier for this suggestion
    SuggestionId: string
    /// ID of the reservation this suggestion relates to
    ReservationId: Types.ReservationId
    /// Type of suggestion
    SuggestionType: string
    /// Description of the suggestion
    Description: string
    /// Specific changes suggested
    SuggestedChanges: Map<string, obj>
    /// Expected impact of implementing this suggestion
    ExpectedImpact: string option
    /// Priority of this suggestion
    Priority: int
    /// Whether implementing this suggestion requires approval
    RequiresApproval: bool
}

// ========== APPROVAL TYPES ==========

/// Types of approval that may be required
type ApprovalType =
    | ManagerApproval
    | FinancialApproval
    | SecurityApproval
    | ComplianceApproval
    | CustomApproval of string

/// A request for manual approval
type ApprovalRequest = {
    /// Unique identifier for this approval request
    ApprovalId: string
    /// ID of the reservation requiring approval
    ReservationId: Types.ReservationId
    /// Type of approval required
    ApprovalType: ApprovalType
    /// Required approval level or role
    RequiredLevel: string
    /// Reason approval is required
    Reason: string
    /// Additional context for the approver
    Context: Map<string, obj> option
    /// When approval is needed by
    RequiredBy: DateTimeOffset option
    /// What happens if approval times out
    TimeoutAction: string option
    /// Priority of this approval request
    Priority: int
    /// Rule that triggered this approval requirement
    TriggeredByRule: string option
}

// ========== PRICING INFORMATION ==========

/// Breakdown of pricing components
type PriceComponent = {
    /// Name of the price component
    ComponentName: string
    /// Description of what this component covers
    Description: string option
    /// Base amount for this component
    BaseAmount: decimal
    /// Quantity or multiplier
    Quantity: decimal
    /// Final amount for this component
    Amount: decimal
    /// Currency
    Currency: string
    /// Rule that calculated this component
    CalculatedByRule: string option
}

/// Detailed pricing information
type PricingDetails = {
    /// ID of the reservation this pricing relates to
    ReservationId: Types.ReservationId
    /// Individual price components
    Components: PriceComponent[]
    /// Subtotal before taxes and fees
    Subtotal: decimal
    /// Tax amount
    Tax: decimal option
    /// Additional fees
    Fees: decimal option
    /// Total amount
    Total: decimal
    /// Currency
    Currency: string
    /// Discounts applied
    Discounts: PriceComponent[] option
    /// When this pricing was calculated
    CalculatedAt: DateTimeOffset
}

// ========== MAIN OUTPUT TYPE ==========

/// Complete output from the rule interpretation service
type RuleInterpretationOutput = {
    /// Commands to be executed by the calling system
    Commands: ReservationCommand[]
    /// Messages about the validation process
    Messages: ValidationMessage[]
    /// Detected conflicts
    Conflicts: ConflictDetails[]
    /// Suggestions for improvements or alternatives
    Suggestions: ConflictSuggestion[]
    /// Approval requests
    ApprovalRequests: ApprovalRequest[]
    /// Detailed pricing information (not used by validation-only service, present for compatibility)
    PricingDetails: PricingDetails[]
    /// Overall success status
    Success: bool
    /// Whether any critical errors occurred
    HasCriticalErrors: bool
    /// Total processing time
    ProcessingTime: TimeSpan
    /// Current processing phase for pipeline integration
    Phase: ValidationIo.ValidationPhase
    /// Next required external steps for downstream services
    NextRequired: ValidationIo.PipelineStep[]
    /// List of required validations (as descriptors)
    RequiredValidations: ValidationIo.ValidationDescriptorDto[]
    /// List of validation descriptors still required (remaining)
    RemainingValidations: ValidationIo.ValidationDescriptorDto[]
    /// List of validations completed (instances with status/messages)
    CompletedValidations: ValidationIo.ValidationInstanceDto[]
    /// Flat errors extracted from messages
    Errors: ValidationIo.ValidationErrorDto[]
    /// Trace information (if enabled)
    Trace: string[] option
    /// Metadata about the processing (may echo caller-provided artifact/ticket identifiers)
    Metadata: Map<string, SimpleValue>
}

// ========== HELPER FUNCTIONS ==========

module OutputTypes =

    /// Create a simple reservation command
    let createCommand commandType reservationId status =
        {
            CommandId = Types.CommandId (Guid.NewGuid())
            CommandType = commandType
            ReservationId = reservationId
            Status = status
            FinalPrice = None
            Currency = None
            PropertyUpdates = None
            Parameters = Map.empty
            ExecuteAt = None
            Priority = 0
            IsBlocking = false
        }

    /// Create a validation message
    let createMessage messageType severity reservationId message =
        {
            MessageId = Types.MessageId (Guid.NewGuid())
            MessageType = messageType
            Severity = severity
            ReservationId = reservationId
            RuleId = None
            Message = message
            Details = None
            Field = None
            Context = None
            Timestamp = DateTimeOffset.UtcNow
        }

    /// Create a validation message at a specified time (deterministic for tests)
    let createMessageAt timestamp messageType severity reservationId message =
        {
            MessageId = Types.MessageId (Guid.NewGuid())
            MessageType = messageType
            Severity = severity
            ReservationId = reservationId
            RuleId = None
            Message = message
            Details = None
            Field = None
            Context = None
            Timestamp = timestamp
        }

    /// Create a validation message using an injected clock function
    let createMessageUsing (now: unit -> DateTimeOffset) messageType severity reservationId message =
        createMessageAt (now()) messageType severity reservationId message

    /// Create a conflict details record
    let createConflict conflictType primaryReservationId description =
        {
            ConflictId = Guid.NewGuid().ToString()
            ConflictType = conflictType
            Severity = Error
            PrimaryReservationId = primaryReservationId
            ConflictingReservationIds = [||]
            Description = description
            ResolutionStrategies = [||]
            CanAutoResolve = false
            DetectedByRule = None
        }

    /// Create an approval request
    let createApprovalRequest approvalType reservationId reason =
        {
            ApprovalId = Guid.NewGuid().ToString()
            ReservationId = reservationId
            ApprovalType = approvalType
            RequiredLevel = "Manager"
            Reason = reason
            Context = None
            RequiredBy = None
            TimeoutAction = None
            Priority = 0
            TriggeredByRule = None
        }
