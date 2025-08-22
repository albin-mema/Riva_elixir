namespace ReservationService.Core

open System
open System.Text.Json.Serialization
open ReservationService.Core.RuleTypes

// Output types for the rule-based interpreter service
// These define the commands and messages returned to the calling system

// ========== COMMAND TYPES ==========

/// Types of commands the service can return
type CommandType =
    | CreateReservation
    | UpdateReservation
    | CancelReservation
    | RequireApproval
    | CalculatePrice
    | CheckAvailability
    | SendNotification
    | LogEvent
    | InvokeExternalService
    | ScheduleFollowUp

/// Status for reservation commands
type ReservationStatus =
    | Approved
    | Rejected
    | PendingApproval
    | RequiresModification
    | NeedsMoreInfo

/// A command to be executed by the calling system
type ReservationCommand = {
    /// Unique identifier for this command
    CommandId: string
    /// Type of command
    CommandType: CommandType
    /// ID of the reservation this command relates to
    ReservationId: string
    /// Status to set (for reservation commands)
    Status: ReservationStatus option
    /// Final calculated price
    FinalPrice: decimal option
    /// Currency for the price
    Currency: string option
    /// Properties to update on the reservation
    PropertyUpdates: Map<string, obj> option
    /// Parameters for the command
    Parameters: Map<string, obj>
    /// When this command should be executed
    ExecuteAt: DateTimeOffset option
    /// Priority for command execution
    Priority: int
    /// Whether this command must succeed for others to proceed
    IsBlocking: bool
}

// ========== MESSAGE TYPES ==========

/// Types of messages the service can return
type MessageType =
    | ValidationSuccess
    | ValidationFailure
    | ValidationWarning
    | BusinessRuleViolation
    | ConflictDetected
    | PriceCalculated
    | ApprovalRequired
    | Information
    | Debug

/// Severity levels for messages
type MessageSeverity =
    | Info
    | Warning
    | Error
    | Critical

/// A message about the validation process
type ValidationMessage = {
    /// Unique identifier for this message
    MessageId: string
    /// Type of message
    MessageType: MessageType
    /// Severity level
    Severity: MessageSeverity
    /// ID of the reservation this message relates to
    ReservationId: string
    /// ID of the rule that generated this message
    RuleId: string option
    /// Human-readable message
    Message: string
    /// Detailed explanation
    Details: string option
    /// Field or property this message relates to
    Field: string option
    /// Additional context data
    Context: Map<string, obj> option
    /// Timestamp when message was generated
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
    PrimaryReservationId: string
    /// Other reservations involved in conflict
    ConflictingReservationIds: string[]
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
    ReservationId: string
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
    ReservationId: string
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
    ReservationId: string
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
    /// Detailed pricing information
    PricingDetails: PricingDetails[]
    /// Overall success status
    Success: bool
    /// Whether any critical errors occurred
    HasCriticalErrors: bool
    /// Total processing time
    ProcessingTime: TimeSpan
    /// Trace information (if enabled)
    Trace: string[] option
    /// Metadata about the processing
    Metadata: Map<string, obj>
}

// ========== HELPER FUNCTIONS ==========

module OutputTypes =
    
    /// Create a simple reservation command
    let createCommand commandType reservationId status =
        {
            CommandId = Guid.NewGuid().ToString()
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
            MessageId = Guid.NewGuid().ToString()
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
