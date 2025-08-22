namespace ReservationService.Core

open System
open ReservationService.Core.Types

module Engine =



    // Update a single validation state given result
    let updateValidationState: UpdateValidationState =
        fun validationId succeeded details at v ->
            if v.Id <> validationId then v
            else
                let newStatus =
                    if succeeded then
                        let validDetails =
                            details
                            |> Option.bind (fun d -> match NonEmptyString.Create(d) with Ok v -> Some v | Error _ -> None)
                        ValidationStatus.Succeeded(at, validDetails)
                    else
                        let reason = defaultArg details "Validation failed"
                        let validReason =
                            match NonEmptyString.Create(reason) with
                            | Ok v -> v
                            | Error _ -> NonEmptyString.CreateUnsafe("Validation failed")
                        ValidationStatus.Failed(at, validReason)
                { v with Status = newStatus; UpdatedAt = at }

    // Get pending validations, considering dependencies
    let getPendingValidations: GetPendingValidations =
        fun reservation ->
            reservation.ValidationHistory
            |> ignore
            // Pending validations come from the request's ValidationStates
            reservation.Request.ValidationStates
            |> List.filter (fun v ->
                match v.Status with
                | Pending
                | InProgress _ -> true
                | Failed _
                | Skipped _
                | Succeeded _ -> false)
            |> List.filter (fun v ->
                // If there are dependencies, they must be succeeded if MustSucceed=true
                v.Dependencies
                |> List.forall (fun d ->
                    reservation.Request.ValidationStates
                    |> List.tryFind (fun x -> x.Id = d.ValidationId)
                    |> Option.map (fun depState ->
                        match depState.Status, d.MustSucceed with
                        | Succeeded _, _ -> true
                        | Failed _, true -> false
                        | _, false -> true
                        | _ -> false)
                    |> Option.defaultValue true))

    // Apply a domain event to a reservation aggregate with payment due hours parameter
    let applyEventWithPaymentDue (paymentDueHours: int) : ApplyEventToReservation =
        fun state event ->
            // DEBUG: Log version progression
            printfn "DEBUG: applyEvent called - Current state version: %d, Event: %A" state.Version (match event with | ReservationCreated _ -> "Created" | ReservationUpdated _ -> "Updated" | _ -> "Other")
            
            match event with
            | ReservationCreated (rid, req, _) ->
                printfn "DEBUG: ReservationCreated - Setting version to 1, previous was: %d" state.Version
                // Price must be provided by the calling system; interpreter should not assume currency or defaults
                match req.InterimPrice with
                | None -> Error [ ValidationError ("price", "InterimPrice must be provided by caller") ]
                | Some providedPrice ->
                    Ok { Id = rid
                         Request = req
                         FinalTime = req.RequestedTime
                         FinalPrice = providedPrice
                         CreatedAt = req.CreatedAt
                         CreatedBy = req.CreatedBy
                         Status = Draft
                         Version = 1UL
                         ValidationHistory = req.ValidationStates
                         Events = [event] }

            | ReservationUpdated (_rid, req, meta) ->
                printfn "DEBUG: ReservationUpdated - Setting version to %d, previous was: %d" meta.Version state.Version
                if meta.Version <> state.Version + 1UL then
                    printfn "ERROR: Version mismatch! Expected %d, got %d" (state.Version + 1UL) meta.Version
                Ok { state with
                        Request = req;
                        Version = meta.Version;
                        Events = event :: state.Events }

            | ValidationStarted (_rid, vState, meta) ->
                let updatedReq =
                    let vs =
                        state.Request.ValidationStates
                        |> List.map (fun v ->
                            if v.Id = vState.Id then { vState with Status = InProgress (meta.Timestamp, None) } else v)
                    { state.Request with ValidationStates = vs }
                Ok { state with Request = updatedReq; Events = event :: state.Events; Version = meta.Version }

            | ValidationCompleted (_rid, vState, meta) ->
                let vs =
                    state.Request.ValidationStates
                    |> List.map (fun v -> if v.Id = vState.Id then vState else v)
                Ok { state with Request = { state.Request with ValidationStates = vs };
                                 ValidationHistory = vState :: state.ValidationHistory;
                                 Events = event :: state.Events;
                                 Version = meta.Version }

            | TermsAccepted (_rid, by, at, meta) ->
                // Use payment due time parameter instead of hardcoded config
                Ok { state with Status = PendingPayment (at.AddHours(float paymentDueHours), None);
                                 Events = event :: state.Events;
                                 Version = meta.Version }

            | ReservationConfirmed (_rid, by, at, meta) ->
                Ok { state with Status = Confirmed (at, by);
                                 Events = event :: state.Events;
                                 Version = meta.Version }

            | ReservationCancelled (_rid, reason, meta) ->
                Ok { state with Status = Cancelled reason;
                                 Events = event :: state.Events;
                                 Version = meta.Version }

            | ReservationChangeRequested (_rid, change, meta) ->
                Ok { state with Events = event :: state.Events; Version = meta.Version }

            | ReservationChangeApplied (_rid, changes, appliedAt, meta) ->
                let newTime = defaultArg changes.NewTime state.FinalTime
                Ok { state with FinalTime = newTime;
                                 Events = event :: state.Events;
                                 Version = meta.Version }

            | ReservationTimedOut (_rid, at, meta) ->
                Ok { state with Status = Cancelled (Timeout at);
                                 Events = event :: state.Events;
                                 Version = meta.Version }

    // Process a partial validation result
    let processPartialValidation: ProcessPartialValidation =
        fun validationId succeeded details state ->
            let at = DateTimeOffset.UtcNow
            let vs =
                state.Request.ValidationStates
                |> List.map (updateValidationState validationId succeeded details at)
            let newState = { state with Request = { state.Request with ValidationStates = vs } }
            Ok newState

    // Create a snapshot of the reservation state
    let createSnapshot: CreateReservationSnapshot =
        fun reservation sinceVersion timestamp ->
            { ReservationId = reservation.Id
              State = reservation
              Version = reservation.Version
              Timestamp = timestamp
              EventIds = reservation.Events |> List.choose (fun ev ->
                  match ev with
                  | ReservationCreated (_,_,m)
                  | ReservationUpdated (_,_,m)
                  | ValidationStarted (_,_,m)
                  | ValidationCompleted (_,_,m)
                  | TermsAccepted (_,_,_,m)
                  | ReservationConfirmed (_,_,_,m)
                  | ReservationCancelled (_,_,m)
                  | ReservationChangeRequested (_,_,m)
                  | ReservationChangeApplied (_,_,_,m)
                  | ReservationTimedOut (_,_,m) -> Some m.EventId) }

    // Backward-compatible applyEvent function that uses default payment due time
    let applyEvent: ApplyEventToReservation =
        fun state event ->
            applyEventWithPaymentDue 24 state event

    // Replay a sequence of events onto a state with payment due hours parameter
    let replayWithPaymentDue (paymentDueHours: int) : ReplayEventsToReservation =
        fun state events ->
            printfn "DEBUG: Replay called with %d events, starting version: %d" (List.length events) state.Version
            ((Ok state), events)
            ||> List.fold (fun acc ev ->
                match acc with
                | Ok s ->
                    printfn "DEBUG: Applying event in replay, current version: %d" s.Version
                    applyEventWithPaymentDue paymentDueHours s ev
                | Error e ->
                    printfn "DEBUG: Error in replay before applying event: %s" (string e)
                    acc)

    // Backward-compatible replay function
    let replay: ReplayEventsToReservation =
        fun state events ->
            replayWithPaymentDue 24 state events

    // Very minimal command application sketch (expand as needed)
    let applyCommand (stateOpt: Reservation option) (cmd: Command) (currentTime: DateTimeOffset) : Result<Event list, string> =
        let mkMeta version (cmdId: CommandId) =
            { EventId = Guid.NewGuid()
              CommandId = cmdId
              CorrelationId = None
              Timestamp = currentTime
              Version = version }
        match cmd, stateOpt with
        | CreateReservation (req, cmeta), None ->
            let rid = defaultArg req.Id (ReservationId(Guid.NewGuid()))
            let ev = ReservationCreated (rid, req, mkMeta 1UL cmeta.CommandId)
            Ok [ ev ]
        | CreateReservation _, Some _ -> Error "Reservation already exists"
        | UpdateReservation (rid, req, cmeta), Some s when s.Id = rid ->
            Ok [ ReservationUpdated (rid, req, mkMeta (s.Version + 1UL) cmeta.CommandId) ]
        | UpdateReservation _, _ -> Error "Reservation not found"
        | AcceptTerms (rid, by, at, cmeta), Some s when s.Id = rid ->
            Ok [ TermsAccepted (rid, by, at, mkMeta (s.Version + 1UL) cmeta.CommandId) ]
        | ConfirmReservation (rid, by, at, cmeta), Some s when s.Id = rid ->
            Ok [ ReservationConfirmed (rid, by, at, mkMeta (s.Version + 1UL) cmeta.CommandId) ]
        | CancelReservation (rid, reason, cmeta), Some s when s.Id = rid ->
            Ok [ ReservationCancelled (rid, reason, mkMeta (s.Version + 1UL) cmeta.CommandId) ]
        | RequestReservationChange (rid, change, cmeta), Some s when s.Id = rid ->
            Ok [ ReservationChangeRequested (rid, change, mkMeta (s.Version + 1UL) cmeta.CommandId) ]
        | StartValidation (rid, vid, by, cmeta), Some s when s.Id = rid ->
            let vState =
                s.Request.ValidationStates
                |> List.tryFind (fun v -> v.Id = vid)
                |> Option.defaultWith (fun _ -> failwith "Unknown validation")
            Ok [ ValidationStarted (rid, vState, mkMeta (s.Version + 1UL) cmeta.CommandId) ]
        | CompleteValidation (rid, vid, ok, details, at, cmeta), Some s when s.Id = rid ->
            let vState =
                s.Request.ValidationStates
                |> List.map (fun v -> if v.Id = vid then updateValidationState vid ok details at v else v)
                |> List.find (fun v -> v.Id = vid)
            Ok [ ValidationCompleted (rid, vState, mkMeta (s.Version + 1UL) cmeta.CommandId) ]
        | _ -> Error "Invalid command/state"

