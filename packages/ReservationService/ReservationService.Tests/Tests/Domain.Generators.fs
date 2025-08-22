namespace ReservationService.Tests.Tests

module DomainGenerators =
    open System
    open FsCheck
    open FsCheck.FSharp
    open ReservationService.Core.Types

    // Helpers
    let private forceOk = function Ok x -> x | Error e -> failwithf "Generator error: %s" e

    let private genAsciiChar : Gen<char> = Gen.elements ([ 'a'..'z' ] @ [ 'A'..'Z' ] @ [ '0'..'9' ] @ [ ' '; '_'; '-' ])
    let private genString : Gen<string> =
        Gen.listOf genAsciiChar |> Gen.map (fun cs -> new string (List.toArray cs))
    let private genText : Gen<string> = genString

    let private genOption (g: Gen<'a>) : Gen<'a option> = Gen.oneof [ Gen.constant None; Gen.map Some g ]



    let private genNonEmptyStringRaw : Gen<string> =
        gen {
            let! s = genString
            return if String.IsNullOrWhiteSpace s then "x" else s
        }

    let genNonEmptyString : Gen<NonEmptyString> =
        genNonEmptyStringRaw |> Gen.map NonEmptyString.CreateUnsafe

    let genPositiveInt : Gen<PositiveInt> = Gen.choose(1, 1000) |> Gen.map (fun i -> PositiveInt.Create i |> forceOk)
    let genNonNegativeInt : Gen<NonNegativeInt> = Gen.choose(0, 1000) |> Gen.map (fun i -> NonNegativeInt.Create i |> forceOk)
    let genPositiveDecimal : Gen<PositiveDecimal> = Gen.choose(1, 1_000_000) |> Gen.map (fun i -> PositiveDecimal.Create (decimal i) |> forceOk)

    let genMoney : Gen<Money> =
        gen {
            let! amt = genPositiveDecimal
            let! cur = genNonEmptyStringRaw
            return Money.Create(amt.Value, cur) |> forceOk
        }

    let genGuid : Gen<Guid> =
        Gen.arrayOfLength 16 (Gen.choose(0,255))
        |> Gen.map (fun ints -> ints |> Array.map byte |> Guid)
    let genDateTimeOffset : Gen<DateTimeOffset> =
        let epoch = DateTimeOffset(2000,1,1,0,0,0,TimeSpan.Zero)
        Gen.map2 (fun days secs -> epoch.AddDays(float days).AddSeconds(float secs)) (Gen.choose(-20000,20000)) (Gen.choose(0,24*60*60))

    let genTimeRange : Gen<TimeRange> =
        gen {
            let! startT = genDateTimeOffset
            let! minutes = Gen.choose(0, 24*60)
            let fin = startT.AddMinutes(float minutes)
            return TimeRange.Create(startT, fin) |> forceOk
        }

    let genZonedTimeRange : Gen<ZonedTimeRange> =
        gen {
            let! r = genTimeRange
            let! tz = genOption genNonEmptyStringRaw
            return ZonedTimeRange.Create(r, tz) |> forceOk
        }

    let genReservationId : Gen<ReservationId> = genGuid |> Gen.map ReservationId
    let genCustomerId : Gen<CustomerId> = genGuid |> Gen.map CustomerId
    let genCommandId : Gen<CommandId> = genGuid |> Gen.map CommandId
    let genCorrelationId : Gen<CorrelationId> = genGuid |> Gen.map CorrelationId
    let genRuleId : Gen<RuleId> = genGuid |> Gen.map RuleId
    let genServiceId : Gen<ServiceId> = genText |> Gen.map (fun s -> if String.IsNullOrWhiteSpace s then ServiceId "svc" else ServiceId s)

    let genResourceId : Gen<ResourceId> = genNonEmptyStringRaw |> Gen.map (fun s -> ResourceId.Create s |> forceOk)
    let genBusinessId : Gen<BusinessId> = genNonEmptyStringRaw |> Gen.map (fun s -> BusinessId.Create s |> forceOk)
    let genValidationId : Gen<ValidationId> = genNonEmptyStringRaw |> Gen.map (fun s -> ValidationId.Create s |> forceOk)

    let genParticipantRole : Gen<ParticipantRole> =
        Gen.frequency [ 3, Gen.constant ParticipantRole.Primary
                        3, Gen.constant ParticipantRole.Guest
                        2, Gen.constant ParticipantRole.Staff
                        2, genNonEmptyString |> Gen.map ParticipantRole.Other ]

    let genParticipant : Gen<Participant> =
        let genContact =
            gen {
                let! count = Gen.choose(0,3)
                let! keys = Gen.listOfLength count genNonEmptyStringRaw
                let! vals = Gen.listOfLength count genText
                return (List.zip keys vals |> Map.ofList)
            }
        let genWithId =
            gen {
                let! id = genGuid
                let! nameOpt = genOption genNonEmptyStringRaw
                let! role = genParticipantRole
                let! contact = genContact
                match nameOpt with
                | None -> return Participant.IdentifiedParticipant (id, None, role, contact)
                | Some n ->
                    let nes = NonEmptyString.CreateUnsafe n
                    return Participant.IdentifiedParticipant (id, Some nes, role, contact)
            }
        let genWithName =
            gen {
                let! name = genNonEmptyString
                let! role = genParticipantRole
                let! contact = genContact
                return Participant.NamedParticipant (name, role, contact)
            }
        Gen.frequency [ 3, genWithId; 2, genWithName ]

    let genResourceConfig : Gen<ResourceConfig> =
        gen {
            let! name = genNonEmptyString
            let! propCount = Gen.choose(0,3)
            let! reqProps = Gen.listOfLength propCount genNonEmptyStringRaw
            let validateOpt = None
            return { Name = name; RequiredProperties = reqProps |> List.map NonEmptyString.CreateUnsafe |> Set.ofList; Validate = validateOpt }
        }

    let genResourceDefinition : Gen<ResourceDefinition> = genResourceConfig |> Gen.map (fun c -> { Config = c })

    let genNEAttrMap : Gen<Map<NonEmptyString, NonEmptyString>> =
        gen {
            let! count = Gen.choose(0,4)
            let! keys = Gen.listOfLength count genNonEmptyString
            let! vals = Gen.listOfLength count genNonEmptyString
            return (List.zip keys vals |> Map.ofList)
        }

    let genResourceDescriptor : Gen<ResourceDescriptor> =
        gen {
            let! id = genResourceId
            let! def = genResourceDefinition
            let! attrs = genNEAttrMap
            let! acCount = Gen.choose(0,3)
            let! ac = Gen.listOfLength acCount genNonEmptyString
            return { Id = id; Definition = def; Attributes = attrs; AvailabilityConstraints = ac }
        }

    let genServiceDescriptor : Gen<ServiceDescriptor> =
        gen {
            let! id = genServiceId
            let! name = genNonEmptyString
            let! durOpt = genOption (Gen.choose(1, 8*60) |> Gen.map (fun m -> TimeSpan.FromMinutes(float m)))
            let! priceOpt = genOption genMoney
            let! attrs = genNEAttrMap
            let! reqResCount = Gen.choose(0,3)
            let! reqRes = Gen.listOfLength reqResCount genResourceId
            return { Id = id; Name = name; Duration = durOpt; BasePrice = priceOpt; Attributes = attrs; RequiredResources = reqRes }
        }


    let genRetryBehavior : Gen<RetryBehavior> =
        Gen.oneof [
            Gen.constant RetryBehavior.NoRetry
            Gen.constant RetryBehavior.RetryOnce
            Gen.map2 (fun a d -> RetryBehavior.LinearBackoff (a, d)) genPositiveInt genPositiveInt
            Gen.map3 (fun a b m -> RetryBehavior.ExponentialBackoff (a, b, m)) genPositiveInt genPositiveInt genPositiveInt
        ]

    let genValidationTimeout : Gen<ValidationTimeout> =
        Gen.oneof [
            Gen.constant ValidationTimeout.NoTimeout
            Gen.choose(1, 86_400) |> Gen.map (fun s -> ValidationTimeout.TimeoutAfter (TimeSpan.FromSeconds(float s)))
            genDateTimeOffset |> Gen.map ValidationTimeout.TimeoutAt
        ]

    let genValidationKind : Gen<ValidationKind> =
        Gen.oneof [
            Gen.constant ValidationKind.AvailabilityCheck
            Gen.constant ValidationKind.CustomerProfileCheck
            Gen.constant ValidationKind.PaymentCheck
            genNonEmptyString |> Gen.map (fun s -> ValidationKind.ExternalVerification s)
            genRuleId |> Gen.map ValidationKind.BusinessRuleCheck
            genNonEmptyString |> Gen.map ValidationKind.ManualApproval
            genNonEmptyString |> Gen.map ValidationKind.CustomValidation
        ]

    let genValidationStatus : Gen<ValidationStatus> =
        Gen.oneof [
            Gen.constant ValidationStatus.Pending
            Gen.map2 (fun t a -> ValidationStatus.InProgress (t, a)) genDateTimeOffset (genOption genNonEmptyString)
            Gen.map2 (fun t d -> ValidationStatus.Succeeded (t, d)) genDateTimeOffset (genOption genNonEmptyString)
            Gen.map2 (fun t r -> ValidationStatus.Failed (t, r)) genDateTimeOffset genNonEmptyString
            Gen.map2 (fun t r -> ValidationStatus.Skipped (t, r)) genDateTimeOffset genNonEmptyString
        ]

    let genValidationDependency : Gen<ValidationDependency> =
        gen { let! id = genValidationId
              let! must = Gen.elements [true; false]
              return { ValidationId = id; MustSucceed = must } }

    let genValidationState : Gen<ValidationState> =
        gen {
            let! id = genValidationId
            let! kind = genValidationKind
            let! isReq = Gen.elements [true; false]
            let! status = genValidationStatus
            let! depCount = Gen.choose(0,2)
            let! deps = Gen.listOfLength depCount genValidationDependency
            let! desc = genNonEmptyString
            let! timeout = genValidationTimeout
            let! retry = genRetryBehavior
            let! created = genDateTimeOffset
            let! updatedAdd = Gen.choose(0, 60*60)
            let updated = created.AddSeconds(float updatedAdd)
            return { Id = id; Kind = kind; IsRequired = isReq; Status = status; Dependencies = deps; Description = desc; Timeout = timeout; RetryBehavior = retry; CreatedAt = created; UpdatedAt = updated }
        }

    let genReservationRequest : Gen<ReservationRequest> =
        gen {
            let! idOpt = genOption genGuid
            let! cust = genCustomerId
            let! biz = genBusinessId
            let! res = genResourceDescriptor
            let! ztr = genZonedTimeRange
            let! svcCount = Gen.choose(0,3)
            let! services = Gen.listOfLength svcCount genServiceDescriptor
            let! partCount = Gen.choose(0,5)
            let! parts = Gen.listOfLength partCount genParticipant
            let! special = genOption genText
            let! metaCount = Gen.choose(0,4)
            let! metaKeys = Gen.listOfLength metaCount genText
            let! metaVals = Gen.listOfLength metaCount genText
            let metadata = List.zip metaKeys metaVals |> Map.ofList
            let! valCount = Gen.choose(0,3)
            let! vstates = Gen.listOfLength valCount genValidationState
            let! interim = genOption genMoney
            let! adj = genOption genZonedTimeRange
            let! createdAt = genDateTimeOffset
            let! createdBy = genOption genText
            let! ver = Gen.choose(0, Int32.MaxValue)
            return {
                Id = idOpt |> Option.map ReservationId
                CustomerId = cust
                BusinessId = biz
                Resource = res
                RequestedTime = ztr
                Services = services
                Participants = parts
                SpecialRequests = special
                Metadata = metadata
                ValidationStates = vstates
                InterimPrice = interim
                AdjustedTime = adj
                CreatedAt = createdAt
                CreatedBy = createdBy
                Version = uint64 ver
            }
        }

    type DomainArbitraries =
        static member NonEmptyString() = Arb.fromGen genNonEmptyString
        static member PositiveInt() = Arb.fromGen genPositiveInt
        static member NonNegativeInt() = Arb.fromGen genNonNegativeInt
        static member PositiveDecimal() = Arb.fromGen genPositiveDecimal
        static member Money() = Arb.fromGen genMoney
        static member TimeRange() = Arb.fromGen genTimeRange
        static member ZonedTimeRange() = Arb.fromGen genZonedTimeRange
        static member ReservationId() = Arb.fromGen genReservationId
        static member CustomerId() = Arb.fromGen genCustomerId
        static member CommandId() = Arb.fromGen genCommandId
        static member CorrelationId() = Arb.fromGen genCorrelationId
        static member RuleId() = Arb.fromGen genRuleId
        static member ServiceId() = Arb.fromGen genServiceId
        static member ResourceId() = Arb.fromGen genResourceId
        static member BusinessId() = Arb.fromGen genBusinessId
        static member ValidationId() = Arb.fromGen genValidationId
        static member ParticipantRole() = Arb.fromGen genParticipantRole
        static member Participant() = Arb.fromGen genParticipant
        static member ResourceConfig() = Arb.fromGen genResourceConfig
        static member ResourceDefinition() = Arb.fromGen genResourceDefinition
        static member ResourceDescriptor() = Arb.fromGen genResourceDescriptor
        static member ServiceDescriptor() = Arb.fromGen genServiceDescriptor
        static member RetryBehavior() = Arb.fromGen genRetryBehavior
        static member ValidationTimeout() = Arb.fromGen genValidationTimeout
        static member ValidationKind() = Arb.fromGen genValidationKind
        static member ValidationStatus() = Arb.fromGen genValidationStatus
        static member ValidationDependency() = Arb.fromGen genValidationDependency
        static member ValidationState() = Arb.fromGen genValidationState
        static member ReservationRequest() = Arb.fromGen genReservationRequest

