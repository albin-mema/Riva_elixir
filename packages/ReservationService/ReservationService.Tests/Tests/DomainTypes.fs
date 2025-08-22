module DomainTypes

open System
open Xunit
open FsUnit.Xunit
open ReservationService.Core.Types

// ---------- ReservationId Tests ----------
[<Fact>]
let ``ReservationId should be created from Guid`` () =
    let guid = Guid.NewGuid()
    let reservationId = ReservationId guid
    reservationId |> should equal (ReservationId guid)

[<Fact>]
let ``ReservationId should be equal when Guids are equal`` () =
    let guid = Guid.NewGuid()
    let id1 = ReservationId guid
    let id2 = ReservationId guid
    id1 |> should equal id2

[<Fact>]
let ``ReservationId should not be equal when Guids are different`` () =
    let guid1 = Guid.NewGuid()
    let guid2 = Guid.NewGuid()
    let id1 = ReservationId guid1
    let id2 = ReservationId guid2
    id1 |> should not' (equal id2)

[<Fact>]
let ``ReservationId should be hashable`` () =
    let guid = Guid.NewGuid()
    let id1 = ReservationId guid
    let id2 = ReservationId guid
    (hash id1) |> should equal (hash id2)

// ---------- CustomerId Tests ----------
[<Fact>]
let ``CustomerId should be created from Guid`` () =
    let guid = Guid.NewGuid()
    let customerId = CustomerId guid
    customerId |> should equal (CustomerId guid)

[<Fact>]
let ``CustomerId should be equal when Guids are equal`` () =
    let guid = Guid.NewGuid()
    let id1 = CustomerId guid
    let id2 = CustomerId guid
    id1 |> should equal id2

[<Fact>]
let ``CustomerId should not be equal when Guids are different`` () =
    let guid1 = Guid.NewGuid()
    let guid2 = Guid.NewGuid()
    let id1 = CustomerId guid1
    let id2 = CustomerId guid2
    id1 |> should not' (equal id2)

[<Fact>]
let ``CustomerId should be hashable`` () =
    let guid = Guid.NewGuid()
    let id1 = CustomerId guid
    let id2 = CustomerId guid
    (hash id1) |> should equal (hash id2)

// ---------- Money Value Object Tests ----------
[<Fact>]
let ``Money should be created with amount and currency`` () =
    match Money.Create(100.0M, "EUR") with
    | Ok money ->
        money.AmountValue |> should equal 100.0M
        money.CurrencyValue |> should equal "EUR"
    | Error msg -> failwith $"Failed to create money: {msg}"

[<Fact>]
let ``Money.Create should reject negative amounts`` () =
    match Money.Create(-100.0M, "USD") with
    | Ok _ -> failwith "Should not create money with negative amount"
    | Error msg -> Assert.Contains("positive", msg)

[<Fact>]
let ``Money.Create should reject invalid currency codes`` () =
    match Money.Create(100.0M, "INVALID") with
    | Ok _ -> failwith "Should not create money with invalid currency"
    | Error msg -> Assert.Contains("3 characters", msg)

[<Fact>]
let ``Money should be equal when amount and currency are equal`` () =
    let money1 = Money.Create(100.0M, "EUR") |> Result.defaultWith (fun _ -> failwith "Failed to create money1")
    let money2 = Money.Create(100.0M, "EUR") |> Result.defaultWith (fun _ -> failwith "Failed to create money2")
    money1 |> should equal money2

[<Fact>]
let ``Money should not be equal when amounts are different`` () =
    let money1 = Money.Create(100.0M, "EUR") |> Result.defaultWith (fun _ -> failwith "Failed to create money1")
    let money2 = Money.Create(200.0M, "EUR") |> Result.defaultWith (fun _ -> failwith "Failed to create money2")
    money1 |> should not' (equal money2)

[<Fact>]
let ``Money should not be equal when currencies are different`` () =
    let money1 = Money.Create(100.0M, "EUR") |> Result.defaultWith (fun _ -> failwith "Failed to create money1")
    let money2 = Money.Create(100.0M, "USD") |> Result.defaultWith (fun _ -> failwith "Failed to create money2")
    money1 |> should not' (equal money2)

[<Fact>]
let ``Money should be hashable`` () =
    let money1 = Money.Create(100.0M, "EUR") |> Result.defaultWith (fun _ -> failwith "Failed to create money1")
    let money2 = Money.Create(100.0M, "EUR") |> Result.defaultWith (fun _ -> failwith "Failed to create money2")
    (hash money1) |> should equal (hash money2)

// ---------- Basic F# Record Type Tests (TimeRange) ----------
[<Fact>]
let ``TimeRange should calculate duration correctly`` () =
    let start = DateTimeOffset(2023, 1, 1, 10, 0, 0, TimeSpan.Zero)
    let ``end`` = DateTimeOffset(2023, 1, 1, 12, 0, 0, TimeSpan.Zero)
    match TimeRange.Create(start, ``end``) with
    | Ok timeRange -> timeRange.Duration |> should equal (TimeSpan.FromHours(2.0))
    | Error msg -> failwith $"Failed to create TimeRange: {msg}"

[<Fact>]
let ``TimeRange should reject invalid ranges where end is before start`` () =
    let start = DateTimeOffset(2023, 1, 1, 12, 0, 0, TimeSpan.Zero)
    let ``end`` = DateTimeOffset(2023, 1, 1, 10, 0, 0, TimeSpan.Zero)
    match TimeRange.Create(start, ``end``) with
    | Ok _ -> failwith "Should not create TimeRange with end before start"
    | Error msg -> Assert.Contains("after start time", msg)

[<Fact>]
let ``TimeRange should be equal when start and end are equal`` () =
    let start = DateTimeOffset(2023, 1, 1, 10, 0, 0, TimeSpan.Zero)
    let ``end`` = DateTimeOffset(2023, 1, 1, 12, 0, 0, TimeSpan.Zero)
    let range1 = TimeRange.Create(start, ``end``) |> Result.defaultWith (fun _ -> failwith "Failed to create range1")
    let range2 = TimeRange.Create(start, ``end``) |> Result.defaultWith (fun _ -> failwith "Failed to create range2")
    range1 |> should equal range2

[<Fact>]
let ``TimeRange should not be equal when start times are different`` () =
    let start1 = DateTimeOffset(2023, 1, 1, 10, 0, 0, TimeSpan.Zero)
    let start2 = DateTimeOffset(2023, 1, 1, 11, 0, 0, TimeSpan.Zero)
    let ``end`` = DateTimeOffset(2023, 1, 1, 12, 0, 0, TimeSpan.Zero)
    let range1 = TimeRange.Create(start1, ``end``) |> Result.defaultWith (fun _ -> failwith "Failed to create range1")
    let range2 = TimeRange.Create(start2, ``end``) |> Result.defaultWith (fun _ -> failwith "Failed to create range2")
    range1 |> should not' (equal range2)

// ---------- Basic F# Union Type Tests (ParticipantRole) ----------
[<Fact>]
let ``Primary should be a valid ParticipantRole`` () =
    let role = Primary
    role |> should equal Primary

[<Fact>]
let ``Guest should be a valid ParticipantRole`` () =
    let role = Guest
    role |> should equal Guest

[<Fact>]
let ``Staff should be a valid ParticipantRole`` () =
    let role = Staff
    role |> should equal Staff

[<Fact>]
let ``Other should be a valid ParticipantRole with custom value`` () =
    let consultantStr = NonEmptyString.CreateUnsafe("Consultant")
    let role = Other consultantStr
    match role with
    | Other value when value = consultantStr -> true |> should equal true
    | _ -> false |> should equal true

// ---------- Improved Participant Type Tests ----------
[<Fact>]
let ``Participant should be created with ID and optional name`` () =
    let guid = Guid.NewGuid()
    let emptyMap = Map.empty
    match Participant.CreateWithId(guid, Some "John Doe", Primary, emptyMap) with
    | Ok participant ->
        match participant with
        | IdentifiedParticipant (id, name, role, contact) ->
            id |> should equal guid
            name.IsSome |> should equal true
            name.Value.Value |> should equal "John Doe"
            role |> should equal Primary
            Assert.Equal<Map<string, string>>(emptyMap, contact)
        | _ -> failwith "Expected IdentifiedParticipant"
    | Error msg -> failwith $"Failed to create participant: {msg}"

[<Fact>]
let ``Participant should be created with ID only`` () =
    let guid = Guid.NewGuid()
    let emptyMap = Map.empty
    match Participant.CreateWithId(guid, None, Guest, emptyMap) with
    | Ok participant ->
        match participant with
        | IdentifiedParticipant (id, name, role, contact) ->
            id |> should equal guid
            name.IsNone |> should equal true
            role |> should equal Guest
            Assert.Equal<Map<string, string>>(emptyMap, contact)
        | _ -> failwith "Expected IdentifiedParticipant"
    | Error msg -> failwith $"Failed to create participant: {msg}"

[<Fact>]
let ``Participant should be created with name only`` () =
    let emptyMap = Map.empty
    match Participant.CreateWithName("Jane Smith", Staff, emptyMap) with
    | Ok participant ->
        match participant with
        | NamedParticipant (name, role, contact) ->
            name.Value |> should equal "Jane Smith"
            role |> should equal Staff
            Assert.Equal<Map<string, string>>(emptyMap, contact)
        | _ -> failwith "Expected NamedParticipant"
    | Error msg -> failwith $"Failed to create participant: {msg}"

[<Fact>]
let ``Participant should reject empty names`` () =
    let guid = Guid.NewGuid()
    let emptyMap = Map.empty
    match Participant.CreateWithId(guid, Some "", Primary, emptyMap) with
    | Ok _ -> failwith "Should not create participant with empty name"
    | Error msg -> Assert.Contains("empty", msg)

[<Fact>]
let ``Participant should reject empty name-only creation`` () =
    let emptyMap = Map.empty
    match Participant.CreateWithName("", Primary, emptyMap) with
    | Ok _ -> failwith "Should not create participant with empty name"
    | Error msg -> Assert.Contains("empty", msg)

// ---------- Improved ResourceType Tests ----------
[<Fact>]
let ``Room should be a valid ResourceType with capacity`` () =
    let capacity = FixedCapacity (PositiveInt.Create(50) |> Result.defaultWith (fun _ -> failwith "Failed to create capacity"))
    let resourceType = Room capacity
    match resourceType with
    | Room c -> c |> should equal capacity
    | _ -> failwith "Expected Room"

[<Fact>]
let ``Equipment should be a valid ResourceType with portability`` () =
    let resourceType = Equipment true
    match resourceType with
    | Equipment isPortable -> isPortable |> should equal true
    | _ -> failwith "Expected Equipment"

[<Fact>]
let ``Vehicle should be a valid ResourceType with capacity`` () =
    let capacity = VariableCapacity (NonNegativeInt.Zero, PositiveInt.Create(8) |> Result.defaultWith (fun _ -> failwith "Failed to create max capacity"))
    let resourceType = Vehicle capacity
    match resourceType with
    | Vehicle c -> c |> should equal capacity
    | _ -> failwith "Expected Vehicle"

[<Fact>]
let ``Person should be a valid ResourceType`` () =
    let resourceType = Person
    resourceType |> should equal Person

[<Fact>]
let ``OtherResource should be a valid ResourceType with custom value`` () =
    let facilityStr = NonEmptyString.CreateUnsafe("Facility")
    let resourceType = OtherResource facilityStr
    match resourceType with
    | OtherResource value when value = facilityStr -> true |> should equal true
    | _ -> false |> should equal true