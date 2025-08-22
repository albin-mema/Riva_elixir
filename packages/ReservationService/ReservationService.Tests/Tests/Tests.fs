module Tests

open System
open Xunit
open ReservationService.Core.Types

[<Fact>]
let ``Test project setup verification`` () =
    // This test verifies that the test project is properly configured
    // and can run successfully
    Assert.Equal(2, 1 + 1)

[<Fact>]
let ``Can access main library project`` () =
    // This test verifies that the test project can reference
    // and use the main ReservationService library
    let reservationId = ReservationId(System.Guid.NewGuid())
    let customerId = CustomerId(System.Guid.NewGuid())

    // Test configuration
    let defaultCurrency = "EUR"
    let defaultTimeZone = "UTC"

    // Test the new safe Money type
    match Money.Create(100.0M, defaultCurrency) with
    | Ok money ->
        Assert.NotNull(reservationId)
        Assert.NotNull(customerId)
        Assert.Equal(100.0M, money.AmountValue)
        Assert.Equal(defaultCurrency, money.CurrencyValue)
    | Error msg ->
        Assert.True(false, $"Failed to create money: {msg}")

[<Fact>]
let ``Money validation works correctly`` () =
    let defaultCurrency = "EUR"

    // Test that the new Money type prevents illegal states

    // Valid money should work
    match Money.Create(50.0M, "USD") with
    | Ok money ->
        Assert.Equal(50.0M, money.AmountValue)
        Assert.Equal("USD", money.CurrencyValue)
    | Error msg ->
        Assert.True(false, $"Valid money creation failed: {msg}")

    // Invalid money should be rejected
    match Money.Create(-100.0M, defaultCurrency) with
    | Ok _ ->
        Assert.True(false, "Should not create money with negative amount")
    | Error msg ->
        Assert.Contains("positive", msg)

    // Currency string is caller-provided; only non-empty enforced now
    match Money.Create(100.0M, "INVALID") with
    | Ok money ->
        Assert.Equal("INVALID", money.CurrencyValue)
    | Error msg ->
        Assert.True(false, $"Unexpected error for caller-provided currency: {msg}")
