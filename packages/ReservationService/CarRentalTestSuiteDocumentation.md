# Car Rental Test Suite Architecture

## Overview

This document describes the comprehensive test suite architecture for the Car Rental functionality, designed following the existing ReservationService test patterns and best practices. The test suite includes 60-70 tests covering common cases, edge cases, and error handling scenarios.

## Test Architecture Overview

### Test Categories and Distribution

| Category | Test Count | Purpose |
|----------|------------|---------|
| DomainTypes Tests | 18 | Validate core domain entities and value objects |
| BusinessLogic Tests | 25 | Test business rules, validation, and pricing logic |
| IntegrationTests | 20 | Test complete rental lifecycle workflows |
| PropertyTests | 15 | Property-based testing for edge cases and invariants |
| ErrorHandlingTests | 15 | Test error scenarios and failure modes |
| **Total** | **93** | **Comprehensive coverage** |

### Test File Structure

```
ReservationService.Tests/
├── Scenarios/
│   ├── CarRental.ScenarioBuilders.fs     # Car rental specific scenario builders
│   └── ScenarioBuilders.fs               # Common scenario builders (existing)
├── Tests/
│   ├── CarRental.DomainTypes.fs           # Domain entity tests
│   ├── CarRental.BusinessLogic.fs         # Business logic tests
│   ├── CarRental.IntegrationTests.fs      # Integration tests
│   ├── CarRental.PropertyTests.fs        # Property-based tests
│   ├── CarRental.ErrorHandlingTests.fs    # Error handling tests
│   ├── DomainTypes.fs                    # Common domain tests (existing)
│   ├── BusinessLogic.fs                  # Common business logic tests (existing)
│   ├── Domain.Generators.fs              # FsCheck generators (existing)
│   └── IntegrationTests.fs               # Common integration tests (existing)
└── ReservationService.Tests.fsproj       # Project configuration
```

## Test Architecture Patterns

### 1. Domain-Driven Testing Approach

The test suite follows a domain-driven design approach with clear separation of concerns:

```fsharp
// Domain Types Tests
[<Fact>]
let ``Vehicle should be created with valid specification`` () =
    let vehicle = createStandardVehicle()
    vehicle.Specification.Make |> should equal "Toyota"
    vehicle.Specification.Model |> should equal "Camry"
    vehicle.Status |> should equal Available
```

### 2. Builder Pattern for Test Data

Comprehensive scenario builders enable consistent test data creation:

```fsharp
// Vehicle Builder
let vehicle = VehicleBuilder()
    .WithId("veh-001")
    .WithMakeAndModel("Toyota", "Camry")
    .AsAvailable()
    .WithAutomaticTransmission()
    .WithGPS()
    .Build()

// Customer Builder
let customer = CustomerBuilder()
    .WithId("cust-001")
    .WithName("John", "Doe")
    .WithLicenseInfo("DL1234567", "USA")
    .WithLoyaltyTier(1)
    .Build()
```

### 3. Property-Based Testing with FsCheck

Leverage FsCheck for comprehensive edge case testing:

```fsharp
[<Property>]
let ``Vehicle equality should be consistent`` (vehicle: Vehicle) =
    vehicle = vehicle // Reflexivity
    vehicle = vehicle // Symmetry
    vehicle = vehicle // Transitivity
```

### 4. Integration Testing with Complete Workflows

Test complete rental lifecycle end-to-end:

```fsharp
[<Fact>]
let ``Complete rental lifecycle should work end-to-end`` () =
    // Arrange: Create test data
    let vehicle = createStandardVehicle()
    let customer = createStandardCustomer()
    let request = createStandardRentalRequest()
    
    // Act: Simulate complete rental lifecycle
    let rental = createCompleteRentalScenario()
    
    // Assert: Verify final state
    rental.Status |> should equal Completed
    rental.Payments |> should haveLength 1
```

## Test Categories in Detail

### 1. DomainTypes Tests (18 tests)

**Purpose**: Validate core domain entities and value objects

**Coverage**:
- **Vehicle Tests (6)**: Creation, validation, status transitions, specification validation
- **Customer Tests (5)**: Creation, validation, license info, driving history
- **Rental Tests (4)**: Creation, status validation, pricing calculation
- **Location Tests (3)**: Creation, capacity management, operating hours

**Key Patterns**:
```fsharp
// Vehicle validation tests
[<Fact>]
let ``Valid vehicle should pass validation`` () =
    let vehicle = createStandardVehicle()
    VehicleValidation.validateVehicle vehicle |> should equal Ok

// Customer validation tests
[<Fact>]
let ``Customer with expired license should fail validation`` () =
    let customer = createCustomerWithExpiredLicense()
    match CustomerValidation.validateCustomer customer with
    | Ok _ -> failwith "Should detect expired license"
    | Error msg -> msg |> should contain "expired"
```

### 2. BusinessLogic Tests (25 tests)

**Purpose**: Test business rules, validation, and pricing logic

**Coverage**:
- **Availability Logic (6)**: Vehicle availability, time conflicts, location capacity
- **Pricing Logic (8)**: Daily/weekly/monthly rates, insurance, add-ons, discounts
- **Validation Logic (6)**: Customer validation, vehicle validation, rental validation
- **Business Rules (5)**: Status transitions, cancellation policies, damage assessment

**Key Patterns**:
```fsharp
// Availability testing
[<Fact>]
let ``Should handle vehicle double booking conflict`` () =
    let vehicle = createStandardVehicle()
    let overlappingRequest = createOverlappingRentalRequest()
    let result = VehicleValidation.isAvailableForRent vehicle overlappingRequest.PickupDateTime overlappingRequest.DropoffDateTime
    result |> should equal false

// Pricing testing
[<Fact>]
let ``Pricing calculation should be consistent`` () =
    let vehicle = createStandardVehicle()
    let request = createStandardRentalRequest()
    let pricing = Pricing.calculateTotalRentalCost vehicle request
    pricing.TotalAmount |> should be (greaterThan pricing.Subtotal)
```

### 3. IntegrationTests (20 tests)

**Purpose**: Test complete rental lifecycle workflows

**Coverage**:
- **Complete Lifecycle (5)**: End-to-end rental process from request to completion
- **Multi-Location (4)**: One-way rentals, cross-location availability
- **Pricing and Payment (4)**: Complete pricing calculation, payment processing
- **Inspection and Damage (4)**: Pre/post rental inspection, damage assessment
- **Seasonal and Location (3)**: Seasonal pricing, airport location pricing
- **Fleet Management (4)**: Multi-vehicle availability, vehicle reassignment

**Key Patterns**:
```fsharp
// Complete lifecycle testing
[<Fact>]
let ``Complete rental lifecycle should work end-to-end`` () =
    let rental = createCompleteRentalScenario()
    rental.Status |> should equal Completed
    rental.Payments |> should haveLength 1

// Multi-location testing
[<Fact>]
let ``One-way rental workflow should work correctly`` () =
    let rental = createOneWayRentalScenario()
    rental.Pricing.Fees |> should equal 50.0m // One-way fee
```

### 4. PropertyTests (15 tests)

**Purpose**: Property-based testing for edge cases and invariants

**Coverage**:
- **Equality Properties (4)**: Vehicle, customer, pricing, location equality
- **Validation Properties (4)**: Vehicle, customer, rental, location validation
- **Calculation Properties (3)**: Pricing calculation, time calculation, money calculation
- **Availability Properties (2)**: Vehicle availability, location capacity
- **Status Transition Properties (2)**: Rental status transitions

**Key Patterns**:
```fsharp
// Property-based testing
[<Property>]
let ``Vehicle equality should be consistent`` (vehicle: Vehicle) =
    vehicle = vehicle // Reflexivity
    vehicle = vehicle // Symmetry
    vehicle = vehicle // Transitivity

[<Property>]
let ``Pricing calculation should be consistent`` (dailyRate: decimal) (days: int) =
    let vehicle = createVehicleWithRate dailyRate
    let request = createRentalRequestWithDays days
    let pricing = Pricing.calculateTotalRentalCost vehicle request
    pricing.DurationDays |> equal days
```

### 5. ErrorHandlingTests (15 tests)

**Purpose**: Test error scenarios and failure modes

**Coverage**:
- **Availability Conflicts (4)**: Double booking, same-day pickup, past dates, maintenance
- **Pricing Errors (3)**: Negative pricing, zero duration, invalid currency
- **Lifecycle Failures (4)**: Invalid status transitions, cancellation after pickup, payment failure
- **Validation Errors (4)**: License expiry, insufficient age, poor driving history, operating hours

**Key Patterns**:
```fsharp
// Error handling testing
[<Fact>]
let ``Should handle vehicle double booking conflict`` () =
    let vehicle = createStandardVehicle()
    let overlappingRequest = createOverlappingRentalRequest()
    let result = VehicleValidation.validateVehicleAvailability vehicle overlappingRequest.PickupDateTime overlappingRequest.DropoffDateTime
    match result with
    | Ok _ -> failwith "Should detect availability conflict"
    | Error msg -> msg |> should contain "not available"

[<Fact>]
let ``Should handle payment failure error`` () =
    let failedPayment = createFailedPayment()
    match PaymentValidation.validatePayment failedPayment with
    | Ok _ -> failwith "Should detect payment failure"
    | Error msg -> msg |> should contain "failed"
```

## Car Rental Specific Patterns

### 1. Vehicle Category Testing

```fsharp
// Test different vehicle categories
[<Fact>]
let ``Luxury vehicle should have higher rates`` () =
    let luxuryVehicle = VehicleBuilder().AsLuxury().Build()
    let standardVehicle = VehicleBuilder().AsStandard().Build()
    luxuryVehicle.Specification.DailyRate |> should be (greaterThan standardVehicle.Specification.DailyRate)
```

### 2. Insurance Package Testing

```fsharp
// Test different insurance packages
[<Fact>]
let ``Premium insurance should cost more than basic`` () =
    let requestWithPremium = { createStandardRentalRequest() with Insurance = PremiumInsurance }
    let requestWithBasic = { createStandardRentalRequest() with Insurance = BasicInsurance }
    let premiumPricing = Pricing.calculateTotalRentalCost createStandardVehicle requestWithPremium
    let basicPricing = Pricing.calculateTotalRentalCost createStandardVehicle requestWithBasic
    premiumPricing.InsuranceCost |> should be (greaterThan basicPricing.InsuranceCost)
```

### 3. Add-on Feature Testing

```fsharp
// Test add-on features
[<Fact>]
let ``GPS add-on should increase total cost`` () =
    let requestWithGPS = { createStandardRentalRequest() with AddOns = [GPSNavigation] }
    let requestWithoutGPS = { createStandardRentalRequest() with AddOns = [] }
    let pricingWithGPS = Pricing.calculateTotalRentalCost createStandardVehicle requestWithGPS
    let pricingWithoutGPS = Pricing.calculateTotalRentalCost createStandardVehicle requestWithoutGPS
    pricingWithGPS.TotalAmount |> should be (greaterThan pricingWithoutGPS.TotalAmount)
```

### 4. Loyalty Program Testing

```fsharp
// Test loyalty discounts
[<Fact>]
let ``Gold customer should receive discount`` () =
    let goldCustomer = { createStandardCustomer() with LoyaltyTier = 2 }
    let silverCustomer = { createStandardCustomer() with LoyaltyTier = 1 }
    let request = createStandardRentalRequest()
    let goldPricing = Pricing.calculateTotalRentalCost createStandardVehicle request
    let silverPricing = Pricing.calculateTotalRentalCost createStandardVehicle request
    goldPricing.Subtotal |> should be (lessThan silverPricing.Subtotal) // Gold gets discount
```

### 5. Seasonal Pricing Testing

```fsharp
// Test seasonal pricing
[<Fact>]
let ``Summer pricing should be higher than winter`` () =
    let summerRequest = createSummerRentalRequest()
    let winterRequest = createWinterRentalRequest()
    let summerPricing = Pricing.calculateTotalRentalCost createStandardVehicle summerRequest
    let winterPricing = Pricing.calculateTotalRentalCost createStandardVehicle winterRequest
    summerPricing.Subtotal |> should be (greaterThan winterPricing.Subtotal)
```

## Test Execution and Configuration

### Test Runner Configuration

```xml
<!-- ReservationService.Tests.fsproj -->
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <IsPackable>false</IsPackable>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="17.8.0" />
    <PackageReference Include="xunit" Version="2.6.1" />
    <PackageReference Include="xunit.runner.visualstudio" Version="2.5.3" />
    <PackageReference Include="FsCheck" Version="14.0.0" />
    <PackageReference Include="FsCheck.Xunit" Version="14.0.0" />
    <PackageReference Include="Swensen.Unquote" Version="6.1.0" />
  </ItemGroup>
</Project>
```

### Test Execution Commands

```bash
# Run all tests
dotnet test

# Run specific test category
dotnet test --filter "Category=DomainTypes"

# Run with detailed output
dotnet test --logger "console;verbosity=detailed"

# Run property-based tests with more iterations
dotnet test --filter "Category=PropertyTests" --collect:"XPlat Code Coverage"
```

## Best Practices and Guidelines

### 1. Test Data Management

- **Use builders consistently** for creating test data
- **Avoid magic numbers** - use named constants
- **Keep test data realistic** but minimal
- **Separate test data creation from test logic**

### 2. Test Organization

- **Group related tests** logically by functionality
- **Use descriptive test names** that clearly indicate what's being tested
- **Follow AAA pattern**: Arrange, Act, Assert
- **Keep tests focused** on a single behavior

### 3. Property-Based Testing

- **Use FsCheck generators** for complex data
- **Test invariants** that should always hold true
- **Combine with example-based tests** for specific scenarios
- **Set appropriate sizes** for generated test data

### 4. Error Handling

- **Test both success and failure cases**
- **Verify error messages** are meaningful
- **Test edge cases** thoroughly
- **Use proper exception handling** in tests

### 5. Integration Testing

- **Test complete workflows** end-to-end
- **Use realistic scenarios** that mirror real usage
- **Test error scenarios** in integration tests
- **Keep integration tests** focused on integration points

## Continuous Integration

### CI Pipeline Configuration

```yaml
# .github/workflows/car-rental-tests.yml
name: Car Rental Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - name: Setup .NET
      uses: actions/setup-dotnet@v4
      with:
        dotnet-version: '8.0.x'
    - name: Restore dependencies
      run: dotnet restore
    - name: Build
      run: dotnet build --no-restore
    - name: Run tests
      run: dotnet test --no-build --verbosity normal
    - name: Run tests with coverage
      run: dotnet test --no-build --collect:"XPlat Code Coverage" --results-directory ./test-results
```

### Test Metrics and Quality Gates

- **Code Coverage**: Minimum 80% coverage for car rental code
- **Test Execution Time**: Keep tests under 30 seconds each
- **Test Reliability**: All tests must be deterministic
- **Error Coverage**: All error scenarios must be tested

## Future Enhancements

### 1. Performance Testing

- Add performance benchmarks for pricing calculations
- Test vehicle availability checking performance
- Validate database query performance for large datasets

### 2. Load Testing

- Test concurrent rental requests
- Validate payment processing under load
- Test fleet management operations with high volume

### 3. Chaos Testing

- Introduce random failures in external services
- Test system resilience under various failure scenarios
- Validate recovery mechanisms

### 4. Contract Testing

- Add API contract tests for external integrations
- Test payment gateway interfaces
- Validate third-party service contracts

## Conclusion

This comprehensive test suite architecture provides robust coverage of the car rental functionality, following established patterns and best practices. The combination of domain-driven testing, property-based testing, and comprehensive integration testing ensures high-quality, reliable code that handles both common scenarios and edge cases effectively.

The test suite is designed to be maintainable, extensible, and aligned with the existing ReservationService testing patterns, making it easy to add new features and modify existing functionality with confidence.