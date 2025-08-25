# Domain-Agnostic Validation Test Template Guide

## Overview
This template provides a standardized structure for testing reservation validation rules across multiple domains (Beach Umbrella, Car Rental, Restaurant). It focuses on common validation concerns while allowing domain-specific implementations.

## Core Components

### Builders
- **ReservationBuilder**: Base class for constructing reservation contexts
- **ResourceBuilder**: Base class for defining resource constraints

### Validation Rules
Implement `IValidationRule` interface:
```fsharp
type IValidationRule =
    abstract Validate : ReservationContext -> ValidationResult
```

### Test Organization
```
Tests/
├── BasicValidation/       # Core rule validations
├── EdgeCases/             # Boundary condition tests
├── Integration/           # Multi-rule scenarios
└── Generators/            # Property-based test helpers
```

## Implementation Steps

1. **Create Domain Builders**:
```fsharp
type RestaurantReservationBuilder() =
    inherit ReservationBuilder()
    override _.Build() = // Domain-specific logic
```

2. **Implement Validation Rules**:
```fsharp
type CapacityRule() =
    interface IValidationRule with
        member _.Validate(context) =
            // Domain-specific capacity check
```

3. **Write Tests**:
```fsharp
module CapacityValidation =
    [<Fact>]
    let ``Given restaurant table When party exceeds capacity Then rejects reservation`` () =
        // Test setup using builders
```

## Best Practices
- Use property-based testing for rule validation
- Follow Given/When/Then naming convention
- Keep test data generation in Generators/