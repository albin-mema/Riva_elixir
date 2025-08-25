namespace ReservationService.Core

open System

// ========== CAR RENTAL DOMAIN TYPES ==========

/// Vehicle category types
type VehicleCategory =
    | Economy
    | Compact
    | Midsize
    | FullSize
    | SUV
    | Luxury
    | Premium
    | Convertible
    | Minivan
    | Truck

/// Transmission types
type TransmissionType =
    | Manual
    | Automatic
    | CVT

/// Fuel types
type FuelType =
    | Gasoline
    | Diesel
    | Electric
    | Hybrid

/// Vehicle status for availability tracking
type VehicleStatus =
    | Available
    | Rented
    | Maintenance
    | Reserved
    | Unavailable
    | Cleaning

/// Vehicle features and amenities
type VehicleFeature =
    | AirConditioning
    | Bluetooth
    | GPS
    | LeatherSeats
    | Sunroof
    | BackupCamera
    | ParkingSensors
    | HeatedSeats
    | PremiumSound
    | USBCharging
    | ChildSeat
    | SkiRack
    | BikeRack
    | TrailerHitch

/// Vehicle specification details
type VehicleSpecification = {
    Make: string
    Model: string
    Year: int
    Category: VehicleCategory
    Transmission: TransmissionType
    FuelType: FuelType
    Seats: int
    Doors: int
    LuggageCapacity: int
    Features: Set<VehicleFeature>
    DailyRate: decimal
    WeeklyRate: decimal
    MonthlyRate: decimal
    MinimumAge: int
    SecurityDeposit: decimal
}

/// Vehicle entity with availability tracking
type Vehicle = {
    Id: string
    LicensePlate: string
    Vin: string
    Specification: VehicleSpecification
    Status: VehicleStatus
    CurrentLocation: string
    OdometerReading: decimal
    LastServiceDate: DateTimeOffset option
    NextServiceDue: DateTimeOffset option
    CreatedAt: DateTimeOffset
    UpdatedAt: DateTimeOffset
}

/// Customer license information
type LicenseInfo = {
    LicenseNumber: string
    IssuingCountry: string
    ExpiryDate: DateTimeOffset
    Class: string
}

/// Customer driving history
type DrivingHistory = {
    YearsLicensed: int
    AccidentsCount: int
    ViolationsCount: int
    LastViolationDate: DateTimeOffset option
}

/// Customer payment method
type PaymentMethod =
    | CreditCard of cardNumber: string * expiry: string * cvv: string * cardholderName: string
    | DebitCard of cardNumber: string * expiry: string * cvv: string * cardholderName: string
    | PayPal of email: string
    | BankTransfer of accountNumber: string * routingNumber: string

/// Customer profile with eligibility validation
type Customer = {
    Id: string
    FirstName: string
    LastName: string
    Email: string
    PhoneNumber: string
    DateOfBirth: DateTimeOffset
    LicenseInfo: LicenseInfo
    DrivingHistory: DrivingHistory
    PaymentMethods: PaymentMethod list
    LoyaltyTier: int
    PreferredVehicleCategories: VehicleCategory list
    CreatedAt: DateTimeOffset
    UpdatedAt: DateTimeOffset
}

/// Rental package types
type RentalPackage =
    | Basic
    | Standard
    | Premium
    | Luxury
    | Business

/// Insurance options
type InsuranceOption =
    | BasicInsurance
    | PremiumInsurance
    | NoInsurance
    | ThirdPartyInsurance

/// Add-on services
type AddOnService =
    | GPSNavigation
    | ChildSeat
    | SkiRack
    | BikeRack
    | AdditionalDriver
    | FuelService
    | AirportDelivery
    | HotelDelivery

/// Rental request details
type RentalRequest = {
    CustomerId: string
    VehicleId: string
    PickupLocation: string
    DropoffLocation: string
    PickupDateTime: DateTimeOffset
    DropoffDateTime: DateTimeOffset
    Package: RentalPackage
    Insurance: InsuranceOption
    AddOns: AddOnService list
    SpecialRequests: string option
    EstimatedMileage: decimal
}

/// Rental status lifecycle
type RentalStatus =
    | Requested
    | Confirmed
    | ReadyForPickup
    | InProgress
    | Completed
    | Cancelled
    | NoShow
    | Overdue

/// Rental inspection report
type InspectionReport = {
    PreRentalOdometer: decimal
    PreRentalCondition: string
    PreRentalPhotos: string list
    PostRentalOdometer: decimal option
    PostRentalCondition: string option
    PostRentalPhotos: string list option
    DamageNotes: string option
    FuelLevel: string option
}

/// Payment status
type PaymentStatus =
    | Pending
    | PartiallyPaid
    | Paid
    | Refunded
    | Failed
    | Cancelled

/// Payment transaction
type PaymentTransaction = {
    Id: string
    RentalId: string
    Amount: decimal
    Method: PaymentMethod
    Status: PaymentStatus
    TransactionDate: DateTimeOffset
    RefundDate: DateTimeOffset option
    RefundAmount: decimal option
    Notes: string option
}

/// Rental entity with full lifecycle
type Rental = {
    Id: string
    Request: RentalRequest
    Status: RentalStatus
    ConfirmationNumber: string
    Vehicle: Vehicle
    Customer: Customer
    Pricing: RentalPricing
    Inspection: InspectionReport option
    Payments: PaymentTransaction list
    CreatedAt: DateTimeOffset
    UpdatedAt: DateTimeOffset
    CompletedAt: DateTimeOffset option
    CancelledAt: DateTimeOffset option
    CancellationReason: string option
}

/// Rental pricing calculation
type RentalPricing = {
    BaseDailyRate: decimal
    DurationDays: int
    DurationWeeks: int
    DurationMonths: int
    Subtotal: decimal
    InsuranceCost: decimal
    AddOnCosts: decimal
    Taxes: decimal
    Fees: decimal
    TotalAmount: decimal
    DepositAmount: decimal
    DepositRefunded: bool
    DepositRefundedDate: DateTimeOffset option
}

/// Location/Branch information
type Location = {
    Id: string
    Name: string
    Address: string
    City: string
    State: string
    PostalCode: string
    Country: string
    PhoneNumber: string
    Email: string
    OperatingHours: (TimeSpan * TimeSpan) list
    AvailableVehicles: Vehicle list
    TotalCapacity: int
    Coordinates: decimal * decimal
    IsAirportLocation: bool
    IsDowntownLocation: bool
}

/// Vehicle availability check request
type AvailabilityRequest = {
    PickupLocation: string
    DropoffLocation: string
    PickupDateTime: DateTimeOffset
    DropoffDateTime: DateTimeOffset
    VehicleCategory: VehicleCategory option
    PreferredFeatures: VehicleFeature list
}

/// Availability response
type AvailabilityResponse = {
    AvailableVehicles: Vehicle list
    RecommendedVehicles: Vehicle list
    TotalPrice: decimal option
    AlternativeLocations: Location list
}

/// Search filters for vehicle availability
type VehicleSearchFilters = {
    Categories: VehicleCategory list
    PriceRange: decimal * decimal option
    Transmission: TransmissionType option
    Features: VehicleFeature list
    PickupLocation: string
    DropoffLocation: string
    PickupDateTime: DateTimeOffset
    DropoffDateTime: DateTimeOffset
    MaxDistance: decimal option
}

/// Error types for car rental operations
type CarRentalError =
    | VehicleNotAvailable of string
    | InvalidRentalDates of string
    | CustomerIneligible of string
    | PaymentFailed of string
    | LocationNotFound of string
    | VehicleNotFound of string
    | InvalidLicense of string
    | InsufficientDeposit of string
    | MaintenanceConflict of string
    | OverlappingReservation of string

/// Result type for car rental operations
type CarRentalResult<'T> = Result<'T, CarRentalError list>

// ========== HELPER FUNCTIONS FOR VALIDATION ==========

module VehicleValidation =
    /// Validate vehicle license plate format
    let validateLicensePlate (plate: string) =
        if String.IsNullOrWhiteSpace plate then Error "License plate cannot be empty"
        elif plate.Length < 3 || plate.Length > 10 then Error "License plate must be 3-10 characters"
        elif not (plate |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '-')) then 
            Error "License plate can only contain letters, numbers, and hyphens"
        else Ok plate

    /// Validate VIN format
    let validateVin (vin: string) =
        if String.IsNullOrWhiteSpace vin then Error "VIN cannot be empty"
        elif vin.Length <> 17 then Error "VIN must be exactly 17 characters"
        elif not (vin |> Seq.forall (fun c -> Char.IsLetterOrDigit c)) then 
            Error "VIN can only contain letters and numbers"
        else Ok vin

    /// Validate vehicle age requirements
    let validateVehicleAge (year: int) =
        let currentYear = DateTime.UtcNow.Year
        if year < 1990 then Error "Vehicle year must be 1990 or newer"
        elif year > currentYear + 1 then Error "Vehicle year cannot be in the future"
        else Ok year

    /// Check if vehicle is available for rental
    let isAvailableForRent (vehicle: Vehicle) (pickup: DateTimeOffset) (dropoff: DateTimeOffset) =
        match vehicle.Status with
        | Available -> true
        | Reserved -> false
        | Rented -> false
        | Maintenance -> false
        | Unavailable -> false
        | Cleaning -> false

module CustomerValidation =
    /// Validate customer age requirements
    let validateCustomerAge (dateOfBirth: DateTimeOffset) =
        let today = DateTimeOffset.UtcNow
        let age = today.Year - dateOfBirth.Year
        if age < 18 then Error "Customer must be at least 18 years old"
        elif age > 100 then Error "Invalid age provided"
        else Ok age

    /// Validate license expiry date
    let validateLicenseExpiry (expiryDate: DateTimeOffset) =
        let today = DateTimeOffset.UtcNow
        if expiryDate <= today then Error "License has expired"
        elif expiryDate > today.AddYears(5) then Error "License expiry date too far in future"
        else Ok expiryDate

    /// Check customer eligibility for rental
    let checkEligibility (customer: Customer) =
        let ageResult = validateCustomerAge customer.DateOfBirth
        let licenseResult = validateLicenseExpiry customer.LicenseInfo.ExpiryDate
        
        match ageResult, licenseResult with
        | Ok age, Ok _ when age < 25 -> 
            // Additional young driver restrictions
            if customer.DrivingHistory.YearsLicensed < 2 then
                Error "Drivers under 25 must have at least 2 years of licensed driving experience"
            else Ok ()
        | Ok _, Ok _ -> Ok ()
        | Error ageError, _ -> Error ageError
        | _, Error licenseError -> Error licenseError

module RentalValidation =
    /// Validate rental date range
    let validateRentalDates (pickup: DateTimeOffset) (dropoff: DateTimeOffset) =
        if pickup >= dropoff then Error "Pickup date must be before dropoff date"
        elif pickup < DateTimeOffset.UtcNow then Error "Pickup date cannot be in the past"
        elif (dropoff - pickup).TotalDays > 365 then Error "Rental duration cannot exceed 365 days"
        else Ok (pickup, dropoff)

    /// Validate rental location availability
    let validateLocationAvailability (location: Location) (pickup: DateTimeOffset) (dropoff: DateTimeOffset) =
        let isWithinOperatingHours (time: DateTimeOffset) (hours: TimeSpan * TimeSpan) =
            let openTime, closeTime = hours
            let timeOnly = time.TimeOfDay
            timeOnly >= openTime && timeOnly <= closeTime

        let isLocationOpen = 
            location.OperatingHours
            |> List.exists (isWithinOperatingHours pickup)
            && List.exists (isWithinOperatingHours dropoff)

        if not isLocationOpen then Error "Location is not available during requested times"
        else Ok ()

    /// Calculate rental duration in days
    let calculateDurationDays (pickup: DateTimeOffset) (dropoff: DateTimeOffset) =
        let duration = dropoff - pickup
        let days = Math.Ceiling(duration.TotalDays)
        if days < 1 then 1 else int days

// ========== PRICING CALCULATION MODULES ==========

module Pricing =
    /// Calculate base rental cost
    let calculateBaseCost (vehicle: Vehicle) durationDays =
        let dailyRate = vehicle.Specification.DailyRate
        let weeklyRate = vehicle.Specification.WeeklyRate
        let monthlyRate = vehicle.Specification.MonthlyRate
        
        if durationDays >= 30 then
            monthlyRate * (decimal (durationDays / 30))
        elif durationDays >= 7 then
            weeklyRate * (decimal (durationDays / 7))
        else
            dailyRate * (decimal durationDays)

    /// Calculate insurance cost
    let calculateInsuranceCost (insurance: InsuranceOption) (baseCost: decimal) =
        match insurance with
        | BasicInsurance -> baseCost * 0.15m
        | PremiumInsurance -> baseCost * 0.25m
        | NoInsurance -> 0.0m
        | ThirdPartyInsurance -> baseCost * 0.10m

    /// Calculate add-on costs
    let calculateAddOnCosts (addOns: AddOnService list) durationDays =
        let addOnCost = function
            | GPSNavigation -> 5.0m * (decimal durationDays)
            | ChildSeat -> 10.0m * (decimal durationDays)
            | SkiRack -> 15.0m * (decimal durationDays)
            | BikeRack -> 15.0m * (decimal durationDays)
            | AdditionalDriver -> 15.0m * (decimal durationDays)
            | FuelService -> 50.0m
            | AirportDelivery -> 25.0m
            | HotelDelivery -> 20.0m
        
        addOns |> List.sumBy addOnCost

    /// Calculate insurance cost with base cost parameter
    let calculateInsuranceCost (insurance: InsuranceOption) (baseCost: decimal) =
        match insurance with
        | BasicInsurance -> baseCost * 0.15m
        | PremiumInsurance -> baseCost * 0.25m
        | NoInsurance -> 0.0m
        | ThirdPartyInsurance -> baseCost * 0.10m

    /// Calculate add-on costs with duration parameter
    let calculateAddOnCostsWithDuration (addOns: AddOnService list) durationDays =
        calculateAddOnCosts addOns durationDays

    /// Calculate total rental cost
    let calculateTotalRentalCost (vehicle: Vehicle) (request: RentalRequest) =
        let durationDays = RentalValidation.calculateDurationDays request.PickupDateTime request.DropoffDateTime
        let baseCost = calculateBaseCost vehicle durationDays
        let insuranceCost = calculateInsuranceCost request.Insurance baseCost
        let addOnCosts = calculateAddOnCosts request.AddOns durationDays
        let subtotal = baseCost + insuranceCost + addOnCosts
        let taxes = subtotal * 0.08m // 8% tax rate
        let fees = if request.PickupLocation <> request.DropoffLocation then 50.0m else 0.0m // One-way fee
        let total = subtotal + taxes + fees
        
        {
            BaseDailyRate = vehicle.Specification.DailyRate
            DurationDays = durationDays
            DurationWeeks = durationDays / 7
            DurationMonths = durationDays / 30
            Subtotal = subtotal
            InsuranceCost = insuranceCost
            AddOnCosts = addOnCosts
            Taxes = taxes
            Fees = fees
            TotalAmount = total
            DepositAmount = vehicle.Specification.SecurityDeposit
            DepositRefunded = false
            DepositRefundedDate = None
        }