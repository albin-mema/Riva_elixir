namespace ReservationService.Tests.Scenarios

open System
open System.Collections.Generic
open System.Text.Json
open ReservationService.Core

/// Common builders and helpers for car rental scenario tests
module CarRentalScenarioBuilders =

    // ========== VEHICLE BUILDER ==========

    type VehicleBuilder() =
        let mutable id = "veh-001"
        let mutable licensePlate = "ABC123"
        let mutable vin = "1HGCM82633A123456"
        let mutable make = "Toyota"
        let mutable model = "Camry"
        let mutable year = 2023
        let mutable category = Midsize
        let mutable transmission = Automatic
        let mutable fuelType = Gasoline
        let mutable seats = 5
        let mutable doors = 4
        let mutable luggageCapacity = 3
        let mutable features = Set.empty<VehicleFeature>
        let mutable dailyRate = 45.0m
        let mutable weeklyRate = 280.0m
        let mutable monthlyRate = 950.0m
        let mutable minimumAge = 21
        let mutable securityDeposit = 200.0m
        let mutable status = Available
        let mutable currentLocation = "NYC-Downtown"
        let mutable odometerReading = 15000.0m
        let mutable lastServiceDate = None
        let mutable nextServiceDue = DateTimeOffset.UtcNow.AddDays(30.0)
        let mutable createdAt = DateTimeOffset.UtcNow
        let mutable updatedAt = DateTimeOffset.UtcNow

        member this.WithId(vehicleId: string) =
            id <- vehicleId; this

        member this.WithLicensePlate(plate: string) =
            licensePlate <- plate; this

        member this.WithVin(vinNumber: string) =
            vin <- vinNumber; this

        member this.WithMakeAndModel(m: string, mo: string) =
            make <- m; model <- mo; this

        member this.WithYear(y: int) =
            year <- y; this

        member this.WithCategory(cat: VehicleCategory) =
            category <- cat; this

        member this.WithTransmission(trans: TransmissionType) =
            transmission <- trans; this

        member this.WithFuelType(fuel: FuelType) =
            fuelType <- fuel; this

        member this.WithCapacity(s: int, d: int, l: int) =
            seats <- s; doors <- d; luggageCapacity <- l; this

        member this.WithFeature(feature: VehicleFeature) =
            features <- features.Add feature; this

        member this.WithFeatures(featureList: VehicleFeature list) =
            features <- Set.ofList featureList; this

        member this.WithRates(daily: decimal, weekly: decimal, monthly: decimal) =
            dailyRate <- daily; weeklyRate <- weekly; monthlyRate <- monthly; this

        member this.WithRentalRequirements(minAge: int, deposit: decimal) =
            minimumAge <- minAge; securityDeposit <- deposit; this

        member this.WithStatus(stat: VehicleStatus) =
            status <- stat; this

        member this.WithLocation(location: string) =
            currentLocation <- location; this

        member this.WithOdometer(reading: decimal) =
            odometerReading <- reading; this

        member this.WithServiceDates(last: DateTimeOffset option, next: DateTimeOffset) =
            lastServiceDate <- last; nextServiceDue <- next; this

        member this.AsEconomy() = this.WithCategory Economy
        member this.AsCompact() = this.WithCategory Compact
        member this.AsSUV() = this.WithCategory SUV
        member this.AsLuxury() = this.WithCategory Luxury
        member this.AsConvertible() = this.WithCategory Convertible
        member this.AsMinivan() = this.WithCategory Minivan
        member this.AsTruck() = this.WithCategory Truck

        member this.WithAutomaticTransmission() = this.WithTransmission Automatic
        member this.WithManualTransmission() = this.WithTransmission Manual
        member this.WithCVTTransmission() = this.WithTransmission CVT

        member this.WithGasoline() = this.WithFuelType Gasoline
        member this.WithDiesel() = this.WithFuelType Diesel
        member this.WithElectric() = this.WithFuelType Electric
        member this.WithHybrid() = this.WithFuelType Hybrid

        member this.WithAirConditioning() = this.WithFeature AirConditioning
        member this.WithBluetooth() = this.WithFeature Bluetooth
        member this.WithGPS() = this.WithFeature GPS
        member this.WithLeatherSeats() = this.WithFeature LeatherSeats
        member this.WithSunroof() = this.WithFeature Sunroof
        member this.WithBackupCamera() = this.WithFeature BackupCamera
        member this.WithParkingSensors() = this.WithFeature ParkingSensors
        member this.WithHeatedSeats() = this.WithFeature HeatedSeats
        member this.WithPremiumSound() = this.WithFeature PremiumSound
        member this.WithUSBCharging() = this.WithFeature USBCharging

        member this.AsAvailable() = this.WithStatus Available
        member this.AsRented() = this.WithStatus Rented
        member this.AsMaintenance() = this.WithStatus Maintenance
        member this.AsReserved() = this.WithStatus Reserved
        member this.AsUnavailable() = this.WithStatus Unavailable
        member this.AsCleaning() = this.WithStatus Cleaning

        member this.Build() =
            let specification = {
                Make = make
                Model = model
                Year = year
                Category = category
                Transmission = transmission
                FuelType = fuelType
                Seats = seats
                Doors = doors
                LuggageCapacity = luggageCapacity
                Features = features
                DailyRate = dailyRate
                WeeklyRate = weeklyRate
                MonthlyRate = monthlyRate
                MinimumAge = minimumAge
                SecurityDeposit = securityDeposit
            }
            
            {
                Id = id
                LicensePlate = licensePlate
                Vin = vin
                Specification = specification
                Status = status
                CurrentLocation = currentLocation
                OdometerReading = odometerReading
                LastServiceDate = lastServiceDate
                NextServiceDue = nextServiceDue
                CreatedAt = createdAt
                UpdatedAt = updatedAt
            }

    // ========== CUSTOMER BUILDER ==========

    type CustomerBuilder() =
        let mutable id = "cust-001"
        let mutable firstName = "John"
        let mutable lastName = "Doe"
        let mutable email = "john.doe@example.com"
        let mutable phoneNumber = "+1-555-0123"
        let mutable dateOfBirth = DateTimeOffset.UtcNow.AddYears(-30)
        let mutable licenseNumber = "DL1234567"
        let mutable issuingCountry = "USA"
        let mutable licenseExpiry = DateTimeOffset.UtcNow.AddYears(2)
        let mutable licenseClass = "Class C"
        let mutable yearsLicensed = 5
        let mutable accidentsCount = 0
        let mutable violationsCount = 1
        let mutable lastViolationDate = Some (DateTimeOffset.UtcNow.AddDays(-365))
        let mutable paymentMethods = []
        let mutable loyaltyTier = 1
        let mutable preferredCategories = [Midsize; SUV]
        let mutable createdAt = DateTimeOffset.UtcNow
        let mutable updatedAt = DateTimeOffset.UtcNow

        member this.WithId(customerId: string) =
            id <- customerId; this

        member this.WithName(first: string, last: string) =
            firstName <- first; lastName <- last; this

        member this.WithContact(email: string, phone: string) =
            this.Email <- email; this.PhoneNumber <- phone; this

        member this.WithAge(age: int) =
            dateOfBirth <- DateTimeOffset.UtcNow.AddYears(-age); this

        member this.WithLicenseInfo(number: string, country: string, expiry: DateTimeOffset, licenseClass: string) =
            licenseNumber <- number; issuingCountry <- country; licenseExpiry <- expiry; this.LicenseClass <- licenseClass; this

        member this.WithDrivingHistory(years: int, accidents: int, violations: int, lastViolation: DateTimeOffset option) =
            yearsLicensed <- years; accidentsCount <- accidents; violationsCount <- violations; lastViolationDate <- lastViolation; this

        member this.WithPaymentMethod(method: PaymentMethod) =
            paymentMethods <- method :: paymentMethods; this

        member this.WithLoyaltyTier(tier: int) =
            loyaltyTier <- tier; this

        member this.WithPreferredCategories(categories: VehicleCategory list) =
            preferredCategories <- categories; this

        member this.AsVIP() = this.WithLoyaltyTier 3
        member this.AsGold() = this.WithLoyaltyTier 2
        member this.AsBronze() = this.WithLoyaltyTier 1

        member this.WithCleanDrivingRecord() = this.WithDrivingHistory 5 0 0 None
        member this.WithMinorViolations() = this.WithDrivingHistory 3 0 1 (Some (DateTimeOffset.UtcNow.AddDays(-365)))
        member this.WithMajorViolations() = this.WithDrivingHistory 2 1 3 (Some (DateTimeOffset.UtcNow.AddDays(-180)))

        member this.WithCreditCard(cardNumber: string, expiry: string, cvv: string, name: string) =
            this.WithPaymentMethod (CreditCard(cardNumber, expiry, cvv, name))

        member this.WithPayPal(email: string) =
            this.WithPaymentMethod (PayPal email)

        member this.WithBankTransfer(account: string, routing: string) =
            this.WithPaymentMethod (BankTransfer(account, routing))

        member this.Email
            with get() = email
            and set(value) = email <- value

        member this.PhoneNumber
            with get() = phoneNumber
            and set(value) = phoneNumber <- value

        member this.LicenseClass
            with get() = licenseClass
            and set(value) = licenseClass <- value

        member this.Build() =
            let licenseInfo = {
                LicenseNumber = licenseNumber
                IssuingCountry = issuingCountry
                ExpiryDate = licenseExpiry
                Class = licenseClass
            }
            
            let drivingHistory = {
                YearsLicensed = yearsLicensed
                AccidentsCount = accidentsCount
                ViolationsCount = violationsCount
                LastViolationDate = lastViolationDate
            }
            
            {
                Id = id
                FirstName = firstName
                LastName = lastName
                Email = email
                PhoneNumber = phoneNumber
                DateOfBirth = dateOfBirth
                LicenseInfo = licenseInfo
                DrivingHistory = drivingHistory
                PaymentMethods = paymentMethods
                LoyaltyTier = loyaltyTier
                PreferredVehicleCategories = preferredCategories
                CreatedAt = createdAt
                UpdatedAt = updatedAt
            }

    // ========== RENTAL REQUEST BUILDER ==========

    type RentalRequestBuilder() =
        let mutable customerId = "cust-001"
        let mutable vehicleId = "veh-001"
        let mutable pickupLocation = "NYC-Downtown"
        let mutable dropoffLocation = "NYC-Downtown"
        let mutable pickupDateTime = DateTimeOffset.UtcNow.AddDays(1)
        let mutable dropoffDateTime = DateTimeOffset.UtcNow.AddDays(3)
        let mutable rentalPackage = Standard
        let mutable insurance = BasicInsurance
        let mutable addOns = []
        let mutable specialRequests = None
        let mutable estimatedMileage = 200.0m

        member this.WithCustomer(customerId': string) =
            customerId <- customerId'; this

        member this.WithVehicle(vehicleId': string) =
            vehicleId <- vehicleId'; this

        member this.WithLocations(pickup: string, dropoff: string) =
            pickupLocation <- pickup; dropoffLocation <- dropoff; this

        member this.WithDateTimeRange(pickup: DateTimeOffset, dropoff: DateTimeOffset) =
            pickupDateTime <- pickup; dropoffDateTime <- dropoff; this

        member this.WithDuration(days: int) =
            dropoffDateTime <- pickupDateTime.AddDays(float days); this

        member this.WithPackage(pkg: RentalPackage) =
            rentalPackage <- pkg; this

        member this.WithInsurance(ins: InsuranceOption) =
            insurance <- ins; this

        member this.WithAddOn(addOn: AddOnService) =
            addOns <- addOn :: addOns; this

        member this.WithAddOns(addOnList: AddOnService list) =
            addOns <- addOnList @ addOns; this

        member this.WithSpecialRequests(requests: string) =
            specialRequests <- Some requests; this

        member this.WithEstimatedMileage(mileage: decimal) =
            estimatedMileage <- mileage; this

        member this.AsBasicPackage() = this.WithPackage Basic
        member this.AsStandardPackage() = this.WithPackage Standard
        member this.AsPremiumPackage() = this.WithPackage Premium
        member this.AsLuxuryPackage() = this.WithPackage Luxury
        member this.AsBusinessPackage() = this.WithPackage Business

        member this.WithBasicInsurance() = this.WithInsurance BasicInsurance
        member this.WithPremiumInsurance() = this.WithInsurance PremiumInsurance
        member this.WithNoInsurance() = this.WithInsurance NoInsurance
        member this.WithThirdPartyInsurance() = this.WithInsurance ThirdPartyInsurance

        member this.WithGPS() = this.WithAddOn GPSNavigation
        member this.WithChildSeat() = this.WithAddOn ChildSeat
        member this.WithSkiRack() = this.WithAddOn SkiRack
        member this.WithBikeRack() = this.WithAddOn BikeRack
        member this.WithAdditionalDriver() = this.WithAddOn AdditionalDriver
        member this.WithFuelService() = this.WithAddOn FuelService
        member this.WithAirportDelivery() = this.WithAddOn AirportDelivery
        member this.WithHotelDelivery() = this.WithAddOn HotelDelivery

        member this.AsOneWayRental() = this.WithLocations("NYC-Downtown", "NYC-Airport")
        member this.AsRoundTrip() = this.WithLocations("NYC-Downtown", "NYC-Downtown")
        member this.AsMultiLocation() = this.WithLocations("NYC-Downtown", "Boston-Downtown")

        member this.AsWeekendRental() =
            let saturday = DateTimeOffset.UtcNow.AddDays(1).Date.AddDays(float ((6 - int DateTimeOffset.UtcNow.DayOfWeek + 7) % 7))
            this.WithDateTimeRange(saturday, saturday.AddDays(2))

        member this.AsWeeklyRental() =
            let monday = DateTimeOffset.UtcNow.AddDays(1).Date.AddDays(float ((1 - int DateTimeOffset.UtcNow.DayOfWeek + 7) % 7))
            this.WithDateTimeRange(monday, monday.AddDays(6))

        member this.AsMonthlyRental() =
            let firstOfMonth = DateTimeOffset.UtcNow.AddDays(1).Date.AddDays(float (1 - DateTimeOffset.UtcNow.Day))
            this.WithDateTimeRange(firstOfMonth, firstOfMonth.AddDays(29))

        member this.Build() =
            {
                CustomerId = customerId
                VehicleId = vehicleId
                PickupLocation = pickupLocation
                DropoffLocation = dropoffLocation
                PickupDateTime = pickupDateTime
                DropoffDateTime = dropoffDateTime
                Package = rentalPackage
                Insurance = insurance
                AddOns = addOns
                SpecialRequests = specialRequests
                EstimatedMileage = estimatedMileage
            }

    // ========== LOCATION BUILDER ==========

    type LocationBuilder() =
        let mutable id = "loc-001"
        let mutable name = "NYC Downtown"
        let mutable address = "123 Main St"
        let mutable city = "New York"
        let mutable state = "NY"
        let mutable postalCode = "10001"
        let mutable country = "USA"
        let mutable phoneNumber = "+1-555-0123"
        let mutable email = "nyc@rental.com"
        let mutable operatingHours = [(TimeSpan(9, 0, 0), TimeSpan(17, 0, 0))]
        let mutable availableVehicles = []
        let mutable totalCapacity = 50
        let mutable latitude = 40.7128m
        let mutable longitude = -74.0060m
        let mutable isAirportLocation = false
        let mutable isDowntownLocation = true

        member this.WithId(locationId: string) =
            id <- locationId; this

        member this.WithName(n: string) =
            name <- n; this

        member this.WithAddress(addr: string, c: string, s: string, p: string, cnt: string) =
            address <- addr; city <- c; state <- s; postalCode <- p; country <- cnt; this

        member this.WithContact(phone: string, email': string) =
            phoneNumber <- phone; email <- email'; this

        member this.WithOperatingHours(hours: (TimeSpan * TimeSpan) list) =
            operatingHours <- hours; this

        member this.WithDailyHours(openTime: TimeSpan, closeTime: TimeSpan) =
            operatingHours <- [(openTime, closeTime)]; this

        member this.With24HourService() =
            operatingHours <- [(TimeSpan.Zero, TimeSpan(23, 59, 59))]; this

        member this.WithVehicles(vehicles: Vehicle list) =
            availableVehicles <- vehicles; this

        member this.WithCapacity(capacity: int) =
            totalCapacity <- capacity; this

        member this.WithCoordinates(lat: decimal, lng: decimal) =
            latitude <- lat; longitude <- lng; this

        member this.AsAirportLocation() =
            isAirportLocation <- true; isDowntownLocation <- false; this

        member this.AsDowntownLocation() =
            isDowntownLocation <- true; isAirportLocation <- false; this

        member this.AsSuburbanLocation() =
            isDowntownLocation <- false; isAirportLocation <- false; this

        member this.AsStationLocation() =
            name <- name.Replace("Downtown", "Station"); this

        member this.Build() =
            {
                Id = id
                Name = name
                Address = address
                City = city
                State = state
                PostalCode = postalCode
                Country = country
                PhoneNumber = phoneNumber
                Email = email
                OperatingHours = operatingHours
                AvailableVehicles = availableVehicles
                TotalCapacity = totalCapacity
                Coordinates = (latitude, longitude)
                IsAirportLocation = isAirportLocation
                IsDowntownLocation = isDowntownLocation
            }

    // ========== RENTAL BUILDER ==========

    type RentalBuilder() =
        let mutable id = "rental-001"
        let mutable confirmationNumber = "CONF123456"
        let mutable status = Requested
        let mutable vehicle = Unchecked.defaultof<Vehicle>
        let mutable customer = Unchecked.defaultof<Customer>
        let mutable request = Unchecked.defaultof<RentalRequest>
        let mutable pricing = Unchecked.defaultof<RentalPricing>
        let mutable inspection = None
        let mutable payments = []
        let mutable createdAt = DateTimeOffset.UtcNow
        let mutable updatedAt = DateTimeOffset.UtcNow
        let mutable completedAt = None
        let mutable cancelledAt = None
        let mutable cancellationReason = None

        member this.WithId(rentalId: string) =
            id <- rentalId; this

        member this.WithConfirmationNumber(number: string) =
            confirmationNumber <- number; this

        member this.WithVehicle(v: Vehicle) =
            vehicle <- v; this

        member this.WithCustomer(c: Customer) =
            customer <- c; this

        member this.WithRequest(r: RentalRequest) =
            request <- r; this

        member this.WithPricing(p: RentalPricing) =
            pricing <- p; this

        member this.WithStatus(stat: RentalStatus) =
            status <- stat; this

        member this.WithInspection(inspection': InspectionReport) =
            inspection <- Some inspection'; this

        member this.WithPayment(payment: PaymentTransaction) =
            payments <- payment :: payments; this

        member this.WithPayments(paymentList: PaymentTransaction list) =
            payments <- paymentList @ payments; this

        member this.WithTimestamps(created: DateTimeOffset, updated: DateTimeOffset) =
            createdAt <- created; updatedAt <- updated; this

        member this.WithCompletionTime(completed: DateTimeOffset) =
            completedAt <- Some completed; this

        member this.WithCancellation(cancelled: DateTimeOffset, reason: string) =
            cancelledAt <- Some cancelled; cancellationReason <- Some reason; this

        member this.AsRequested() = this.WithStatus Requested
        member this.AsConfirmed() = this.WithStatus Confirmed
        member this.AsReadyForPickup() = this.WithStatus ReadyForPickup
        member this.AsInProgress() = this.WithStatus InProgress
        member this.AsCompleted() = this.WithStatus Completed
        member this.AsCancelled() = this.WithStatus Cancelled
        member this.AsNoShow() = this.WithStatus NoShow
        member this.AsOverdue() = this.WithStatus Overdue

        member this.WithPreRentalInspection(odometer: decimal, condition: string, photos: string list) =
            let inspectionReport = {
                PreRentalOdometer = odometer
                PreRentalCondition = condition
                PreRentalPhotos = photos
                PostRentalOdometer = None
                PostRentalCondition = None
                PostRentalPhotos = None
                DamageNotes = None
                FuelLevel = None
            }
            this.WithInspection inspectionReport

        member this.WithPostRentalInspection(odometer: decimal, condition: string, photos: string list, damage: string option, fuel: string option) =
            let inspectionReport = {
                PreRentalOdometer = 0m // Would be set from pre-inspection
                PreRentalCondition = "" // Would be set from pre-inspection
                PreRentalPhotos = [] // Would be set from pre-inspection
                PostRentalOdometer = Some odometer
                PostRentalCondition = Some condition
                PostRentalPhotos = Some photos
                DamageNotes = damage
                FuelLevel = fuel
            }
            this.WithInspection inspectionReport

        member this.WithSuccessfulPayment(amount: decimal, method: PaymentMethod) =
            let payment = {
                Id = $"payment-{System.Guid.NewGuid()}"
                RentalId = id
                Amount = amount
                Method = method
                Status = Paid
                TransactionDate = DateTimeOffset.UtcNow
                RefundDate = None
                RefundAmount = None
                Notes = None
            }
            this.WithPayment payment

        member this.WithFailedPayment(amount: decimal, method: PaymentMethod, reason: string) =
            let payment = {
                Id = $"payment-{System.Guid.NewGuid()}"
                RentalId = id
                Amount = amount
                Method = method
                Status = Failed
                TransactionDate = DateTimeOffset.UtcNow
                RefundDate = None
                RefundAmount = None
                Notes = Some reason
            }
            this.WithPayment payment

        member this.WithCustomerCancellation(reason: string) =
            this.WithCancellation DateTimeOffset.UtcNow reason

        member this.WithBusinessCancellation(reason: string) =
            this.WithCancellation DateTimeOffset.UtcNow reason

        member this.Build() =
            {
                Id = id
                Request = request
                Status = status
                ConfirmationNumber = confirmationNumber
                Vehicle = vehicle
                Customer = customer
                Pricing = pricing
                Inspection = inspection
                Payments = payments
                CreatedAt = createdAt
                UpdatedAt = updatedAt
                CompletedAt = completedAt
                CancelledAt = cancelledAt
                CancellationReason = cancellationReason
            }

    // ========== CONVENIENCE BUILDERS ==========

    /// Create a standard vehicle for testing
    let createStandardVehicle() =
        VehicleBuilder()
            .WithMakeAndModel("Toyota", "Camry")
            .WithYear(2023)
            .WithCategory(Midsize)
            .WithTransmission(Automatic)
            .WithFuelType(Gasoline)
            .WithCapacity(5, 4, 3)
            .WithRates(45.0m, 280.0m, 950.0m)
            .WithRentalRequirements(21, 200.0m)
            .AsAvailable()
            .WithLocation("NYC-Downtown")
            .Build()

    /// Create a standard customer for testing
    let createStandardCustomer() =
        CustomerBuilder()
            .WithName("John", "Doe")
            .WithContact("john.doe@example.com", "+1-555-0123")
            .WithAge(30)
            .WithLicenseInfo("DL1234567", "USA", DateTimeOffset.UtcNow.AddYears(2), "Class C")
            .WithCleanDrivingRecord()
            .WithCreditCard("4111111111111111", "12/25", "123", "John Doe")
            .WithLoyaltyTier(1)
            .WithPreferredCategories([Midsize; SUV])
            .Build()

    /// Create a standard rental request for testing
    let createStandardRentalRequest() =
        RentalRequestBuilder()
            .WithCustomer("cust-001")
            .WithVehicle("veh-001")
            .WithLocations("NYC-Downtown", "NYC-Downtown")
            .WithDateTimeRange(DateTimeOffset.UtcNow.AddDays(1), DateTimeOffset.UtcNow.AddDays(3))
            .AsStandardPackage()
            .WithBasicInsurance()
            .Build()

    /// Create a standard location for testing
    let createStandardLocation() =
        LocationBuilder()
            .WithId("loc-001")
            .WithName("NYC Downtown")
            .WithAddress("123 Main St", "New York", "NY", "10001", "USA")
            .WithContact("+1-555-0123", "nyc@rental.com")
            .WithDailyHours(TimeSpan(9, 0, 0), TimeSpan(17, 0, 0))
            .WithCapacity(50)
            .AsDowntownLocation()
            .WithCoordinates(40.7128m, -74.0060m)
            .Build()

    /// Create a complete rental scenario
    let createCompleteRentalScenario() =
        let vehicle = createStandardVehicle()
        let customer = createStandardCustomer()
        let request = createStandardRentalRequest()
        let pricing = Pricing.calculateTotalRentalCost vehicle request in
        
        RentalBuilder()
            .WithId("rental-001")
            .WithConfirmationNumber("CONF123456")
            .WithVehicle(vehicle)
            .WithCustomer(customer)
            .WithRequest(request)
            .WithPricing(pricing)
            .AsConfirmed()
            .WithSuccessfulPayment(pricing.TotalAmount, CreditCard("4111111111111111", "12/25", "123", "John Doe"))
            .Build()

    /// Create a scenario with multiple vehicles
    let createMultiVehicleScenario() =
        let vehicles = [
            VehicleBuilder().WithId("veh-001").WithMakeAndModel("Toyota", "Camry").AsAvailable().Build()
            VehicleBuilder().WithId("veh-002").WithMakeAndModel("Honda", "Civic").AsAvailable().Build()
            VehicleBuilder().WithId("veh-003").WithMakeAndModel("Ford", "Explorer").AsRented().Build()
        ]
        
        let customer = createStandardCustomer()
        let request = createStandardRentalRequest()
        
        {
            AvailableVehicles = vehicles |> List.filter (fun v -> v.Status = Available)
            Customer = customer
            Request = request
            Locations = [createStandardLocation()]
        }

    /// Create a scenario with different rental durations
    let createDurationScenarios() =
        let vehicle = createStandardVehicle()
        let customer = createStandardCustomer()
        
        [
            ("Daily", RentalRequestBuilder().WithDateTimeRange(DateTimeOffset.UtcNow.AddDays(1), DateTimeOffset.UtcNow.AddDays(2)).Build()),
            ("Weekly", RentalRequestBuilder().WithDateTimeRange(DateTimeOffset.UtcNow.AddDays(1), DateTimeOffset.UtcNow.AddDays(8)).Build()),
            ("Monthly", RentalRequestBuilder().WithDateTimeRange(DateTimeOffset.UtcNow.AddDays(1), DateTimeOffset.UtcNow.AddDays(31)).Build())
        ] |> List.map (fun (duration, request) ->
            let pricing = Pricing.calculateTotalRentalCost vehicle request
            {
                Duration = duration
                Vehicle = vehicle
                Customer = customer
                Request = request
                Pricing = pricing
            }
        )

    /// Create a scenario with different insurance options
    let createInsuranceScenarios() =
        let vehicle = createStandardVehicle()
        let customer = createStandardCustomer()
        let baseRequest = createStandardRentalRequest()
        
        [
            ("Basic", { baseRequest with Insurance = BasicInsurance }),
            ("Premium", { baseRequest with Insurance = PremiumInsurance }),
            ("None", { baseRequest with Insurance = NoInsurance }),
            ("ThirdParty", { baseRequest with Insurance = ThirdPartyInsurance })
        ] |> List.map (fun (insuranceType, request) ->
            let pricing = Pricing.calculateTotalRentalCost vehicle request
            {
                InsuranceType = insuranceType
                Vehicle = vehicle
                Customer = customer
                Request = request
                Pricing = pricing
            }
        )

    /// Create a scenario with different add-on combinations
    let createAddOnScenarios() =
        let vehicle = createStandardVehicle()
        let customer = createStandardCustomer()
        let baseRequest = createStandardRentalRequest()
        
        [
            ("None", []),
            ("GPS", [GPSNavigation]),
            ("ChildSeat", [ChildSeat]),
            ("GPS + ChildSeat", [GPSNavigation; ChildSeat]),
            ("All", [GPSNavigation; ChildSeat; SkiRack; BikeRack; AdditionalDriver; FuelService])
        ] |> List.map (fun (addOnType, addOns) ->
            let request = { baseRequest with AddOns = addOns }
            let pricing = Pricing.calculateTotalRentalCost vehicle request
            {
                AddOnType = addOnType
                Vehicle = vehicle
                Customer = customer
                Request = request
                Pricing = pricing
            }
        )