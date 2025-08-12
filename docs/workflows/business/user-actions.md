# User Actions and Business Logic Documentation

This document defines all business logic and user flows for the Reservo platform using Reactor workflows.

## System Overview

Reservo is a universal reservation system. The core functionality enables:

* **Businesses** to manage land plots, including visual layouts, item positioning, and pricing
* **Customers** to search for available items and make full-day or multi-day reservations through a public interface
* **Payments** to be handled exclusively with cash, with the system responsible for tracking payment amounts and statuses
* **Real-time updates** to flow seamlessly from the business management UI to the public booking interface, ensuring data consistency
* **Online reservations** made by customers are provisional until a cash payment is received and recorded by a business employee, finalizing the booking

## Core Business Entities

### Land Plot Management

* **Plot**: A physical land area owned or managed by a business
* **Layout**: A visual representation of a plot, mapping the precise positions of all reservable items on a fixed grid
* **Section**: A logical division within a plot used to organize large or complex layouts
* **Item**: The individual reservable unit (e.g., stall, space, spot) positioned on the plot layout
* **Item Type**: A category for items (e.g., "Premium Stall," "Standard Spot") with a configurable base price per day

### Platform

* **Reservation**: A booking for a full-day or a series of consecutive full days. Supports:
  * **Provisional** status for online bookings awaiting payment
  * **Confirmed** status post-payment
* **Multi-day Reservation**: A reservation spanning consecutive full days
* **Pricing**: Constant base price per item type with business exceptions; no weekday/weekend differentials
* **Availability**: The real-time booking status for every item, updated instantly across the system

### Payment Tracking

* **Payment Record**: A record of a cash amount received for a reservation
* **Payment Status**: Internal tracking of whether a reservation has been paid for; no digital processing

## User Types and Flows

### 1. Business Owner Flow

**Primary Actions:**

* Create and manage the business account
* Set up land plots and design their visual layouts
* Position, categorize, and price reservable items
* Define base pricing rules
* Update item availability and view business analytics

**Reactor Flow: BusinessSetupFlow**

```yaml
BusinessSetupFlow:
  Input: business_info, plot_details
  Steps:
    1. create_business
    2. create_plot
    3. create_layout
    4. define_sections
    5. setup_item_types
    6. set_initial_pricing
  Compensations:
    - rollback create_business
    - rollback create_plot
    - rollback create_layout
    - rollback define_sections
    - rollback setup_item_types
    - rollback set_initial_pricing
```

**Reactor Flow: ItemManagementFlow**

```yaml
ItemManagementFlow:
  Input: business_id, item_details, grid_position
  Steps:
    1. validate_business_ownership
    2. check_grid_position_within_bounds
    3. create_item
    4. assign_item_type
    5. position_on_layout
    6. set_availability
    7. broadcast_to_public_ui
  Compensations:
    - remove_item
    - restore_previous_state
```

### 2. Business Employee Flow

**Primary Actions:**

* Manage reservations for walk-in customers
* Confirm provisional online bookings upon receiving cash payment
* Update item availability manually if needed
* Record cash payments accurately
* View business analytics relevant to their role
* Adjust reservation price for special cases

**Reactor Flow: EmployeeReservationFlow**

```yaml
EmployeeReservationFlow:
  Input: employee_id, client_info, item_id, dates
  Steps:
    1. verify_employee_permissions
    2. find_or_create_client
    3. check_availability_for_dates(ensure_consecutive: true)
    4. validate_pricing_rules
    5. calculate_total_price
    6. update_price_if_needed_for_client
    7. create_reservation(status: 'confirmed')
    8. verify_cash_amount(amount_received)
    9. record_payment_amount
   10. update_availability
   11. generate_receipt
  Compensations:
    - cancel_reservation
    - restore_availability
    - remove_payment_record
```

**Reactor Flow: EmployeePaymentConfirmationFlow**

```yaml
EmployeePaymentConfirmationFlow:
  Input: employee_id, reservation_id, cash_amount
  Steps:
    1. verify_employee_permissions
    2. load_provisional_reservation
    3. verify_cash_amount
    4. record_payment_amount
    5. update_reservation_status(status: 'confirmed')
    6. update_availability
    7. generate_receipt
  Compensations:
    - revert_to_provisional
    - remove_payment_record
```

### 3. Customer (Public User) Flow

**Primary Actions:**

* Browse available items by location and date
* Receive system-generated suggestions
* Make provisional reservations for items
* View personal booking history

**Reactor Flow: CustomerBookingFlow**

```yaml
CustomerBookingFlow:
  Input: customer_info, search_criteria, item_id, dates
  Steps:
    1. find_or_create_customer
    2. search_available_items
    3. hold_item_for_15_min
    4. create_reservation(status: 'provisional')
    5. send_confirmation(with_payment_instructions: true)
  Compensations:
    - release_held_item
    - cancel_provisional_reservation
```

**Reactor Flow: AvailabilitySearchFlow**

```yaml
AvailabilitySearchFlow:
  Input: location, dates, preferences
  Steps:
    1. query_plots_in_area
    2. check_item_availability
    3. apply_filters
    4. calculate_prices
    5. rank_by_preferences
    6. return_suggestions
```

## Core Business Rules

### Reservation Rules
* **Full-Day Only**: Reservations are for entire calendar days; no hourly bookings
* **Consecutive Days**: Multi-day reservations must be consecutive
* **Provisional Holds**: Online bookings are provisional until paid; holds expire after 15 minutes
* **Immediate Updates**: Availability changes propagate instantly to all interfaces
* **Fixed Grid Layout**: Items placed on a pre-defined grid—no overlap logic needed beyond boundary checks

### Pricing Rules
* Base price per item type
* Multi-day discounts configurable
* Prices can vary by season/date
* Real-time price updates
* Employee override capability for special cases

### Layout Rules
* Items must be positioned within plot boundaries on the fixed grid
* No overlapping items on the same grid position
* Visual position corresponds to physical location
* Sections help organize large plots
* Grid size is configurable per plot

## System Reactor Flows

### Real-time Synchronization Flow

```yaml
AvailabilitySyncFlow:
  Trigger: business updates item/price/availability
  Steps:
    1. validate_update
    2. resolve_version_conflicts
    3. update_database
    4. broadcast_to_public_cache
    5. update_search_indices
    6. notify_affected_searches
```

### Suggestion Algorithm Flow

```yaml
ItemSuggestionFlow:
  Input: user_preferences, search_params
  Steps:
    1. analyze_past_bookings
    2. check_current_availability
    3. apply_proximity_scoring
    4. rank_by_user_preferences
    5. return_top_suggestions
```

### Daily Operations Flow

```yaml
DailyOperationsFlow:
  Schedule: daily at midnight
  Steps:
    1. archive_completed_reservations
    2. update_availability_calendar
    3. send_upcoming_reminders
    4. generate_daily_reports
    5. cleanup_abandoned_bookings(expired provisional holds)
    6. release_expired_item_holds
```

### Item Hold Management Flow

```yaml
ItemHoldFlow:
  Input: item_id, customer_id, duration_minutes
  Steps:
    1. check_item_availability
    2. create_temporary_hold
    3. set_expiration_timer(15_minutes)
    4. notify_other_searches
  On_Expiration:
    1. release_hold
    2. restore_availability
    3. notify_search_updates
```

## Permission Matrix

### Business Owner Permissions

* can_manage_business_settings
* can_manage_plots_and_layouts
* can_manage_all_items
* can_set_pricing
* can_view_all_reports
* can_manage_employees

### Business Employee Permissions

* can_create_reservations
* can_confirm_provisional_reservations
* can_view_items
* can_update_availability
* can_record_payments
* can_adjust_prices
* can_view_own_reservations

### Customer Permissions

* can_search_items
* can_make_provisional_reservations
* can_view_own_bookings
* can_update_own_profile

## Data Flow Architecture

**Business to Public Flow**

```
Business UI → Reactor Flow → Database → Event Broadcast → Public UI Cache → Search Results
```

**Reservation Creation Flow**

```
Search → Hold Item (15 min) → Create Provisional Booking → [Offline: Customer Pays Cash] → Employee Records Payment → Confirm Booking → Update Availability
```

**Payment Confirmation Flow**

```
Provisional Reservation → Employee Verification → Cash Receipt → Payment Record → Status Update → Confirmed Reservation
```

## Error Handling and Edge Cases

### Concurrent Booking Handling
* Item holds prevent double-booking during the 15-minute window
* Version control on availability updates prevents race conditions
* Immediate broadcast of holds to all active searches

### Payment Edge Cases
* Partial payments: System tracks amount paid vs. total due
* Overpayments: Recorded with change amount noted
* No-shows: Provisional bookings auto-expire after configurable period

### System Resilience
* All Reactor flows include compensating actions
* Database transactions ensure data consistency
* Event sourcing for audit trail and recovery