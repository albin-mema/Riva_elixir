# Reservation Center: Unified Booking Interface Design

## Overview
The Reservation Center replaces 4 separate pages (Reservation, RecurringReservation, BookingCalendar, AvailabilityException) with a single, powerful booking management interface.

## Current Problems
- **Context Switching**: Users jump between calendar, reservations list, and availability management
- **Fragmented View**: Can't see all booking information in one place
- **Inefficient Workflow**: Creating a reservation requires multiple page visits
- **Poor Discoverability**: Related features are scattered across different pages

## Proposed Unified Interface

### Main Layout Structure
```
┌─────────────────────────────────────────────────────────────────┐
│ 📅 Reservation Center                    [Today] [Week] [Month] │
├─────────────────────────────────────────────────────────────────┤
│ Quick Actions: [+ New Booking] [Recurring Pattern] [Bulk Edit]  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Calendar View (Primary Interface)                              │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ Time slots with drag-and-drop booking                   │   │
│  │ Color-coded by status, item type, client               │   │
│  │ Hover for quick details, click for full edit           │   │
│  │ Visual availability indicators                          │   │
│  │ Exception overlays (holidays, maintenance, etc.)       │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│ Side Panel (Context-Sensitive)                                 │
│ • Booking Details (when reservation selected)                  │
│ • Quick Booking Form (when time slot selected)                 │
│ • Availability Rules (when managing exceptions)                │
│ • Recurring Patterns (when setting up recurring bookings)     │
└─────────────────────────────────────────────────────────────────┘
```

### Key Features Integration

#### 1. Unified Calendar View
**Combines**: BookingCalendarLive + ReservationLive data
- **Visual Timeline**: All reservations displayed on calendar grid
- **Multi-View Support**: Day/Week/Month views with appropriate detail levels
- **Real-time Updates**: Live updates as bookings change
- **Drag & Drop**: Move reservations by dragging on calendar
- **Color Coding**: Status-based colors (confirmed, pending, cancelled)

#### 2. Contextual Booking Management
**Combines**: ReservationLive + RecurringReservationLive functionality
- **Quick Booking**: Click empty slot → instant booking form
- **Detailed Booking**: Full form with client search, notes, special requirements
- **Recurring Patterns**: Set up weekly/monthly recurring reservations
- **Bulk Operations**: Select multiple slots for group bookings

#### 3. Integrated Availability Management
**Combines**: AvailabilityExceptionLive + ItemScheduleLive data
- **Exception Overlays**: Visual indicators for holidays, maintenance, special hours
- **Quick Exception Creation**: Right-click calendar → create exception
- **Availability Rules**: Set recurring availability patterns
- **Capacity Management**: Visual indicators for item capacity limits

#### 4. Smart Filtering and Search
**New unified functionality**:
- **Multi-criteria Filters**: By item, client, status, date range
- **Quick Search**: Find reservations by client name, booking ID, notes
- **Saved Views**: Custom filter combinations for different use cases
- **Smart Suggestions**: Auto-suggest available slots based on requirements

## User Workflows

### Workflow 1: Daily Booking Management
**Current Process** (4 pages):
1. Check BookingCalendarLive for today's schedule
2. Go to ReservationLive to see reservation details
3. Check AvailabilityExceptionLive for any special conditions
4. Create new reservation in ReservationLive

**New Process** (1 interface):
1. Open Reservation Center → see everything at once
2. Click empty slot → quick booking form appears
3. All context (availability, exceptions, existing bookings) visible

### Workflow 2: Setting Up Recurring Reservations
**Current Process** (2 pages):
1. Create pattern in RecurringReservationLive
2. Check BookingCalendarLive to verify instances

**New Process** (1 interface):
1. Select time slot → choose "Make Recurring"
2. Set pattern → preview shows on calendar immediately
3. Confirm → all instances appear on calendar

### Workflow 3: Managing Availability Exceptions
**Current Process** (3 pages):
1. Create exception in AvailabilityExceptionLive
2. Check impact in BookingCalendarLive
3. Modify affected reservations in ReservationLive

**New Process** (1 interface):
1. Right-click calendar date → "Create Exception"
2. Set exception details → affected bookings highlighted
3. Handle conflicts directly in same interface

## Technical Implementation

### Component Architecture
```elixir
defmodule RivaAshWeb.ReservationCenterLive do
  # Main LiveView orchestrating all booking functionality
  
  # Child components:
  # - CalendarViewComponent (unified calendar display)
  # - BookingFormComponent (quick and detailed booking forms)
  # - AvailabilityManagerComponent (exception and rule management)
  # - RecurringPatternComponent (recurring reservation setup)
  # - FilterPanelComponent (search and filtering)
end
```

### Data Integration Strategy
```elixir
# Single mount function loads all related data
def mount(_params, session, socket) do
  # Load reservations, recurring patterns, availability exceptions
  # Set up real-time subscriptions for live updates
  # Initialize calendar view state
end

# Unified event handling for all booking operations
def handle_event("slot_clicked", %{"date" => date, "time" => time}, socket)
def handle_event("reservation_dragged", %{"id" => id, "new_time" => time}, socket)
def handle_event("create_exception", %{"date" => date, "type" => type}, socket)
```

### State Management
- **Unified Socket State**: All booking-related data in single socket
- **Real-time Subscriptions**: Live updates for calendar changes
- **Optimistic Updates**: Immediate UI feedback with server confirmation
- **Conflict Resolution**: Handle booking conflicts gracefully

## Benefits

### User Experience
- **80% fewer clicks** for common booking operations
- **Immediate context** - see all related information at once
- **Faster decision making** - visual calendar shows availability instantly
- **Reduced errors** - conflicts and exceptions visible immediately

### Business Impact
- **Increased booking efficiency** - staff can handle more reservations
- **Better resource utilization** - easier to spot and fill gaps
- **Improved customer service** - faster response to booking requests
- **Reduced training time** - single interface to learn

### Technical Benefits
- **Simplified codebase** - fewer LiveView modules to maintain
- **Better performance** - single data load instead of multiple page loads
- **Easier testing** - unified workflows easier to test end-to-end
- **Enhanced maintainability** - related functionality in one place

## Migration Strategy

### Phase 1: Build New Interface
1. Create ReservationCenterLive with basic calendar view
2. Implement unified booking form
3. Add real-time updates and drag-and-drop

### Phase 2: Feature Integration
1. Integrate recurring reservation functionality
2. Add availability exception management
3. Implement advanced filtering and search

### Phase 3: Migration and Cleanup
1. Update navigation to point to new interface
2. Migrate existing bookings and patterns
3. Remove old separate pages
4. Update documentation and training materials

This unified approach transforms booking management from a fragmented, multi-page experience into a cohesive, efficient workflow that matches how users actually think about managing reservations.
