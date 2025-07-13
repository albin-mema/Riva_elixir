# Riva Ash UI Scaffolding TODO

This document outlines all the UI screens, pages, and components that need to be created for the Riva Ash reservation management system to be complete.

## Current Status

### ✅ Completed Components
- **Authentication**: Sign-in, Register, Sign-out flows
- **Layout System**: Root layout, authenticated layout with navigation
- **Atomic Design System**: Atoms (Button, Icon, Text, Badge), Molecules (Card, FormField, EmptyState), Organisms (PageHeader, BusinessForm, BusinessCard, EmployeeForm)
- **Business Management**: Business CRUD with cards and forms
- **Employee Management**: Employee CRUD with Flop table integration
- **Core Components**: Flash messages, React component integration

### 🔄 Partially Implemented
- **Navigation**: Basic sidebar navigation (needs expansion for all resources)
- **Table Components**: Flop integration exists for employees (needs extension to other resources)

## 📋 UI Components To Create

### 1. Atomic Design Components (Missing)

#### Atoms
- [ ] **Input Component** - Standardized form inputs with validation states
- [ ] **Select Component** - Dropdown selects with search functionality
- [ ] **Checkbox Component** - Checkboxes with labels and validation
- [ ] **Radio Component** - Radio buttons with groups
- [ ] **DatePicker Component** - Date selection with calendar popup
- [ ] **TimePicker Component** - Time selection component
- [ ] **Toggle Component** - Switch/toggle buttons
- [ ] **Avatar Component** - User/business avatars with fallbacks
- [ ] **Spinner Component** - Loading indicators
- [ ] **Tooltip Component** - Hover tooltips for help text

#### Molecules
- [ ] **SearchBar Component** - Search input with filters
- [ ] **Pagination Component** - Table pagination controls
- [ ] **FilterPanel Component** - Advanced filtering interface
- [ ] **StatusIndicator Component** - Status badges with colors
- [ ] **ActionMenu Component** - Dropdown action menus
- [ ] **ConfirmDialog Component** - Confirmation modals
- [ ] **NotificationToast Component** - Toast notifications
- [ ] **BreadcrumbNav Component** - Breadcrumb navigation
- [ ] **TabNavigation Component** - Tab interface component
- [ ] **ProgressBar Component** - Progress indicators

#### Organisms
- [ ] **DataTable Component** - Reusable table with Flop integration
- [ ] **CalendarView Component** - Monthly/weekly calendar display
- [ ] **TimelineView Component** - Timeline for reservations/events
- [ ] **LayoutDesigner Component** - Grid-based layout designer for plots
- [ ] **ReservationForm Component** - Complex reservation creation form
- [ ] **ClientForm Component** - Client registration/edit form
- [ ] **ItemForm Component** - Item creation/edit form
- [ ] **PricingForm Component** - Pricing configuration form
- [ ] **PermissionMatrix Component** - Employee permission management
- [ ] **DashboardStats Component** - Statistics dashboard widgets

### 2. Page-Level Components (LiveViews)

#### Core Management Pages
- [ ] **DashboardLive** - Main dashboard with statistics and quick actions
- [ ] **PlotLive** - Plot management (CRUD, list, detail views)
- [ ] **SectionLive** - Section management within plots
- [ ] **ItemTypeLive** - Item type management
- [ ] **ItemLive** - Item management with positioning
- [ ] **LayoutLive** - Layout designer and management
- [ ] **ClientLive** - Client management and registration
- [ ] **PermissionLive** - Permission and role management

#### Reservation & Booking Pages
- [ ] **ReservationLive** - Reservation management dashboard
- [ ] **BookingCalendarLive** - Calendar view for reservations
- [ ] **AvailabilityLive** - Availability management and exceptions
- [ ] **RecurringReservationLive** - Recurring reservation patterns
- [ ] **ItemScheduleLive** - Item scheduling and availability

#### Financial Management Pages
- [ ] **PricingLive** - Pricing management and configuration
- [ ] **PaymentLive** - Payment tracking and management
- [ ] **ReportsLive** - Financial and operational reports

#### System Administration Pages
- [ ] **UserManagementLive** - User account management (admin only)
- [ ] **SystemSettingsLive** - System-wide configuration
- [ ] **AuditLogLive** - System audit trail viewer

### 3. Complex Interactive Components

#### Calendar & Scheduling
- [ ] **MonthlyCalendar Component** - Full month view with reservations
- [ ] **WeeklyCalendar Component** - Week view with time slots
- [ ] **DailySchedule Component** - Day view with hourly slots
- [ ] **TimeSlotPicker Component** - Interactive time slot selection
- [ ] **RecurrencePattern Component** - Recurring reservation setup
- [ ] **AvailabilityGrid Component** - Weekly availability grid editor

#### Layout & Positioning
- [ ] **PlotLayoutDesigner Component** - Visual plot layout editor
- [ ] **GridPositionPicker Component** - Row/column position selector
- [ ] **ItemPositionMap Component** - Visual item positioning
- [ ] **SectionVisualizer Component** - Section layout display

#### Data Visualization
- [ ] **ReservationTimeline Component** - Timeline view of reservations
- [ ] **OccupancyChart Component** - Occupancy rate visualization
- [ ] **RevenueChart Component** - Revenue tracking charts
- [ ] **UtilizationHeatmap Component** - Item utilization heatmap

### 4. Form Components (Specific)

#### Resource Forms
- [ ] **PlotForm Component** - Plot creation/editing
- [ ] **SectionForm Component** - Section creation/editing
- [ ] **ItemTypeForm Component** - Item type configuration
- [ ] **ItemForm Component** - Item creation with positioning
- [ ] **LayoutForm Component** - Layout configuration form
- [ ] **ClientRegistrationForm Component** - Client registration
- [ ] **ReservationBookingForm Component** - Reservation creation
- [ ] **PricingConfigForm Component** - Pricing setup
- [ ] **PaymentForm Component** - Payment processing
- [ ] **ScheduleForm Component** - Item schedule configuration

#### Advanced Forms
- [ ] **BulkActionForm Component** - Bulk operations interface
- [ ] **ImportDataForm Component** - Data import interface
- [ ] **ExportConfigForm Component** - Export configuration
- [ ] **NotificationSettingsForm Component** - Notification preferences

### 5. Navigation & Layout Enhancements

#### Navigation Components
- [ ] **ExpandedSidebar Component** - Full navigation with all resources
- [ ] **BreadcrumbTrail Component** - Dynamic breadcrumb navigation
- [ ] **QuickActions Component** - Quick action shortcuts
- [ ] **SearchGlobal Component** - Global search across resources
- [ ] **NotificationCenter Component** - Notification management

#### Layout Templates
- [ ] **DashboardTemplate** - Dashboard page template
- [ ] **ListViewTemplate** - Resource list page template
- [ ] **DetailViewTemplate** - Resource detail page template
- [ ] **FormViewTemplate** - Form page template
- [ ] **CalendarTemplate** - Calendar page template

### 6. Mobile & Responsive Components

#### Mobile-Specific
- [ ] **MobileNavigation Component** - Mobile-optimized navigation
- [ ] **MobileCalendar Component** - Touch-friendly calendar
- [ ] **MobileReservationCard Component** - Mobile reservation display
- [ ] **SwipeActions Component** - Swipe gesture actions
- [ ] **MobileSearch Component** - Mobile search interface

### 7. Integration Components

#### External Integrations
- [ ] **PaymentGateway Component** - Payment processing integration
- [ ] **EmailTemplate Component** - Email notification templates
- [ ] **SMSNotification Component** - SMS notification interface
- [ ] **CalendarSync Component** - External calendar synchronization
- [ ] **ReportExport Component** - Report generation and export

### 8. Error & Loading States

#### State Management Components
- [ ] **LoadingStates Component** - Various loading indicators
- [ ] **ErrorBoundary Component** - Error handling and display
- [ ] **EmptyStates Component** - Empty state variations
- [ ] **OfflineIndicator Component** - Offline status indicator
- [ ] **ConnectionStatus Component** - Real-time connection status

## 🎯 Priority Implementation Order

### Phase 1: Core Management (High Priority)
1. DashboardLive - Central hub
2. PlotLive - Core spatial organization
3. SectionLive - Plot subdivisions
4. ItemLive - Reservable resources
5. DataTable Component - Reusable table foundation

### Phase 2: Reservation System (High Priority)
1. ReservationLive - Reservation management
2. BookingCalendarLive - Calendar interface
3. CalendarView Component - Calendar display
4. ReservationForm Component - Booking interface
5. TimeSlotPicker Component - Time selection

### Phase 3: Client & Financial (Medium Priority)
1. ClientLive - Client management
2. PricingLive - Pricing configuration
3. PaymentLive - Payment tracking
4. ClientForm Component - Client registration
5. PricingForm Component - Pricing setup

### Phase 4: Advanced Features (Medium Priority)
1. LayoutLive - Layout designer
2. RecurringReservationLive - Recurring patterns
3. LayoutDesigner Component - Visual layout editor
4. PermissionMatrix Component - Permission management
5. ReportsLive - Reporting interface

### Phase 5: Polish & Enhancement (Low Priority)
1. Mobile components
2. Advanced visualizations
3. Integration components
4. Performance optimizations
5. Accessibility improvements

## 📝 Implementation Notes

### Technical Considerations
- All components should follow the existing atomic design pattern
- Use Flop library for all table functionality and pagination
- Integrate with existing authentication and authorization system
- Maintain consistency with current Tailwind CSS styling
- Ensure LiveView compatibility and real-time updates
- Follow existing form validation patterns with AshPhoenix.Form

### Design System
- Extend existing atomic components before creating new ones
- Maintain consistent spacing, colors, and typography
- Ensure mobile responsiveness for all components
- Follow accessibility best practices (WCAG 2.1)
- Use existing icon library (Heroicons) consistently

### Performance
- Implement lazy loading for large data sets
- Use Phoenix LiveView streams for real-time updates
- Optimize calendar and timeline components for large date ranges
- Implement proper caching strategies for frequently accessed data
- Consider React components for complex interactive features

This scaffolding provides a comprehensive roadmap for completing the Riva Ash UI. Each component should be implemented with proper testing, documentation, and integration with the existing Ash framework backend.
