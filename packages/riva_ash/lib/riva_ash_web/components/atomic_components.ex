defmodule RivaAshWeb.Components.AtomicComponents do
  @moduledoc """
  Central module that exports all atomic design components.

  This module provides a single import point for all atomic design components,
  organized by their atomic design level (atoms, molecules, organisms, templates).

  ## Usage

  To use all atomic components in a module:

      use RivaAshWeb.Components.AtomicComponents

  To import specific levels:

      import RivaAshWeb.Components.AtomicComponents, only: [atoms: 0]
      import RivaAshWeb.Components.AtomicComponents, only: [molecules: 0]
  """

  defmacro __using__(_opts) do
    quote do
      import RivaAshWeb.Components.AtomicComponents
      atoms()
      molecules()
      organisms()
      templates()
      forms()
      interactive()
      navigation()
    end
  end

  @doc """
  Imports all atom-level components.

  Atoms are the basic building blocks of the design system:
  - Icon: For displaying icons
  - Button: For user actions
  - Text: For typography
  - Badge: For labels and status indicators
  - Input: For form inputs
  - Select: For dropdowns
  - Checkbox: For boolean inputs
  - Radio: For single selection
  - DatePicker: For date selection
  - TimePicker: For time selection
  - Toggle: For switches
  - Avatar: For user/business avatars
  - Spinner: For loading states
  - Tooltip: For help text
  """
  defmacro atoms do
    quote do
      # Use modern UI components where available
      import RivaAshWeb.Components.UI

      # Legacy atomic components for specialized use cases
      import RivaAshWeb.Components.Atoms.Avatar
      import RivaAshWeb.Components.Atoms.DatePicker
      import RivaAshWeb.Components.Atoms.Icon
      import RivaAshWeb.Components.Atoms.Radio
      import RivaAshWeb.Components.Atoms.Spinner
      import RivaAshWeb.Components.Atoms.Text
      import RivaAshWeb.Components.Atoms.TimePicker
      import RivaAshWeb.Components.Atoms.Toggle
      import RivaAshWeb.Components.Atoms.Tooltip
    end
  end

  @doc """
  Imports all molecule-level components.

  Molecules are simple components made from atoms:
  - Card: For grouping content
  - FormField: For form inputs with labels and errors
  - EmptyState: For displaying when no data is available
  - SearchBar: For search functionality
  - Pagination: For table navigation
  - FilterPanel: For advanced filtering
  - StatusIndicator: For status display
  - ActionMenu: For dropdown actions
  - ConfirmDialog: For confirmations
  - NotificationToast: For notifications
  - BreadcrumbNav: For navigation
  - TabNavigation: For tab interfaces
  - ProgressBar: For progress indication
  """
  defmacro molecules do
    quote do
      import RivaAshWeb.Components.Molecules.ActionMenu
      import RivaAshWeb.Components.Molecules.BreadcrumbNav
      import RivaAshWeb.Components.Molecules.Card
      import RivaAshWeb.Components.Molecules.ConfirmDialog
      import RivaAshWeb.Components.Molecules.EmptyState
      import RivaAshWeb.Components.Molecules.FilterPanel
      import RivaAshWeb.Components.Molecules.FormField
      import RivaAshWeb.Components.Molecules.NotificationToast
      import RivaAshWeb.Components.Molecules.Pagination
      import RivaAshWeb.Components.Molecules.ProgressBar
      import RivaAshWeb.Components.Molecules.SearchBar
      import RivaAshWeb.Components.Molecules.StatusIndicator
      import RivaAshWeb.Components.Molecules.TabNavigation
    end
  end

  @doc """
  Imports all organism-level components.

  Organisms are complex components made from atoms and molecules:
  - PageHeader: For consistent page headers
  - BusinessForm: For creating/editing businesses
  - BusinessCard: For displaying business information
  - EmployeeForm: For creating/editing employees
  - DataTable: For data display with Flop
  - CalendarView: For calendar interfaces
  - TimelineView: For chronological display
  - LayoutDesigner: For visual layout editing
  - ReservationForm: For booking interface
  - ClientForm: For client management
  - ItemForm: For item management
  - PricingForm: For pricing configuration
  - PermissionMatrix: For permission management
  - DashboardStats: For statistics display
  """
  defmacro organisms do
    quote do
      import RivaAshWeb.Components.Organisms.BusinessCard
      import RivaAshWeb.Components.Organisms.BusinessForm
      import RivaAshWeb.Components.Organisms.CalendarView
      import RivaAshWeb.Components.Organisms.ClientForm
      import RivaAshWeb.Components.Organisms.DashboardStats
      import RivaAshWeb.Components.Organisms.DataTable
      import RivaAshWeb.Components.Organisms.EmployeeForm
      import RivaAshWeb.Components.Organisms.ItemForm
      import RivaAshWeb.Components.Organisms.LayoutDesigner
      import RivaAshWeb.Components.Organisms.PageHeader
      import RivaAshWeb.Components.Organisms.PermissionMatrix
      import RivaAshWeb.Components.Organisms.PricingForm
      import RivaAshWeb.Components.Organisms.ReservationForm
      import RivaAshWeb.Components.Organisms.TimelineView
    end
  end

  @doc """
  Imports template-level components.

  Templates are page-level layout components:
  - DashboardTemplate: For dashboard pages
  - ListViewTemplate: For resource list pages
  - DetailViewTemplate: For resource detail pages
  - FormViewTemplate: For form pages
  - CalendarTemplate: For calendar pages
  """
  defmacro templates do
    quote do
      import RivaAshWeb.Components.Templates.CalendarTemplate
      import RivaAshWeb.Components.Templates.DashboardTemplate
      import RivaAshWeb.Components.Templates.DetailViewTemplate
      import RivaAshWeb.Components.Templates.FormViewTemplate
      import RivaAshWeb.Components.Templates.ListViewTemplate
    end
  end

  @doc """
  Imports form components.

  Specialized form components for different resources:
  - PlotForm: For plot management
  - SectionForm: For section management
  - ItemTypeForm: For item type configuration
  - LayoutForm: For layout configuration
  - ReservationBookingForm: For multi-step booking
  - PaymentForm: For payment processing
  - ScheduleForm: For schedule configuration
  """
  defmacro forms do
    quote do
      import RivaAshWeb.Components.Forms.ItemTypeForm
      import RivaAshWeb.Components.Forms.LayoutForm
      import RivaAshWeb.Components.Forms.PaymentForm
      import RivaAshWeb.Components.Forms.PlotForm
      import RivaAshWeb.Components.Forms.ReservationBookingForm
      import RivaAshWeb.Components.Forms.ScheduleForm
      import RivaAshWeb.Components.Forms.SectionForm
    end
  end

  @doc """
  Imports interactive components.

  Complex interactive components:
  - MonthlyCalendar: For month view calendar
  - WeeklyCalendar: For week view calendar
  - DailySchedule: For day view schedule
  - TimeSlotPicker: For time slot selection
  - RecurrencePattern: For recurring reservations
  - AvailabilityGrid: For availability editing
  - PlotLayoutDesigner: For visual layout design
  - GridPositionPicker: For position selection
  """
  defmacro interactive do
    quote do
      import RivaAshWeb.Components.Interactive.AvailabilityGrid
      import RivaAshWeb.Components.Interactive.DailySchedule
      import RivaAshWeb.Components.Interactive.GridPositionPicker
      import RivaAshWeb.Components.Interactive.MonthlyCalendar
      import RivaAshWeb.Components.Interactive.PlotLayoutDesigner
      import RivaAshWeb.Components.Interactive.RecurrencePattern
      import RivaAshWeb.Components.Interactive.TimeSlotPicker
      import RivaAshWeb.Components.Interactive.WeeklyCalendar
    end
  end

  @doc """
  Imports navigation components.

  Navigation and layout components:
  - ExpandedSidebar: For main navigation
  - BreadcrumbTrail: For breadcrumb navigation
  - QuickActions: For quick action shortcuts
  - SearchGlobal: For global search
  - NotificationCenter: For notifications
  """
  defmacro navigation do
    quote do
      import RivaAshWeb.Components.Navigation.BreadcrumbTrail
      import RivaAshWeb.Components.Navigation.ExpandedSidebar
      import RivaAshWeb.Components.Navigation.NotificationCenter
      import RivaAshWeb.Components.Navigation.QuickActions
      import RivaAshWeb.Components.Navigation.SearchGlobal
    end
  end
end
