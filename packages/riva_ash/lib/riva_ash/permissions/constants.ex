defmodule RivaAsh.Permissions.Constants do
  @moduledoc """
  Centralized constants for all system permissions.
  
  This module defines all possible actions/permissions that users can have in the system.
  Use these constants instead of hardcoded strings to ensure consistency and prevent typos.
  
  The constants are designed to work seamlessly with Ash policies and the SAT solver
  for efficient authorization evaluation.
  """

  # Reservation permissions
  @can_create_reservations "can_create_reservations"
  @can_view_all_reservations "can_view_all_reservations"
  @can_modify_reservations "can_modify_reservations"
  @can_cancel_reservations "can_cancel_reservations"
  @can_view_own_reservations "can_view_own_reservations"

  # Employee management permissions
  @can_view_employees "can_view_employees"
  @can_create_employees "can_create_employees"
  @can_modify_employees "can_modify_employees"
  @can_give_permissions "can_give_permissions"
  @can_revoke_permissions "can_revoke_permissions"

  # Business management permissions
  @can_manage_business_settings "can_manage_business_settings"
  @can_manage_items "can_manage_items"
  @can_manage_schedules "can_manage_schedules"
  @can_update_pricing "can_update_pricing"
  @can_view_pricing "can_view_pricing"
  @can_manage_layouts "can_manage_layouts"
  @can_manage_sections "can_manage_sections"

  # Client management permissions
  @can_view_clients "can_view_clients"
  @can_create_clients "can_create_clients"
  @can_modify_clients "can_modify_clients"

  # Payment and financial permissions
  @can_process_payments "can_process_payments"
  @can_view_payments "can_view_payments"
  @can_refund_payments "can_refund_payments"

  # Reporting permissions
  @can_view_reports "can_view_reports"
  @can_export_data "can_export_data"
  @can_view_analytics "can_view_analytics"

  # System permissions
  @can_access_admin_panel "can_access_admin_panel"
  @can_manage_system_settings "can_manage_system_settings"
  @can_view_audit_logs "can_view_audit_logs"

  # Public API to access permission constants
  def can_create_reservations, do: @can_create_reservations
  def can_view_all_reservations, do: @can_view_all_reservations
  def can_modify_reservations, do: @can_modify_reservations
  def can_cancel_reservations, do: @can_cancel_reservations
  def can_view_own_reservations, do: @can_view_own_reservations
  
  def can_view_employees, do: @can_view_employees
  def can_create_employees, do: @can_create_employees
  def can_modify_employees, do: @can_modify_employees
  def can_give_permissions, do: @can_give_permissions
  def can_revoke_permissions, do: @can_revoke_permissions
  
  def can_manage_business_settings, do: @can_manage_business_settings
  def can_manage_items, do: @can_manage_items
  def can_manage_schedules, do: @can_manage_schedules
  def can_update_pricing, do: @can_update_pricing
  def can_view_pricing, do: @can_view_pricing
  def can_manage_layouts, do: @can_manage_layouts
  def can_manage_sections, do: @can_manage_sections

  def can_view_clients, do: @can_view_clients
  def can_create_clients, do: @can_create_clients
  def can_modify_clients, do: @can_modify_clients

  def can_process_payments, do: @can_process_payments
  def can_view_payments, do: @can_view_payments
  def can_refund_payments, do: @can_refund_payments
  
  def can_view_reports, do: @can_view_reports
  def can_export_data, do: @can_export_data
  def can_view_analytics, do: @can_view_analytics
  
  def can_access_admin_panel, do: @can_access_admin_panel
  def can_manage_system_settings, do: @can_manage_system_settings
  def can_view_audit_logs, do: @can_view_audit_logs

  @doc """
  Returns all available permissions as a list.
  Useful for seeding, validation, or UI generation.
  """
  def all_permissions do
    [
      @can_create_reservations,
      @can_view_all_reservations,
      @can_modify_reservations,
      @can_cancel_reservations,
      @can_view_own_reservations,
      @can_view_employees,
      @can_create_employees,
      @can_modify_employees,
      @can_give_permissions,
      @can_revoke_permissions,
      @can_manage_business_settings,
      @can_manage_items,
      @can_manage_schedules,
      @can_update_pricing,
      @can_view_pricing,
      @can_manage_layouts,
      @can_manage_sections,
      @can_view_clients,
      @can_create_clients,
      @can_modify_clients,
      @can_process_payments,
      @can_view_payments,
      @can_refund_payments,
      @can_view_reports,
      @can_export_data,
      @can_view_analytics,
      @can_access_admin_panel,
      @can_manage_system_settings,
      @can_view_audit_logs
    ]
  end

  @doc """
  Returns permissions grouped by category.
  This matches the categories used in the Permission resource.
  """
  def permissions_by_category do
    %{
      reservations: [
        @can_create_reservations,
        @can_view_all_reservations,
        @can_modify_reservations,
        @can_cancel_reservations,
        @can_view_own_reservations
      ],
      employees: [
        @can_view_employees,
        @can_create_employees,
        @can_modify_employees,
        @can_give_permissions,
        @can_revoke_permissions
      ],
      business: [
        @can_manage_business_settings,
        @can_manage_items,
        @can_manage_schedules,
        @can_update_pricing,
        @can_view_pricing,
        @can_manage_layouts,
        @can_manage_sections
      ],
      clients: [
        @can_view_clients,
        @can_create_clients,
        @can_modify_clients
      ],
      payments: [
        @can_process_payments,
        @can_view_payments,
        @can_refund_payments
      ],
      reports: [
        @can_view_reports,
        @can_export_data,
        @can_view_analytics
      ],
      system: [
        @can_access_admin_panel,
        @can_manage_system_settings,
        @can_view_audit_logs
      ]
    }
  end

  @doc """
  Returns the category for a given permission.
  """
  def category_for_permission(permission) do
    permissions_by_category()
    |> Enum.find_value(fn {category, permissions} ->
      if permission in permissions, do: category
    end)
  end

  @doc """
  Validates that a permission exists in the system.
  """
  def valid_permission?(permission) do
    permission in all_permissions()
  end

  @doc """
  Returns permission metadata including description and category.
  This can be used for UI generation and documentation.
  """
  def permission_metadata do
    %{
      @can_create_reservations => %{
        description: "Can create new reservations for clients",
        category: :reservations
      },
      @can_view_all_reservations => %{
        description: "Can view all reservations, not just own",
        category: :reservations
      },
      @can_modify_reservations => %{
        description: "Can modify existing reservations",
        category: :reservations
      },
      @can_cancel_reservations => %{
        description: "Can cancel reservations",
        category: :reservations
      },
      @can_view_own_reservations => %{
        description: "Can view own reservations only",
        category: :reservations
      },
      @can_view_employees => %{
        description: "Can view employee information",
        category: :employees
      },
      @can_create_employees => %{
        description: "Can create new employee accounts",
        category: :employees
      },
      @can_modify_employees => %{
        description: "Can modify employee information",
        category: :employees
      },
      @can_give_permissions => %{
        description: "Can grant permissions to other employees",
        category: :employees
      },
      @can_revoke_permissions => %{
        description: "Can revoke permissions from other employees",
        category: :employees
      },
      @can_manage_business_settings => %{
        description: "Can modify business settings and configuration",
        category: :business
      },
      @can_manage_items => %{
        description: "Can create, modify, and delete items",
        category: :business
      },
      @can_manage_schedules => %{
        description: "Can manage item schedules and availability",
        category: :business
      },
      @can_update_pricing => %{
        description: "Can update pricing information",
        category: :business
      },
      @can_view_pricing => %{
        description: "Can view pricing information",
        category: :business
      },
      @can_manage_layouts => %{
        description: "Can manage layout configurations",
        category: :business
      },
      @can_manage_sections => %{
        description: "Can manage business sections",
        category: :business
      },
      @can_view_clients => %{
        description: "Can view client information",
        category: :clients
      },
      @can_create_clients => %{
        description: "Can create new client accounts",
        category: :clients
      },
      @can_modify_clients => %{
        description: "Can modify client information",
        category: :clients
      },
      @can_process_payments => %{
        description: "Can process payments and transactions",
        category: :payments
      },
      @can_view_payments => %{
        description: "Can view payment information",
        category: :payments
      },
      @can_refund_payments => %{
        description: "Can process refunds",
        category: :payments
      },
      @can_view_reports => %{
        description: "Can access reporting and analytics",
        category: :reports
      },
      @can_export_data => %{
        description: "Can export data and reports",
        category: :reports
      },
      @can_view_analytics => %{
        description: "Can view detailed analytics",
        category: :reports
      },
      @can_access_admin_panel => %{
        description: "Can access the admin panel interface",
        category: :system
      },
      @can_manage_system_settings => %{
        description: "Can manage system-wide settings",
        category: :system
      },
      @can_view_audit_logs => %{
        description: "Can view system audit logs",
        category: :system
      }
    }
  end
end
