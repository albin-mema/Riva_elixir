defmodule RivaAsh.Permissions.Constants do
  @moduledoc """
  Centralized constants for all system permissions.

  This module defines all possible actions/permissions that users can have in the system.
  Use these constants instead of hardcoded strings to ensure consistency and prevent typos.

  The constants are designed to work seamlessly with Ash policies and the SAT solver
  for efficient authorization evaluation.

  ## Type Safety

  All functions are properly typed with Dialyzer specifications for compile-time safety.
  """

  # Configuration
  @permission_prefix Application.compile_env(:riva_ash, :permission_prefix, "can_")
  @permission_separator Application.compile_env(:riva_ash, :permission_separator, "_unmatched")

  # Permission categories
  @reservation_category :reservations
  @employee_category :employees
  @business_category :business
  @client_category :clients
  @payment_category :payments
  @report_category :reports
  @system_category :system
  @plot_category :plots
  @item_hold_category :item_holds

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

  # Plot management permissions
  @can_delete_plot "can_delete_plot"
  @can_view_plot "can_view_plot"

  # Item hold permissions
  @can_create_item_hold "can_create_item_hold"
  @can_release_item_hold "can_release_item_hold"

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

  # Permission metadata struct for type safety
  defstruct permission: nil,
            description: nil,
            category: nil,
            sensitive: false,
            requires_audit: false

  @type permission :: String.t()
  @type category :: atom()
  @type permission_metadata :: %__MODULE__{
          permission: permission(),
          description: String.t(),
          category: category(),
          sensitive: boolean(),
          requires_audit: boolean()
        }
  @type result :: {:ok, any()} | {:error, String.t()}

  @doc """
  Builds a permission string from action and resource parts.

  ## Examples

      iex> build_permission("create", "users")
      "can_create_users"
  """

  @doc """
  Validates that a permission string is properly formatted.

  ## Examples

      iex> valid_permission_format?("can_create_users")
      true

      iex> valid_permission_format?("invalid_permission")
      false
  """
  @spec valid_permission_format?(permission()) :: boolean()
  def valid_permission_format?(permission) when is_binary(permission) do
    permission
    |> has_valid_prefix?()
    |> has_valid_separator?()
  end

  def valid_permission_format?(_unmatched), do: false

  # Helper functions for permission format validation
  @spec has_valid_prefix?(permission()) :: boolean()
  defp has_valid_prefix?(permission) do
    String.starts_with?(permission, @permission_prefix)
  end

  @spec has_valid_separator?(boolean()) :: boolean()
  defp has_valid_separator?(false), do: false

  defp has_valid_separator?(true) do
    String.contains?(@permission_separator)
  end

  # Reservation permissions
  @doc """
  Returns permission to create new reservations.
  """
  @spec can_create_reservations() :: permission()
  def can_create_reservations, do: @can_create_reservations

  @doc """
  Returns permission to view all reservations.
  """
  @spec can_view_all_reservations() :: permission()
  def can_view_all_reservations, do: @can_view_all_reservations

  @doc """
  Returns permission to modify existing reservations.
  """
  @spec can_modify_reservations() :: permission()
  def can_modify_reservations, do: @can_modify_reservations

  @doc """
  Returns permission to cancel reservations.
  """
  @spec can_cancel_reservations() :: permission()
  def can_cancel_reservations, do: @can_cancel_reservations

  @doc """
  Returns permission to view own reservations only.
  """
  @spec can_view_own_reservations() :: permission()
  def can_view_own_reservations, do: @can_view_own_reservations

  # Employee management permissions
  @doc """
  Returns permission to view employee information.
  """
  @spec can_view_employees() :: permission()
  def can_view_employees, do: @can_view_employees

  @doc """
  Returns permission to create new employee accounts.
  """
  @spec can_create_employees() :: permission()
  def can_create_employees, do: @can_create_employees

  @doc """
  Returns permission to modify employee information.
  """
  @spec can_modify_employees() :: permission()
  def can_modify_employees, do: @can_modify_employees

  @doc """
  Returns permission to grant permissions to other employees.
  """
  @spec can_give_permissions() :: permission()
  def can_give_permissions, do: @can_give_permissions

  @doc """
  Returns permission to revoke permissions from other employees.
  """
  @spec can_revoke_permissions() :: permission()
  def can_revoke_permissions, do: @can_revoke_permissions

  # Business management permissions
  @doc """
  Returns permission to modify business settings and configuration.
  """
  @spec can_manage_business_settings() :: permission()
  def can_manage_business_settings, do: @can_manage_business_settings

  @doc """
  Returns permission to create, modify, and delete items.
  """
  @spec can_manage_items() :: permission()
  def can_manage_items, do: @can_manage_items

  @doc """
  Returns permission to manage item schedules and availability.
  """
  @spec can_manage_schedules() :: permission()
  def can_manage_schedules, do: @can_manage_schedules

  @doc """
  Returns permission to update pricing information.
  """
  @spec can_update_pricing() :: permission()
  def can_update_pricing, do: @can_update_pricing

  @doc """
  Returns permission to view pricing information.
  """
  @spec can_view_pricing() :: permission()
  def can_view_pricing, do: @can_view_pricing

  @doc """
  Returns permission to manage layout configurations.
  """
  @spec can_manage_layouts() :: permission()
  def can_manage_layouts, do: @can_manage_layouts

  @doc """
  Returns permission to manage business sections.
  """
  @spec can_manage_sections() :: permission()
  def can_manage_sections, do: @can_manage_sections

  # Plot management permissions
  @doc """
  Returns permission to delete plots.
  """
  @spec can_delete_plot() :: permission()
  def can_delete_plot, do: @can_delete_plot

  @doc """
  Returns permission to view plots.
  """
  @spec can_view_plot() :: permission()
  def can_view_plot, do: @can_view_plot

  # Item hold permissions
  @doc """
  Returns permission to create item holds.
  """
  @spec can_create_item_hold() :: permission()
  def can_create_item_hold, do: @can_create_item_hold

  @doc """
  Returns permission to release item holds.
  """
  @spec can_release_item_hold() :: permission()
  def can_release_item_hold, do: @can_release_item_hold

  # Client management permissions
  @doc """
  Returns permission to view client information.
  """
  @spec can_view_clients() :: permission()
  def can_view_clients, do: @can_view_clients

  @doc """
  Returns permission to create new client accounts.
  """
  @spec can_create_clients() :: permission()
  def can_create_clients, do: @can_create_clients

  @doc """
  Returns permission to modify client information.
  """
  @spec can_modify_clients() :: permission()
  def can_modify_clients, do: @can_modify_clients

  # Payment and financial permissions
  @doc """
  Returns permission to process payments and transactions.
  """
  @spec can_process_payments() :: permission()
  def can_process_payments, do: @can_process_payments

  @doc """
  Returns permission to view payment information.
  """
  @spec can_view_payments() :: permission()
  def can_view_payments, do: @can_view_payments

  @doc """
  Returns permission to process refunds.
  """
  @spec can_refund_payments() :: permission()
  def can_refund_payments, do: @can_refund_payments

  # Reporting permissions
  @doc """
  Returns permission to access reporting and analytics.
  """
  @spec can_view_reports() :: permission()
  def can_view_reports, do: @can_view_reports

  @doc """
  Returns permission to export data and reports.
  """
  @spec can_export_data() :: permission()
  def can_export_data, do: @can_export_data

  @doc """
  Returns permission to view detailed analytics.
  """
  @spec can_view_analytics() :: permission()
  def can_view_analytics, do: @can_view_analytics

  # System permissions
  @doc """
  Returns permission to access the admin panel interface.
  """
  @spec can_access_admin_panel() :: permission()
  def can_access_admin_panel, do: @can_access_admin_panel

  @doc """
  Returns permission to manage system-wide settings.
  """
  @spec can_manage_system_settings() :: permission()
  def can_manage_system_settings, do: @can_manage_system_settings

  @doc """
  Returns permission to view system audit logs.
  """
  @spec can_view_audit_logs() :: permission()
  def can_view_audit_logs, do: @can_view_audit_logs

  @doc """
  Returns all available permissions as a list.

  Useful for seeding, validation, or UI generation.

  ## Returns

  Returns `{:ok, [permission()]}` on success, `{:error, reason}` on failure.
  """
  @spec all_permissions() :: result()
  def all_permissions do
    permissions =
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
        @can_delete_plot,
        @can_view_plot,
        @can_create_item_hold,
        @can_release_item_hold,
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

    validate_permissions(permissions)
    |> case do
      :ok -> {:ok, permissions}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Returns permissions grouped by category.

  This matches the categories used in the Permission resource.

  ## Returns

  Returns `{:ok, %{category() => [permission()]}}` on success, `{:error, reason}` on failure.
  """
  @spec permissions_by_category() :: result()
  def permissions_by_category do
    categories = build_category_map()

    categories
    |> extract_all_permissions()
    |> validate_permissions()
    |> handle_validation_result(categories)
  end

  @spec build_category_map() :: map()
  defp build_category_map do
    %{
      @reservation_category => build_reservation_permissions(),
      @employee_category => build_employee_permissions(),
      @business_category => build_business_permissions(),
      @client_category => build_client_permissions(),
      @payment_category => build_payment_permissions(),
      @report_category => build_report_permissions(),
      @system_category => build_system_permissions(),
      @plot_category => build_plot_permissions(),
      @item_hold_category => build_item_hold_permissions()
    }
  end

  # Category-specific permission builders
  @spec build_reservation_permissions() :: [permission()]
  defp build_reservation_permissions do
    [
      @can_create_reservations,
      @can_view_all_reservations,
      @can_modify_reservations,
      @can_cancel_reservations,
      @can_view_own_reservations
    ]
  end

  @spec build_employee_permissions() :: [permission()]
  defp build_employee_permissions do
    [
      @can_view_employees,
      @can_create_employees,
      @can_modify_employees,
      @can_give_permissions,
      @can_revoke_permissions
    ]
  end

  @spec build_business_permissions() :: [permission()]
  defp build_business_permissions do
    [
      @can_manage_business_settings,
      @can_manage_items,
      @can_manage_schedules,
      @can_update_pricing,
      @can_view_pricing,
      @can_manage_layouts,
      @can_manage_sections
    ]
  end

  @spec build_client_permissions() :: [permission()]
  defp build_client_permissions do
    [
      @can_view_clients,
      @can_create_clients,
      @can_modify_clients
    ]
  end

  @spec build_payment_permissions() :: [permission()]
  defp build_payment_permissions do
    [
      @can_process_payments,
      @can_view_payments,
      @can_refund_payments
    ]
  end

  @spec build_report_permissions() :: [permission()]
  defp build_report_permissions do
    [
      @can_view_reports,
      @can_export_data,
      @can_view_analytics
    ]
  end

  @spec build_system_permissions() :: [permission()]
  defp build_system_permissions do
    [
      @can_access_admin_panel,
      @can_manage_system_settings,
      @can_view_audit_logs
    ]
  end

  @spec build_plot_permissions() :: [permission()]
  defp build_plot_permissions do
    [
      @can_delete_plot,
      @can_view_plot
    ]
  end

  @spec build_item_hold_permissions() :: [permission()]
  defp build_item_hold_permissions do
    [
      @can_create_item_hold,
      @can_release_item_hold
    ]
  end

  @spec extract_all_permissions(map()) :: [permission()]
  defp extract_all_permissions(categories) do
    categories
    |> Map.values()
    |> List.flatten()
  end

  @spec handle_validation_result(:ok | {:error, String.t()}, map()) :: result()
  defp handle_validation_result(:ok, categories), do: {:ok, categories}
  defp handle_validation_result({:error, reason}, _categories), do: {:error, reason}

  @doc """
  Returns the category for a given permission.

  ## Examples

      iex> category_for_permission("can_create_users")
      {:ok, :reservations}

      iex> category_for_permission("invalid_permission")
      {:error, "Permission not found"}
  """
  @spec category_for_permission(permission()) :: result()
  def category_for_permission(permission) when is_binary(permission) do
    with {:ok, categories} <- permissions_by_category(),
         {category, _permissions} <-
           Enum.find(categories, fn {_category, permissions} ->
             permission in permissions
           end) do
      {:ok, category}
    else
      nil -> {:error, "Permission not found"}
      error -> error
    end
  end

  def category_unmatchedfor_unmatchedpermission(_unmatched), do: {:error, "Invalid permission format"}

  @doc """
  Validates that a permission exists in the system.

  ## Examples

      iex> valid_permission?("can_create_users")
      {:ok, true}

      iex> valid_permission?("invalid_permission")
      {:ok, false}
  """
  @spec valid_permission?(permission()) :: result()
  def valid_permission?(permission) when is_binary(permission) do
    case all_permissions() do
      {:ok, permissions} -> {:ok, permission in permissions}
      {:error, reason} -> {:error, reason}
    end
  end

  def valid_permission?(_unmatched), do: {:ok, false}

  @doc """
  Returns permission metadata including description and category.

  This can be used for UI generation and documentation.

  ## Returns

  Returns `{:ok, %{permission() => permission_metadata()}}` on success, `{:error, reason}` on failure.
  """
  @spec permission_metadata() :: result()
  def permission_metadata do
    metadata = build_permission_metadata()

    validate_permissions(Map.keys(metadata))
    |> handle_metadata_validation(metadata)
  end

  @spec build_permission_metadata() :: map()
  defp build_permission_metadata do
    %{
      @can_create_reservations =>
        build_permission_metadata_entry(
          @can_create_reservations,
          "Can create new reservations for clients",
          @reservation_category,
          false,
          true
        ),
      @can_view_all_reservations =>
        build_permission_metadata_entry(
          @can_view_all_reservations,
          "Can view all reservations, not just own",
          @reservation_category,
          false,
          true
        ),
      @can_modify_reservations =>
        build_permission_metadata_entry(
          @can_modify_reservations,
          "Can modify existing reservations",
          @reservation_category,
          false,
          true
        ),
      @can_cancel_reservations =>
        build_permission_metadata_entry(
          @can_cancel_reservations,
          "Can cancel reservations",
          @reservation_category,
          false,
          true
        ),
      @can_view_own_reservations =>
        build_permission_metadata_entry(
          @can_view_own_reservations,
          "Can view own reservations only",
          @reservation_category,
          false,
          false
        ),
      @can_view_employees =>
        build_permission_metadata_entry(
          @can_view_employees,
          "Can view employee information",
          @employee_category,
          true,
          true
        ),
      @can_create_employees =>
        build_permission_metadata_entry(
          @can_create_employees,
          "Can create new employee accounts",
          @employee_category,
          true,
          true
        ),
      @can_modify_employees =>
        build_permission_metadata_entry(
          @can_modify_employees,
          "Can modify employee information",
          @employee_category,
          true,
          true
        ),
      @can_give_permissions =>
        build_permission_metadata_entry(
          @can_give_permissions,
          "Can grant permissions to other employees",
          @employee_category,
          true,
          true
        ),
      @can_revoke_permissions =>
        build_permission_metadata_entry(
          @can_revoke_permissions,
          "Can revoke permissions from other employees",
          @employee_category,
          true,
          true
        ),
      @can_manage_business_settings =>
        build_permission_metadata_entry(
          @can_manage_business_settings,
          "Can modify business settings and configuration",
          @business_category,
          false,
          true
        ),
      @can_manage_items =>
        build_permission_metadata_entry(
          @can_manage_items,
          "Can create, modify, and delete items",
          @business_category,
          false,
          true
        ),
      @can_manage_schedules =>
        build_permission_metadata_entry(
          @can_manage_schedules,
          "Can manage item schedules and availability",
          @business_category,
          false,
          true
        ),
      @can_update_pricing =>
        build_permission_metadata_entry(
          @can_update_pricing,
          "Can update pricing information",
          @business_category,
          true,
          true
        ),
      @can_view_pricing =>
        build_permission_metadata_entry(
          @can_view_pricing,
          "Can view pricing information",
          @business_category,
          true,
          false
        ),
      @can_manage_layouts =>
        build_permission_metadata_entry(
          @can_manage_layouts,
          "Can manage layout configurations",
          @business_category,
          false,
          true
        ),
      @can_manage_sections =>
        build_permission_metadata_entry(
          @can_manage_sections,
          "Can manage business sections",
          @business_category,
          false,
          true
        ),
      @can_delete_plot =>
        build_permission_metadata_entry(
          @can_delete_plot,
          "Can delete plots",
          @plot_category,
          false,
          true
        ),
      @can_view_plot =>
        build_permission_metadata_entry(
          @can_view_plot,
          "Can view plots",
          @plot_category,
          false,
          false
        ),
      @can_create_item_hold =>
        build_permission_metadata_entry(
          @can_create_item_hold,
          "Can create item holds",
          @item_hold_category,
          false,
          true
        ),
      @can_release_item_hold =>
        build_permission_metadata_entry(
          @can_release_item_hold,
          "Can release item holds",
          @item_hold_category,
          false,
          true
        ),
      @can_view_clients =>
        build_permission_metadata_entry(
          @can_view_clients,
          "Can view client information",
          @client_category,
          true,
          true
        ),
      @can_create_clients =>
        build_permission_metadata_entry(
          @can_create_clients,
          "Can create new client accounts",
          @client_category,
          true,
          true
        ),
      @can_modify_clients =>
        build_permission_metadata_entry(
          @can_modify_clients,
          "Can modify client information",
          @client_category,
          true,
          true
        ),
      @can_process_payments =>
        build_permission_metadata_entry(
          @can_process_payments,
          "Can process payments and transactions",
          @payment_category,
          true,
          true
        ),
      @can_view_payments =>
        build_permission_metadata_entry(
          @can_view_payments,
          "Can view payment information",
          @payment_category,
          true,
          true
        ),
      @can_refund_payments =>
        build_permission_metadata_entry(
          @can_refund_payments,
          "Can process refunds",
          @payment_category,
          true,
          true
        ),
      @can_view_reports =>
        build_permission_metadata_entry(
          @can_view_reports,
          "Can access reporting and analytics",
          @report_category,
          false,
          true
        ),
      @can_export_data =>
        build_permission_metadata_entry(
          @can_export_data,
          "Can export data and reports",
          @report_category,
          true,
          true
        ),
      @can_view_analytics =>
        build_permission_metadata_entry(
          @can_view_analytics,
          "Can view detailed analytics",
          @report_category,
          false,
          true
        ),
      @can_access_admin_panel =>
        build_permission_metadata_entry(
          @can_access_admin_panel,
          "Can access the admin panel interface",
          @system_category,
          true,
          true
        ),
      @can_manage_system_settings =>
        build_permission_metadata_entry(
          @can_manage_system_settings,
          "Can manage system-wide settings",
          @system_category,
          true,
          true
        ),
      @can_view_audit_logs =>
        build_permission_metadata_entry(
          @can_view_audit_logs,
          "Can view system audit logs",
          @system_category,
          true,
          true
        )
    }
  end

  @spec build_permission_metadata_entry(permission(), String.t(), category(), boolean(), boolean()) ::
          permission_metadata()
  defp build_permission_metadata_entry(permission, description, category, sensitive, requires_audit) do
    %__MODULE__{
      permission: permission,
      description: description,
      category: category,
      sensitive: sensitive,
      requires_audit: requires_audit
    }
  end

  @spec handle_metadata_validation(:ok | {:error, String.t()}, map()) :: result()
  defp handle_metadata_validation(:ok, metadata), do: {:ok, metadata}
  defp handle_metadata_validation({:error, reason}, _metadata), do: {:error, reason}

  @doc """
  Returns sensitive permissions that require special handling.

  ## Examples

      iex> sensitive_permissions()
      {:ok, ["can_give_permissions", "can_view_audit_logs"]}
  """
  @spec sensitive_permissions() :: result()
  def sensitive_permissions do
    with {:ok, metadata} <- permission_metadata() do
      metadata
      |> extract_sensitive_permissions()
    end
  end

  @doc """
  Returns permissions that require audit logging.

  ## Examples

      iex> audit_required_permissions()
      {:ok, ["can_create_reservations", "can_modify_employees"]}
  """
  @spec audit_required_permissions() :: result()
  def audit_required_permissions do
    with {:ok, metadata} <- permission_metadata() do
      metadata
      |> extract_audit_required_permissions()
    end
  end

  @spec extract_sensitive_permissions(map()) :: result()
  defp extract_sensitive_permissions(metadata) do
    sensitive_perms =
      metadata
      |> Enum.filter(fn {_perm, info} -> info.sensitive end)
      |> Enum.map(fn {perm, _info} -> perm end)

    {:ok, sensitive_perms}
  end

  @spec extract_audit_required_permissions(map()) :: result()
  defp extract_audit_required_permissions(metadata) do
    audit_perms =
      metadata
      |> Enum.filter(fn {_perm, info} -> info.requires_audit end)
      |> Enum.map(fn {perm, _info} -> perm end)

    {:ok, audit_perms}
  end

  @doc """
  Validates a list of permissions.

  ## Examples

      iex> validate_permissions(["can_create_users", "invalid_permission"])
      {:error, "Invalid permission format: invalid_permission"}
  """
  @spec validate_permissions([permission()]) :: :ok | {:error, String.t()}
  def validate_permissions(permissions) when is_list(permissions) do
    invalid_permissions =
      permissions
      |> Enum.reject(&valid_permission_format?/1)

    case invalid_permissions do
      [] -> :ok
      [invalid | _unmatched] -> {:error, "Invalid permission format: #{invalid}"}
    end
  end

  def validate_unmatchedpermissions(_unmatched), do: {:error, "Invalid permissions list"}

  @doc """
  Gets permissions by category with validation.

  ## Examples

      iex> permissions_for_category(:reservations)
      {:ok, ["can_create_reservations", "can_view_all_reservations"]}
  """
  @spec permissions_for_category(category()) :: result()
  def permissions_for_category(category) when is_atom(category) do
    with {:ok, categories} <- permissions_by_category() do
      case Map.get(categories, category) do
        nil -> {:error, "Category not found: #{category}"}
        permissions -> {:ok, permissions}
      end
    end
  end

  def permissions_unmatchedfor_unmatchedcategory(_unmatched), do: {:error, "Invalid category"}

  @doc """
  Checks if a permission is sensitive.

  ## Examples

      iex> permission_sensitive?("can_view_audit_logs")
      {:ok, true}

      iex> permission_sensitive?("can_view_own_reservations")
      {:ok, false}
  """
  @spec permission_sensitive?(permission()) :: result()
  def permission_sensitive?(permission) when is_binary(permission) do
    with {:ok, metadata} <- permission_metadata() do
      case Map.get(metadata, permission) do
        nil -> {:error, "Permission not found: #{permission}"}
        info -> {:ok, info.sensitive}
      end
    end
  end

  def permission_sensitive?(_unmatched), do: {:error, "Invalid permission"}

  @doc """
  Checks if a permission requires audit logging.

  ## Examples

      iex> permission_requires_audit?("can_create_reservations")
      {:ok, true}

      iex> permission_requires_audit?("can_view_own_reservations")
      {:ok, false}
  """
  @spec permission_requires_audit?(permission()) :: result()
  def permission_requires_audit?(permission) when is_binary(permission) do
    with {:ok, metadata} <- permission_metadata() do
      case Map.get(metadata, permission) do
        nil -> {:error, "Permission not found: #{permission}"}
        info -> {:ok, info.requires_audit}
      end
    end
  end

  def permission_requires_audit?(_unmatched), do: {:error, "Invalid permission"}
end
