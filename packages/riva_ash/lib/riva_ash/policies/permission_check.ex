defmodule RivaAsh.Policies.PermissionCheck do
  @moduledoc """
  Custom Ash policy check for permission-based authorization.

  This check works with Ash's SAT solver to efficiently evaluate permission-based
  authorization rules. It integrates with the RivaAsh.Permissions system to check
  if an actor has specific permissions.

  The check supports both direct permission assignments and role-based permissions
  (admins have all permissions by default).

  ## Usage in policies:

      policies do
        policy action_type(:update) do
          authorize_if(RivaAsh.Policies.PermissionCheck.has_permission(:can_update_pricing))
        end
      end

  ## Or using the helper functions:

      policies do
        policy action_type(:update) do
          authorize_if(PermissionCheck.can_update_pricing())
        end
      end
  """

  use Ash.Policy.SimpleCheck

  require Logger
  alias RivaAsh.Permissions
  alias RivaAsh.Permissions.Constants

  @type actor :: map()
  @type context :: map()
  @type opts :: keyword()
  @type permission :: atom() | binary()
  @type check_result :: boolean()

  @impl true
  @spec describe(opts()) :: String.t()
  def describe(opts) do
    permission = opts[:permission]
    "actor has permission: #{permission}"
  end

  @impl true
  @spec match?(actor(), context(), opts()) :: check_result()
  def match?(actor, _context, opts) do
    Logger.debug("Starting permission check for actor: #{inspect(actor)}")

    with {:ok, permission} <- extract_permission(opts),
         :ok <- validate_permission(permission),
         :ok <- validate_actor(actor) do
      result = check_permission(actor, permission)
      Logger.debug("Permission check result for #{permission}: #{result}")
      result
    else
      {:error, reason} ->
        Logger.debug("Permission check failed: #{inspect(reason)}")
        false
    end
  end

  @doc """
  Generic permission check function.

  ## Examples:
      authorize_if(PermissionCheck.has_permission(:can_update_pricing))
      authorize_if(PermissionCheck.has_permission("can_update_pricing"))
  """
  @spec has_permission(permission()) :: {__MODULE__, permission: permission()}
  def has_permission(permission) when is_atom(permission) do
    permission_string = apply(Constants, permission, [])
    {__MODULE__, permission: permission_string}
  end

  def has_permission(permission) when is_binary(permission) do
    case validate_permission(permission) do
      :ok -> {__MODULE__, permission: permission}
      {:error, _reason} -> raise_permission_error(permission)
    end
  end

  # Convenience functions for common permissions - Reservations
  @spec can_create_reservations() :: {__MODULE__, permission: permission()}
  def can_create_reservations, do: has_permission(:can_create_reservations)

  @spec can_view_all_reservations() :: {__MODULE__, permission: permission()}
  def can_view_all_reservations, do: has_permission(:can_view_all_reservations)

  @spec can_modify_reservations() :: {__MODULE__, permission: permission()}
  def can_modify_reservations, do: has_permission(:can_modify_reservations)

  @spec can_cancel_reservations() :: {__MODULE__, permission: permission()}
  def can_cancel_reservations, do: has_permission(:can_cancel_reservations)

  @spec can_view_own_reservations() :: {__MODULE__, permission: permission()}
  def can_view_own_reservations, do: has_permission(:can_view_own_reservations)

  # Convenience functions for common permissions - Employees
  @spec can_view_employees() :: {__MODULE__, permission: permission()}
  def can_view_employees, do: has_permission(:can_view_employees)

  @spec can_create_employees() :: {__MODULE__, permission: permission()}
  def can_create_employees, do: has_permission(:can_create_employees)

  @spec can_modify_employees() :: {__MODULE__, permission: permission()}
  def can_modify_employees, do: has_permission(:can_modify_employees)

  @spec can_give_permissions() :: {__MODULE__, permission: permission()}
  def can_give_permissions, do: has_permission(:can_give_permissions)

  @spec can_revoke_permissions() :: {__MODULE__, permission: permission()}
  def can_revoke_permissions, do: has_permission(:can_revoke_permissions)

  # Convenience functions for common permissions - Business
  @spec can_manage_business_settings() :: {__MODULE__, permission: permission()}
  def can_manage_business_settings, do: has_permission(:can_manage_business_settings)

  @spec can_manage_items() :: {__MODULE__, permission: permission()}
  def can_manage_items, do: has_permission(:can_manage_items)

  @spec can_manage_schedules() :: {__MODULE__, permission: permission()}
  def can_manage_schedules, do: has_permission(:can_manage_schedules)

  @spec can_update_pricing() :: {__MODULE__, permission: permission()}
  def can_update_pricing, do: has_permission(:can_update_pricing)

  @spec can_view_pricing() :: {__MODULE__, permission: permission()}
  def can_view_pricing, do: has_permission(:can_view_pricing)

  @spec can_manage_layouts() :: {__MODULE__, permission: permission()}
  def can_manage_layouts, do: has_permission(:can_manage_layouts)

  @spec can_manage_sections() :: {__MODULE__, permission: permission()}
  def can_manage_sections, do: has_permission(:can_manage_sections)

  # Convenience functions for common permissions - Clients
  @spec can_view_clients() :: {__MODULE__, permission: permission()}
  def can_view_clients, do: has_permission(:can_view_clients)

  @spec can_create_clients() :: {__MODULE__, permission: permission()}
  def can_create_clients, do: has_permission(:can_create_clients)

  @spec can_modify_clients() :: {__MODULE__, permission: permission()}
  def can_modify_clients, do: has_permission(:can_modify_clients)

  # Convenience functions for common permissions - Payments
  @spec can_process_payments() :: {__MODULE__, permission: permission()}
  def can_process_payments, do: has_permission(:can_process_payments)

  @spec can_view_payments() :: {__MODULE__, permission: permission()}
  def can_view_payments, do: has_permission(:can_view_payments)

  @spec can_refund_payments() :: {__MODULE__, permission: permission()}
  def can_refund_payments, do: has_permission(:can_refund_payments)

  # Convenience functions for common permissions - Reports
  @spec can_view_reports() :: {__MODULE__, permission: permission()}
  def can_view_reports, do: has_permission(:can_view_reports)

  @spec can_export_data() :: {__MODULE__, permission: permission()}
  def can_export_data, do: has_permission(:can_export_data)

  @spec can_view_analytics() :: {__MODULE__, permission: permission()}
  def can_view_analytics, do: has_permission(:can_view_analytics)

  # Convenience functions for common permissions - System
  @spec can_access_admin_panel() :: {__MODULE__, permission: permission()}
  def can_access_admin_panel, do: has_permission(:can_access_admin_panel)

  @spec can_manage_system_settings() :: {__MODULE__, permission: permission()}
  def can_manage_system_settings, do: has_permission(:can_manage_system_settings)

  @spec can_view_audit_logs() :: {__MODULE__, permission: permission()}
  def can_view_audit_logs, do: has_permission(:can_view_audit_logs)

  # Private helper functions with single level of abstraction

  @spec extract_permission(opts()) :: {:ok, permission()} | {:error, :no_permission}
  defp extract_permission(opts) do
    case Keyword.get(opts, :permission) do
      nil -> {:error, :no_permission}
      permission -> {:ok, permission}
    end
  end

  @spec validate_permission(permission()) :: :ok | {:error, String.t()}
  defp validate_permission(permission) when is_binary(permission) do
    Logger.debug("Validating permission: #{permission}")

    if Constants.valid_permission?(permission) do
      Logger.debug("Permission validation passed")
      :ok
    else
      error_msg = permission_error_message(permission)
      Logger.debug("Permission validation failed: #{error_msg}")
      {:error, error_msg}
    end
  end

  defp validate_permission(permission) when is_atom(permission) do
    Logger.debug("Converting atom permission to string: #{permission}")
    permission_string = apply(Constants, permission, [])
    validate_permission(permission_string)
  end

  @spec validate_actor(actor()) :: :ok | :error
  defp validate_actor(%{role: _role} = _actor) do
    Logger.debug("Actor validation passed: has role")
    :ok
  end

  defp validate_actor(%{id: actor_id} = _actor) when is_binary(actor_id) do
    Logger.debug("Actor validation passed: has ID #{actor_id}")
    :ok
  end

  defp validate_actor(_actor) do
    Logger.debug("Actor validation failed: missing role or ID")
    :error
  end

  @spec check_permission(actor(), permission()) :: boolean()
  defp check_permission(%{role: :admin}, _permission) do
    Logger.debug("Admin role detected: all permissions granted")
    true
  end

  defp check_permission(%{id: actor_id}, permission) when is_binary(actor_id) do
    Logger.debug("Checking permission #{permission} for actor #{actor_id}")
    result = Permissions.has_permission?(actor_id, permission)
    Logger.debug("Permission check result: #{result}")
    result
  end

  defp check_permission(_actor, _permission) do
    Logger.debug("Permission check failed: invalid actor")
    false
  end

  @spec permission_error_message(permission()) :: String.t()
  defp permission_error_message(permission) do
    "Unknown permission: #{permission}. Valid permissions: #{inspect(Constants.all_permissions())}"
  end

  @spec raise_permission_error(permission()) :: no_return()
  defp raise_permission_error(permission) do
    raise ArgumentError, permission_error_message(permission)
  end
end
