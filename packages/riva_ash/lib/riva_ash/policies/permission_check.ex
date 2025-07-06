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

  alias RivaAsh.Permissions
  alias RivaAsh.Permissions.Constants

  @impl true
  def describe(opts) do
    permission = opts[:permission]
    "actor has permission: #{permission}"
  end

  @impl true
  def match?(actor, _context, opts) do
    permission = opts[:permission]

    # Validate permission exists
    unless Constants.valid_permission?(permission) do
      raise ArgumentError, "Unknown permission: #{permission}. Valid permissions: #{inspect(Constants.all_permissions())}"
    end

    case actor do
      # Admins have all permissions
      %{role: :admin} ->
        true

      # Check permission for employees with ID
      %{id: actor_id} when is_binary(actor_id) ->
        Permissions.has_permission?(actor_id, permission)

      # No actor or invalid actor
      _ ->
        false
    end
  end

  @doc """
  Generic permission check function.

  ## Examples:
      authorize_if(PermissionCheck.has_permission(:can_update_pricing))
      authorize_if(PermissionCheck.has_permission("can_update_pricing"))
  """
  def has_permission(permission) when is_atom(permission) do
    permission_string = apply(Constants, permission, [])
    {__MODULE__, permission: permission_string}
  end

  def has_permission(permission) when is_binary(permission) do
    # Validate permission exists
    unless Constants.valid_permission?(permission) do
      raise ArgumentError, "Unknown permission: #{permission}. Valid permissions: #{inspect(Constants.all_permissions())}"
    end

    {__MODULE__, permission: permission}
  end

  # Convenience functions for common permissions - Reservations
  def can_create_reservations, do: has_permission(:can_create_reservations)
  def can_view_all_reservations, do: has_permission(:can_view_all_reservations)
  def can_modify_reservations, do: has_permission(:can_modify_reservations)
  def can_cancel_reservations, do: has_permission(:can_cancel_reservations)
  def can_view_own_reservations, do: has_permission(:can_view_own_reservations)

  # Convenience functions for common permissions - Employees
  def can_view_employees, do: has_permission(:can_view_employees)
  def can_create_employees, do: has_permission(:can_create_employees)
  def can_modify_employees, do: has_permission(:can_modify_employees)
  def can_give_permissions, do: has_permission(:can_give_permissions)
  def can_revoke_permissions, do: has_permission(:can_revoke_permissions)

  # Convenience functions for common permissions - Business
  def can_manage_business_settings, do: has_permission(:can_manage_business_settings)
  def can_manage_items, do: has_permission(:can_manage_items)
  def can_manage_schedules, do: has_permission(:can_manage_schedules)
  def can_update_pricing, do: has_permission(:can_update_pricing)
  def can_view_pricing, do: has_permission(:can_view_pricing)
  def can_manage_layouts, do: has_permission(:can_manage_layouts)
  def can_manage_sections, do: has_permission(:can_manage_sections)

  # Convenience functions for common permissions - Clients
  def can_view_clients, do: has_permission(:can_view_clients)
  def can_create_clients, do: has_permission(:can_create_clients)
  def can_modify_clients, do: has_permission(:can_modify_clients)

  # Convenience functions for common permissions - Payments
  def can_process_payments, do: has_permission(:can_process_payments)
  def can_view_payments, do: has_permission(:can_view_payments)
  def can_refund_payments, do: has_permission(:can_refund_payments)

  # Convenience functions for common permissions - Reports
  def can_view_reports, do: has_permission(:can_view_reports)
  def can_export_data, do: has_permission(:can_export_data)
  def can_view_analytics, do: has_permission(:can_view_analytics)

  # Convenience functions for common permissions - System
  def can_access_admin_panel, do: has_permission(:can_access_admin_panel)
  def can_manage_system_settings, do: has_permission(:can_manage_system_settings)
  def can_view_audit_logs, do: has_permission(:can_view_audit_logs)
end
