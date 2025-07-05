defmodule RivaAsh.Permissions do
  @moduledoc """
  Helper module for managing employee permissions and authorization.

  This module provides functions to:
  - Check if an employee has specific permissions
  - Grant permissions to employees (with hierarchy validation)
  - Revoke permissions from employees
  - Get all permissions for an employee
  - Validate permission hierarchy rules
  """

  alias RivaAsh.Resources.{Employee, Permission, EmployeePermission}
  alias RivaAsh.Domain

  import Ash.Expr
  require Ash.Query

  @doc """
  Checks if an employee has a specific permission.

  This checks both:
  1. Direct permission assignments
  2. Role-based permissions (admins have all permissions)

  ## Examples

      iex> RivaAsh.Permissions.has_permission?(employee, "can_create_reservations")
      true

      iex> RivaAsh.Permissions.has_permission?(employee, "can_manage_business_settings")
      false
  """
  def has_permission?(employee, permission_name) when is_binary(permission_name) do
    # Admins have all permissions
    if employee.role == :admin do
      true
    else
      # Check if employee has the permission directly assigned
      case get_employee_permissions(employee.id) do
        {:ok, permissions} ->
          Enum.any?(permissions, fn p -> p.name == permission_name end)

        {:error, _} ->
          false
      end
    end
  end

  def has_permission?(employee_id, permission_name)
      when is_binary(employee_id) and is_binary(permission_name) do
    case Ash.get(Employee, employee_id, domain: Domain) do
      {:ok, employee} -> has_permission?(employee, permission_name)
      {:error, _} -> false
    end
  end

  @doc """
  Gets all permissions for an employee.

  Returns both directly assigned permissions and role-based permissions.
  """
  def get_employee_permissions(employee_id) when is_binary(employee_id) do
    case Ash.get(Employee, employee_id, domain: Domain) do
      {:ok, employee} ->
        permissions =
          employee
          |> Ash.load!(:permissions, domain: Domain)
          |> Map.get(:permissions, [])

        {:ok, permissions}

      error ->
        error
    end
  end

  @doc """
  Grants a permission to an employee.

  Validates that the granter has the authority to grant this permission.
  Only employees with "can_give_permissions" permission or admins/managers can grant permissions.

  ## Examples

      iex> RivaAsh.Permissions.grant_permission(manager_id, employee_id, "can_create_reservations")
      {:ok, %EmployeePermission{}}

      iex> RivaAsh.Permissions.grant_permission(staff_id, employee_id, "can_manage_employees")
      {:error, :insufficient_permissions}
  """
  def grant_permission(granter_id, employee_id, permission_name, notes \\ nil) do
    with {:ok, granter} <- Ash.get(Employee, granter_id, domain: Domain),
         {:ok, employee} <- Ash.get(Employee, employee_id, domain: Domain),
         {:ok, permission} <- get_permission_by_name(permission_name),
         :ok <- validate_can_grant_permission(granter, permission),
         :ok <- validate_hierarchy(granter, employee),
         {:ok, _} <- create_permission_assignment(employee_id, permission.id, granter_id, notes) do
      {:ok, :permission_granted}
    else
      {:error, reason} -> {:error, reason}
      error -> error
    end
  end

  @doc """
  Revokes a permission from an employee.

  Only the granter or someone with higher authority can revoke permissions.
  """
  def revoke_permission(revoker_id, employee_id, permission_name) do
    with {:ok, revoker} <- Ash.get(Employee, revoker_id, domain: Domain),
         {:ok, _employee} <- Ash.get(Employee, employee_id, domain: Domain),
         {:ok, permission} <- get_permission_by_name(permission_name),
         {:ok, employee_permission} <- find_employee_permission(employee_id, permission.id),
         :ok <- validate_can_revoke_permission(revoker, employee_permission) do
      Ash.destroy(employee_permission, domain: Domain)
    else
      {:error, reason} -> {:error, reason}
      error -> error
    end
  end

  @doc """
  Gets all available permissions that can be assigned.
  """
  def list_assignable_permissions do
    Permission
    |> Ash.Query.filter(expr(is_assignable == true))
    |> Ash.read!(domain: Domain)
  end

  @doc """
  Gets permissions by category.
  """
  def list_permissions_by_category(category) do
    Permission
    |> Ash.Query.filter(expr(category == ^category))
    |> Ash.read!(domain: Domain)
  end

  # Private helper functions

  defp get_permission_by_name(permission_name) do
    Permission
    |> Ash.Query.filter(expr(name == ^permission_name))
    |> Ash.read_one(domain: Domain)
    |> case do
      {:ok, nil} -> {:error, :permission_not_found}
      {:ok, permission} -> {:ok, permission}
      error -> error
    end
  end

  defp validate_can_grant_permission(granter, _permission) when granter.role == :admin, do: :ok

  defp validate_can_grant_permission(granter, _permission) when granter.role == :manager, do: :ok

  defp validate_can_grant_permission(granter, _permission) do
    if has_permission?(granter, "can_give_permissions") do
      :ok
    else
      {:error, :insufficient_permissions}
    end
  end

  defp validate_hierarchy(granter, _employee) when granter.role == :admin, do: :ok

  defp validate_hierarchy(granter, employee)
       when granter.role == :manager and employee.role in [:staff],
       do: :ok

  defp validate_hierarchy(granter, employee)
       when granter.role == :manager and employee.role == :manager,
       do: {:error, :cannot_grant_to_peer}

  defp validate_hierarchy(granter, employee)
       when granter.role == :manager and employee.role == :admin,
       do: {:error, :cannot_grant_to_superior}

  defp validate_hierarchy(granter, _employee) when granter.role == :staff,
    do: {:error, :insufficient_authority}

  defp create_permission_assignment(employee_id, permission_id, granted_by_id, notes) do
    EmployeePermission
    |> Ash.Changeset.for_create(:grant_permission, %{
      employee_id: employee_id,
      permission_id: permission_id,
      granted_by_id: granted_by_id,
      notes: notes
    })
    |> Ash.create(domain: Domain)
  end

  defp find_employee_permission(employee_id, permission_id) do
    EmployeePermission
    |> Ash.Query.filter(expr(employee_id == ^employee_id and permission_id == ^permission_id))
    |> Ash.read_one(domain: Domain)
    |> case do
      {:ok, nil} -> {:error, :permission_not_found}
      {:ok, employee_permission} -> {:ok, employee_permission}
      error -> error
    end
  end

  defp validate_can_revoke_permission(revoker, _employee_permission) when revoker.role == :admin,
    do: :ok

  defp validate_can_revoke_permission(revoker, employee_permission)
       when revoker.id == employee_permission.granted_by_id,
       do: :ok

  defp validate_can_revoke_permission(_revoker, _employee_permission),
    do: {:error, :insufficient_permissions}
end
