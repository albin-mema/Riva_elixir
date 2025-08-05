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
  alias RivaAsh.Permissions.Constants

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
  @spec has_permission?(Employee.t(), String.t()) :: boolean()
  def has_permission?(employee, permission_name) when is_binary(permission_name) do
    # Validate permission exists
    unless Constants.valid_permission?(permission_name) do
      raise ArgumentError,
            "Unknown permission: #{permission_name}. Valid permissions: #{inspect(Constants.all_permissions())}"
    end

    # Admins have all permissions
    if employee.role == "admin" do
      true
    else
      # Check if employee has the permission directly assigned
      case get_employee_permissions(employee.id) do
        {:ok, permissions} ->
          Enum.any?(permissions, &permission_name_match?(&1, permission_name))

        {:error, _} ->
          false
      end
    end
  end

  @spec permission_name_match?(Permission.t(), String.t()) :: boolean()
  defp permission_name_match?(permission, permission_name) do
    permission.name == permission_name
  end

  @spec has_permission?(String.t(), String.t()) :: boolean()
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
  @spec get_employee_permissions(String.t()) :: {:ok, list(Permission.t())} | {:error, any()}
  def get_employee_permissions(employee_id) when is_binary(employee_id) do
    case Ash.get(Employee, employee_id, domain: Domain) do
      {:ok, employee} ->
        permissions = load_employee_permissions(employee)
        {:ok, permissions}

      error ->
        error
    end
  end

  @spec load_employee_permissions(Employee.t()) :: list(Permission.t())
  defp load_employee_permissions(employee) do
    employee
    |> Ash.load!(:permissions, domain: Domain)
    |> Map.get(:permissions, [])
  end

  @doc """
  Grants a permission to an employee.

  Validates that the granter has the authority to grant this permission.
  Only employees with "can_give_permissions" permission or admins/managers can grant permissions.

  ## Examples

      iex> RivaAsh.Permissions.grant_permission(manager_id, employee_id, "can_create_reservations")
      {:ok, :permission_granted}

      iex> RivaAsh.Permissions.grant_permission(staff_id, employee_id, "can_manage_employees")
      {:error, :insufficient_permissions}
  """
  @spec grant_permission(String.t(), String.t(), String.t(), String.t() | nil) ::
        {:ok, :permission_granted} | {:error, any()}
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

  @spec validate_granter_authority(Employee.t(), Permission.t()) :: :ok | {:error, atom()}
  defp validate_granter_authority(granter, permission) do
    validate_can_grant_permission(granter, permission)
  end

  @spec validate_employee_hierarchy(Employee.t(), Employee.t()) :: :ok | {:error, atom()}
  defp validate_employee_hierarchy(granter, employee) do
    validate_hierarchy(granter, employee)
  end

  @spec create_permission_record(String.t(), String.t(), String.t(), String.t() | nil) ::
        {:ok, EmployeePermission.t()} | {:error, any()}
  defp create_permission_record(employee_id, permission_id, granted_by_id, notes) do
    create_permission_assignment(employee_id, permission_id, granted_by_id, notes)
  end

  @doc """
  Revokes a permission from an employee.

  Only the granter or someone with higher authority can revoke permissions.
  """
  @spec revoke_permission(String.t(), String.t(), String.t()) ::
        {:ok, EmployeePermission.t()} | {:error, any()}
  def revoke_permission(revoker_id, employee_id, permission_name) do
    with {:ok, revoker} <- Ash.get(Employee, revoker_id, domain: Domain),
         {:ok, _employee} <- Ash.get(Employee, employee_id, domain: Domain),
         {:ok, permission} <- get_permission_by_name(permission_name),
         {:ok, employee_permission} <- find_employee_permission(employee_id, permission.id),
         :ok <- validate_can_revoke_permission(revoker, employee_permission) do
      destroy_permission_assignment(employee_permission)
    else
      {:error, reason} -> {:error, reason}
      error -> error
    end
  end

  @spec destroy_permission_assignment(EmployeePermission.t()) :: :ok | {:error, any()}
  defp destroy_permission_assignment(employee_permission) do
    case Ash.destroy(employee_permission, domain: Domain) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  @doc """
  Gets all available permissions that can be assigned.
  """
  @spec list_assignable_permissions() :: list(Permission.t())
  def list_assignable_permissions do
    Permission
    |> Ash.Query.filter(expr(is_assignable == true))
    |> Ash.read!(domain: Domain)
  end

  @doc """
  Gets permissions by category.
  """
  @spec list_permissions_by_category(String.t()) :: list(Permission.t())
  def list_permissions_by_category(category) when is_binary(category) do
    Permission
    |> Ash.Query.filter(expr(category == ^category))
    |> Ash.read!(domain: Domain)
  end

  # Private helper functions

  @spec get_permission_by_name(String.t()) :: {:ok, Permission.t()} | {:error, any()}
  defp get_permission_by_name(permission_name) when is_binary(permission_name) do
    Permission
    |> Ash.Query.filter(expr(name == ^permission_name))
    |> Ash.read_one(domain: Domain)
    |> case do
      {:ok, nil} -> {:error, :permission_not_found}
      {:ok, permission} -> {:ok, permission}
      error -> error
    end
  end

  @spec find_permission_by_name(String.t()) :: {:ok, Permission.t()} | {:error, any()}
  defp find_permission_by_name(permission_name) do
    get_permission_by_name(permission_name)
  end

  @spec handle_permission_query_result({:ok, Permission.t() | nil} | {:error, any()}) ::
        {:ok, Permission.t()} | {:error, any()}
  defp handle_permission_query_result({:ok, nil}), do: {:error, :permission_not_found}
  defp handle_permission_query_result({:ok, permission}), do: {:ok, permission}
  defp handle_permission_query_result(error), do: error

  @spec validate_can_grant_permission(Employee.t(), Permission.t()) :: :ok | {:error, atom()}
  defp validate_can_grant_permission(granter, _permission) when granter.role == :admin, do: :ok

  defp validate_can_grant_permission(granter, _permission) when granter.role == :manager, do: :ok

  defp validate_can_grant_permission(granter, _permission) do
    if has_permission?(granter, "can_give_permissions") do
      :ok
    else
      {:error, :insufficient_permissions}
    end
  end

  @spec validate_hierarchy(Employee.t(), Employee.t()) :: :ok | {:error, atom()}
  defp validate_hierarchy(granter, _employee) when granter.role == :admin, do: :ok

  defp validate_hierarchy(granter, employee)
       when granter.role == :manager and employee.role in [:staff],
       do: :ok

  defp validate_hierarchy(granter, employee)
       when granter.role == "manager" and employee.role == "manager",
       do: {:error, :cannot_grant_to_peer}

  defp validate_hierarchy(granter, employee)
       when granter.role == "manager" and employee.role == "admin",
       do: {:error, :cannot_grant_to_superior}

  defp validate_hierarchy(granter, _employee) when granter.role == "staff",
    do: {:error, :insufficient_authority}

  @spec create_permission_assignment(String.t(), String.t(), String.t(), String.t() | nil) ::
        {:ok, EmployeePermission.t()} | {:error, any()}
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

  @spec find_employee_permission(String.t(), String.t()) ::
        {:ok, EmployeePermission.t()} | {:error, any()}
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

  @spec find_permission_assignment(String.t(), String.t()) ::
        {:ok, EmployeePermission.t()} | {:error, any()}
  defp find_permission_assignment(employee_id, permission_id) do
    find_employee_permission(employee_id, permission_id)
  end

  @spec handle_permission_assignment_query_result({:ok, EmployeePermission.t() | nil} | {:error, any()}) ::
        {:ok, EmployeePermission.t()} | {:error, any()}
  defp handle_permission_assignment_query_result({:ok, nil}), do: {:error, :permission_not_found}
  defp handle_permission_assignment_query_result({:ok, employee_permission}), do: {:ok, employee_permission}
  defp handle_permission_assignment_query_result(error), do: error

  @spec validate_can_revoke_permission(Employee.t(), EmployeePermission.t()) ::
        :ok | {:error, atom()}
  defp validate_can_revoke_permission(revoker, _employee_permission) when revoker.role == "admin",
    do: :ok

  defp validate_can_revoke_permission(revoker, employee_permission)
       when revoker.id == employee_permission.granted_by_id,
       do: :ok

  defp validate_can_revoke_permission(_revoker, _employee_permission),
    do: {:error, :insufficient_permissions}

  # Convenience functions using constants for common permission checks

  @doc """
  Check if employee can create reservations.
  """
  @spec can_create_reservations?(Employee.t() | String.t()) :: boolean()
  def can_create_reservations?(employee_or_id) do
    has_permission?(employee_or_id, Constants.can_create_reservations())
  end

  @doc """
  Check if employee can view all reservations.
  """
  @spec can_view_all_reservations?(Employee.t() | String.t()) :: boolean()
  def can_view_all_reservations?(employee_or_id) do
    has_permission?(employee_or_id, Constants.can_view_all_reservations())
  end

  @doc """
  Check if employee can modify reservations.
  """
  @spec can_modify_reservations?(Employee.t() | String.t()) :: boolean()
  def can_modify_reservations?(employee_or_id) do
    has_permission?(employee_or_id, Constants.can_modify_reservations())
  end

  @doc """
  Check if employee can update pricing.
  """
  @spec can_update_pricing?(Employee.t() | String.t()) :: boolean()
  def can_update_pricing?(employee_or_id) do
    has_permission?(employee_or_id, Constants.can_update_pricing())
  end

  @doc """
  Check if employee can manage employees.
  """
  @spec can_manage_employees?(Employee.t() | String.t()) :: boolean()
  def can_manage_employees?(employee_or_id) do
    has_permission?(employee_or_id, Constants.can_modify_employees())
  end

  @doc """
  Check if employee can give permissions to others.
  """
  @spec can_give_permissions?(Employee.t() | String.t()) :: boolean()
  def can_give_permissions?(employee_or_id) do
    has_permission?(employee_or_id, Constants.can_give_permissions())
  end

  @doc """
  Returns all available permission constants.
  Delegates to Constants module.
  """
  @spec all_permission_constants() :: list(String.t())
  def all_permission_constants, do: Constants.all_permissions()

  @doc """
  Returns permissions grouped by category.
  Delegates to Constants module.
  """
  @spec permissions_by_category() :: map()
  def permissions_by_category, do: Constants.permissions_by_category()

  @doc """
  Validates that a permission name is valid.
  Delegates to Constants module.
  """
  @spec valid_permission?(String.t()) :: boolean()
  def valid_permission?(permission), do: Constants.valid_permission?(permission)
end
