defmodule RivaAsh.Permission.PermissionService do
  @moduledoc """
  Service module for managing Permissions business logic.
  This module encapsulates all business logic related to permissions,
  including employee permissions and permission management,
  keeping it separate from the LiveView UI concerns.
  """

  require Logger

  alias RivaAsh.Repo
  alias RivaAsh.Resources.{Permission, Employee, EmployeePermission}
  alias Ash.Query

  @doc """
  Get all permissions data for the current user.
  """
  def get_user_permissions_data(user) do
    try do
      # Get employees for user's business
      employees = Employee.read!(actor: user, filter: [business_id: user.business_id])

      # Get all permissions
      permissions = Permission.read!(actor: user)

      # Get meta data for pagination
      meta = %{
        total_count: length(employees),
        page_size: 20,
        current_page: 1,
        total_pages: ceil(length(employees) / 20)
      }

      {:ok, {employees, permissions, meta}}
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        Logger.error("Failed to get user permissions data: #{inspect(error)}")
        {:error, :forbidden}

      error ->
        Logger.error("Unexpected error in get_user_permissions_data: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Get employee permissions.
  """
  def get_employee_permissions(employee_id, user) do
    try do
      # Get employee with permissions
      case Ash.get(Employee, employee_id, authorize?: true) do
        {:ok, employee} ->
          if employee.business_id == user.business_id do
            # Get all permissions
            permissions = Permission.read!(actor: user)

            # Get employee's current permissions
            employee_permissions =
              EmployeePermission.read!(
                actor: user,
                filter: [employee_id: employee_id]
              )

            current_permission_ids = Enum.map(employee_permissions, & &1.permission_id)

            {:ok, {employee, permissions, current_permission_ids}}
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get employee: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
        error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
          Logger.error("Failed to get employee permissions: #{inspect(error)}")
          {:error, :forbidden}

        error ->
          Logger.error("Unexpected error in get_employee_permissions: #{inspect(error)}")
          {:error, :unexpected_error}
    end
  end

  @doc """
  Save employee permissions.
  """
  def save_employee_permissions(employee_id, permission_ids, user) do
    try do
      with {:ok, employee} <- Ash.get(Employee, employee_id, authorize?: true),
           true <- employee.business_id == user.business_id || {:error, :forbidden},
           :ok <- remove_existing_permissions(employee_id, user),
           :ok <- add_new_permissions(employee_id, permission_ids, user) do
        {:ok, employee}
      else
        {:error, reason} ->
          Logger.error("Failed to get employee for permission update: #{inspect(reason)}")
          {:error, reason}

        {:error, :forbidden} ->
          {:error, :forbidden}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        Logger.error("Failed to save employee permissions: #{inspect(error)}")
        {:error, :forbidden}

      error ->
        Logger.error("Unexpected error in save_employee_permissions: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  # Private helper functions
  defp remove_existing_permissions(employee_id, user) do
    existing_permissions =
      EmployeePermission.read!(
        actor: user,
        filter: [employee_id: employee_id]
      )

    Enum.each(existing_permissions, fn perm ->
      case Ash.destroy(perm, authorize?: true) do
        {:ok, _} -> :ok
        {:error, reason} -> Logger.error("Failed to remove existing permission: #{inspect(reason)}")
      end
    end)

    :ok
  end

  defp add_new_permissions(employee_id, permission_ids, user) do
    Enum.each(permission_ids, fn permission_id ->
      case EmployeePermission.create(
             attributes: %{
               employee_id: employee_id,
               permission_id: permission_id
             },
             authorize?: true
           ) do
        {:ok, _} -> :ok
        {:error, reason} -> Logger.error("Failed to add permission: #{inspect(reason)}")
      end
    end)

    :ok
  end

  @doc """
  Create a new permission.
  """
  def create_permission(attrs, user) do
    try do
      case Ash.create(Permission,
             attributes:
               Map.merge(attrs, %{
                 business_id: user.business_id,
                 status: :active
               }),
             authorize?: true
           ) do
        {:ok, permission} ->
          {:ok, permission}

        {:error, reason} ->
          Logger.error("Failed to create permission: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in create_permission: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Update an existing permission.
  """
  def update_permission(id, attrs, user) do
    try do
      case Ash.get(Permission, id, authorize?: true) do
        {:ok, permission} ->
          if permission.business_id == user.business_id do
            case Ash.update(permission, attributes: attrs, authorize?: true) do
              {:ok, updated_permission} ->
                {:ok, updated_permission}

              {:error, reason} ->
                Logger.error("Failed to update permission: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get permission for update: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in update_permission: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Delete a permission.
  """
  def delete_permission(id, user) do
    try do
      case Ash.get(Permission, id, authorize?: true) do
        {:ok, permission} ->
          if permission.business_id == user.business_id do
            # First remove any employee permissions associated with this permission
            employee_permissions =
              EmployeePermission.read!(
                actor: user,
                filter: [permission_id: id]
              )

            Enum.each(employee_permissions, fn perm ->
              case Ash.destroy(perm, authorize?: true) do
                {:ok, _} ->
                  :ok

                {:error, reason} ->
                  Logger.error("Failed to remove employee permission: #{inspect(reason)}")
              end
            end)

            # Now delete the permission
            case Ash.destroy(permission, authorize?: true) do
              {:ok, deleted_permission} ->
                {:ok, deleted_permission}

              {:error, reason} ->
                Logger.error("Failed to delete permission: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get permission for deletion: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in delete_permission: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Delete an employee.
  """
  def delete_employee(id, user) do
    try do
      case Ash.get(Employee, id, authorize?: true) do
        {:ok, employee} ->
          if employee.business_id == user.business_id do
            # First remove any permissions associated with this employee
            employee_permissions =
              EmployeePermission.read!(
                actor: user,
                filter: [employee_id: id]
              )

            Enum.each(employee_permissions, fn perm ->
              case Ash.destroy(perm, authorize?: true) do
                {:ok, _} ->
                  :ok

                {:error, reason} ->
                  Logger.error("Failed to remove employee permission: #{inspect(reason)}")
              end
            end)

            # Now delete the employee
            case Ash.destroy(employee, authorize?: true) do
              {:ok, deleted_employee} ->
                {:ok, deleted_employee}

              {:error, reason} ->
                Logger.error("Failed to delete employee: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get employee for deletion: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in delete_employee: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Get a single permission by ID.
  """
  def get_permission(id, user) do
    try do
      case Ash.get(Permission, id, authorize?: true) do
        {:ok, permission} ->
          if permission.business_id == user.business_id do
            {:ok, permission}
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get permission: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_permission: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Validate permission attributes.
  """
  def validate_permission_attrs(attrs) do
    errors =
      []
      |> validate_name(attrs)
      |> validate_business_id(attrs)
      |> validate_module(attrs)
      |> validate_action(attrs)
      |> validate_description(attrs)

    case errors do
      [] -> {:ok, attrs}
      errors -> {:error, errors}
    end
  end

  # Helper functions for validation
  defp validate_name(errors, %{name: name}) when is_binary(name) and byte_size(name) > 0, do: errors
  defp validate_name(errors, _), do: ["Name is required" | errors]

  defp validate_business_id(errors, %{business_id: business_id}) when is_binary(business_id) and byte_size(business_id) > 0, do: errors
  defp validate_business_id(errors, _), do: ["Business ID is required" | errors]

  defp validate_module(errors, %{module: module}) when is_binary(module) and byte_size(module) > 0, do: errors
  defp validate_module(errors, _), do: errors

  defp validate_action(errors, %{action: action}) when is_binary(action) and byte_size(action) > 0, do: errors
  defp validate_action(errors, _), do: errors

  defp validate_description(errors, %{description: description}) when is_binary(description) and byte_size(description) <= 2000, do: errors
  defp validate_description(errors, %{description: description}) when is_binary(description), do: ["Description must be less than 2000 characters" | errors]
  defp validate_description(errors, _), do: errors

  @doc """
  Check if permission name is already taken in the business.
  """
  def name_taken?(name, business_id) do
    query =
      Permission
      |> Query.filter(business_id: business_id, name: name)
      |> Query.limit(1)

    try do
      case Ash.read(query) do
        {:ok, []} ->
          false

        {:ok, [_ | _]} ->
          true

        {:error, reason} ->
          Logger.error("Failed to check name uniqueness: #{inspect(reason)}")
          false
      end
    rescue
      error ->
        Logger.error("Unexpected error in name_taken?: #{inspect(error)}")
        false
    end
  end

  @doc """
  Get permission statistics for a business.
  """
  def get_permission_stats(business_id) do
    try do
      # Get total count
      total_count_query =
        Permission
        |> Query.filter(business_id: business_id)
        |> Query.aggregate(:count, :id)

      # Get active count
      active_count_query =
        Permission
        |> Query.filter(business_id: business_id, status: :active)
        |> Query.aggregate(:count, :id)

      # Get inactive count
      inactive_count_query =
        Permission
        |> Query.filter(business_id: business_id, status: :inactive)
        |> Query.aggregate(:count, :id)

      # Get total assigned permissions
      total_assigned_query =
        EmployeePermission
        |> Query.filter(employee: [business_id: business_id])
        |> Query.aggregate(:count, :id)

      with {:ok, [%{count: total_count}]} <- Ash.read(total_count_query),
           {:ok, [%{count: active_count}]} <- Ash.read(active_count_query),
           {:ok, [%{count: inactive_count}]} <- Ash.read(inactive_count_query),
           {:ok, [%{count: total_assigned}]} <- Ash.read(total_assigned_query) do
         stats = %{
           total_count: total_count,
           active_count: active_count,
           inactive_count: inactive_count,
           total_assigned: total_assigned
         }

         {:ok, stats}
       else
         {:error, reason} ->
           Logger.error("Failed to get permission stats: #{inspect(reason)}")
           {:error, reason}
       end
    rescue
      error ->
        Logger.error("Unexpected error in get_permission_stats: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Check if an employee has a specific permission.
  """
  def has_permission?(employee_id, permission_id, user) do
    try do
      query =
        EmployeePermission
        |> Query.filter(employee_id: employee_id, permission_id: permission_id)

      case Ash.read(query) do
        {:ok, []} ->
          false

        {:ok, [_ | _]} ->
          true

        {:error, reason} ->
          Logger.error("Failed to check permission: #{inspect(reason)}")
          false
      end
    rescue
      error ->
        Logger.error("Unexpected error in has_permission?: #{inspect(error)}")
        false
    end
  end

  @doc """
  Get all permissions for an employee.
  """
  def get_employee_permissions_list(employee_id, user) do
    try do
      query =
        EmployeePermission
        |> Query.filter(employee_id: employee_id)
        |> Query.load([:permission])

      case Ash.read(query) do
        {:ok, permissions} ->
          {:ok, permissions}

        {:error, reason} ->
          Logger.error("Failed to get employee permissions: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_employee_permissions_list: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end
end
