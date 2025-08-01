defmodule RivaAsh.Authorization do
  @moduledoc """
  Shared authorization utilities and permission checking functions.
  Provides consistent authorization patterns across all resources.
  """

  import Ash.Expr
  require Ash.Query

  @doc """
  Checks if an actor has a specific permission.

  ## Examples

      iex> RivaAsh.Authorization.has_permission(actor, :manage_reservations)
      true

      iex> RivaAsh.Authorization.has_permission(actor, :delete_business)
      false
  """
  def has_permission(actor, permission_name) do
    case actor do
      %{role: :admin} ->
        true

      %{role: :user} ->
        check_user_permission(actor, permission_name)

      %{id: employee_id} when is_binary(employee_id) ->
        check_employee_permission(employee_id, permission_name)

      _ ->
        false
    end
  end

  @doc """
  Checks if an actor can access a business's resources.
  """
  def can_access_business?(actor, business_id) do
    case actor do
      %{role: :admin} ->
        true

      %{current_business_id: ^business_id} ->
        true

      %{businesses: businesses} when is_list(businesses) ->
        Enum.any?(businesses, &(&1.id == business_id))

      %{id: user_id} when is_binary(user_id) ->
        # Check if user owns this business
        check_business_ownership(user_id, business_id)

      _ ->
        false
    end
  end

  @doc """
  Checks if an actor owns or manages a business.
  """
  def owns_business?(actor, business_id) do
    case actor do
      %{role: :admin} ->
        true

      %{id: user_id} when is_binary(user_id) ->
        check_business_ownership(user_id, business_id)

      _ ->
        false
    end
  end

  @doc """
  Standard policy for business-scoped resources.
  Use this as a base for resources that belong to a business.
  """
  defmacro business_scoped_policies do
    quote do
      # Admin bypass
      bypass actor_attribute_equals(:role, :admin) do
        authorize_if(always())
      end

      # Business owner has full access to their business data
      policy action_type([:read, :create, :update, :destroy]) do
        authorize_if(expr(business.owner_id == ^actor(:id)))
      end

      # Business context filtering - for resources with direct business_id
      policy action_type([:read, :create, :update]) do
        authorize_if(expr(business_id == ^actor(:current_business_id)))
      end
    end
  end

  @doc """
  Standard policy for employee-accessible resources.
  """
  defmacro employee_accessible_policies(_permission_name) do
    quote do
      # Employees with specific permission can read
      policy action_type(:read) do
        authorize_if(actor_attribute_equals(:role, :employee))
      end

      # Managers can create/update
      policy action_type([:create, :update]) do
        authorize_if(actor_attribute_equals(:role, :manager))
      end
    end
  end

  @doc """
  Helper macro for checking if an action has a specific permission.
  Used in Ash policies for permission-based authorization.
  """
  defmacro action_has_permission(permission_name) do
    quote do
      expr(^actor(:permissions) |> contains(unquote(permission_name)))
    end
  end

  @doc """
  Standard policy for client-owned resources.
  """
  defmacro client_owned_policies do
    quote do
      # Clients can only access their own data
      policy action_type(:read) do
        authorize_if(expr(client_id == ^actor(:id)))
      end

      policy action_type([:create, :update]) do
        authorize_if(expr(client_id == ^actor(:id)))
      end
    end
  end

  # Private helper functions

  defp check_user_permission(actor, permission_name) do
    # Regular users need to check if they own businesses and have permissions
    case actor do
      %{id: user_id} when is_binary(user_id) ->
        # Check if user owns any business and has business owner permissions
        if user_owns_any_business?(user_id) do
          check_business_owner_permission(actor, permission_name)
        else
          # Non-business owners need explicit employee permissions
          check_employee_permission(user_id, permission_name)
        end

      _ ->
        false
    end
  end

  defp check_business_owner_permission(actor, permission_name) do
    # Business owners have certain inherent permissions but not all
    case permission_name do
      # Core business management permissions - always allowed for business owners
      :manage_business_settings ->
        true

      :manage_plots_and_layouts ->
        true

      :manage_all_items ->
        true

      :set_pricing ->
        true

      :view_all_reports ->
        true

      :manage_employees ->
        true

      :can_manage_business_settings ->
        true

      :can_manage_plots_and_layouts ->
        true

      :can_manage_all_items ->
        true

      :can_set_pricing ->
        true

      :can_view_all_reports ->
        true

      :can_manage_employees ->
        true

      # Operational permissions - check if explicitly granted
      permission when is_atom(permission) ->
        check_explicit_business_owner_permission(actor, permission)

      permission when is_binary(permission) ->
        check_explicit_business_owner_permission(actor, String.to_atom(permission))

      # Default deny for unknown permissions
      _ ->
        false
    end
  end

  defp check_explicit_business_owner_permission(actor, permission_name) do
    # For non-core permissions, business owners need explicit grants
    # This could be extended to check a business_owner_permissions table
    # For now, we'll be restrictive and require explicit permission grants
    case actor do
      %{id: actor_id} when is_binary(actor_id) ->
        check_employee_permission(actor_id, permission_name)

      _ ->
        false
    end
  end

  defp user_owns_any_business?(user_id) do
    query =
      RivaAsh.Resources.Business
      |> Ash.Query.filter(expr(owner_id == ^user_id))
      |> Ash.Query.limit(1)

    query
    |> Ash.read(domain: RivaAsh.Domain)
    |> case do
      {:ok, []} -> false
      {:ok, _businesses} -> true
      {:error, _} -> false
    end
  end

  defp check_business_ownership(user_id, business_id) do
    business_id
    |> (&Ash.get(RivaAsh.Resources.Business, &1, domain: RivaAsh.Domain)).()
    |> case do
      {:ok, %{owner_id: ^user_id}} -> true
      _ -> false
    end
  end

  defp check_employee_permission(employee_id, permission_name) do
    employee_id
    |> (&Ash.get(RivaAsh.Resources.Employee, &1,
          domain: RivaAsh.Domain,
          load: [employee_permissions: :permission]
        )).()
    |> case do
      {:ok, nil} ->
        false

      {:ok, employee} ->
        # Check if the employee has the specific permission
        Enum.any?(employee.employee_permissions, fn ep ->
          ep.permission.name == permission_name and ep.is_active
        end)

      {:error, _} ->
        false
    end
  end
end
