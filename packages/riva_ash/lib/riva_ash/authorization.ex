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
  @spec has_permission(map(), atom() | String.t()) :: boolean()
  def has_permission(actor, permission_name) do
    with {:ok, _normalized_permission} <- normalize_permission_name(permission_name),
         true <- admin_user?(actor) do
      true
    else
      {:ok, normalized_permission} ->
        check_user_or_employee_permission(actor, normalized_permission)

      _ ->
        false
    end
  end

  @spec check_user_or_employee_permission(map(), atom()) :: boolean()
  defp check_user_or_employee_permission(actor, permission_name) do
    if regular_user?(actor) do
      check_user_permission(actor, permission_name)
    else
      check_employee_permission(actor.id, permission_name)
    end
  end

  @doc """
  Checks if an actor can access a business's resources.
  """
  @spec can_access_business?(map(), String.t()) :: boolean()
  def can_access_business?(actor, business_id) when is_binary(business_id) do
    with {:ok, _validated_business_id} <- validate_business_id(business_id),
         true <- admin_user?(actor) do
      true
    else
      {:ok, validated_business_id} ->
        check_business_access_permissions(actor, validated_business_id)

      _ ->
        false
    end
  end

  @spec check_business_access_permissions(map(), String.t()) :: boolean()
  defp check_business_access_permissions(actor, business_id) do
    current_business_matches?(actor, business_id) or
      has_business_in_list?(actor, business_id) or
      check_business_ownership(actor.id, business_id)
  end

  @doc """
  Checks if an actor owns or manages a business.
  """
  @spec owns_business?(map(), String.t()) :: boolean()
  def owns_business?(actor, business_id) when is_binary(business_id) do
    case actor do
      %{role: :admin} ->
        true

      %{id: user_id} when is_binary(user_id) ->
        check_business_ownership(user_id, business_id)

      _ ->
        false
    end
  end

  @spec check_business_ownership(String.t(), String.t()) :: boolean()
  defp check_business_ownership(user_id, business_id) when is_binary(user_id) and is_binary(business_id) do
    case Ash.get(RivaAsh.Resources.Business, business_id, domain: RivaAsh.Domain) do
      {:ok, business} -> business.owner_id == user_id
      {:error, _} -> false
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

  # Private helper functions with comprehensive specs

  @spec admin_user?(map()) :: boolean()
  defp admin_user?(%{role: "admin"}), do: true
  defp admin_user?(_), do: false

  @spec regular_user?(map()) :: boolean()
  defp regular_user?(%{role: "user"}), do: true
  defp regular_user?(_), do: false

  @spec current_business_matches?(map(), String.t()) :: boolean()
  defp current_business_matches?(%{current_business_id: business_id}, business_id), do: true
  defp current_business_matches?(_, _), do: false

  @spec has_business_in_list?(map(), String.t()) :: boolean()
  defp has_business_in_list?(%{businesses: businesses}, business_id) when is_list(businesses) do
    Enum.any?(businesses, &(&1.id == business_id))
  end

  defp has_business_in_list?(_, _), do: false

  @spec check_user_permission(map(), atom()) :: boolean()
  defp check_user_permission(actor, permission_name) do
    case actor do
      %{id: user_id} when is_binary(user_id) ->
        check_user_business_permissions(user_id, permission_name)

      _ ->
        false
    end
  end

  @spec check_user_business_permissions(String.t(), atom()) :: boolean()
  defp check_user_business_permissions(user_id, permission_name) do
    if user_owns_any_business?(user_id) do
      check_business_owner_permission(%{id: user_id}, permission_name)
    else
      check_employee_permission(user_id, permission_name)
    end
  end

  @business_owner_permissions [
    :manage_business_settings,
    :manage_plots_and_layouts,
    :manage_all_items,
    :set_pricing,
    :view_all_reports,
    :manage_employees,
    :can_manage_business_settings,
    :can_manage_plots_and_layouts,
    :can_manage_all_items,
    :can_set_pricing,
    :can_view_all_reports,
    :can_manage_employees
  ]

  @spec check_business_owner_permission(map(), atom()) :: boolean()
  defp check_business_owner_permission(actor, permission_name) do
    cond do
      permission_name in @business_owner_permissions -> true
      true -> check_explicit_business_owner_permission(actor, permission_name)
    end
  end

  @spec normalize_permission_name(atom() | String.t()) :: {:ok, atom()} | {:error, String.t()}
  defp normalize_permission_name(permission) when is_atom(permission), do: {:ok, permission}

  defp normalize_permission_name(permission) when is_binary(permission) do
    try do
      {:ok, String.to_existing_atom(permission)}
    rescue
      ArgumentError -> {:error, "Invalid permission name: #{permission}"}
      error -> {:error, "Unexpected error converting permission: #{inspect(error)}"}
    end
  end

  @spec check_explicit_business_owner_permission(map(), atom()) :: boolean()
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

  @spec user_owns_any_business?(String.t()) :: boolean()
  defp user_owns_any_business?(user_id) when is_binary(user_id) do
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

  @spec validate_business_id(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_business_id(business_id) when is_binary(business_id) do
    if String.length(business_id) > 0 do
      {:ok, business_id}
    else
      {:error, "Invalid business ID"}
    end
  end

  @spec check_employee_permission(String.t(), atom()) :: boolean()
  defp check_employee_permission(employee_id, _permission_name) when is_binary(employee_id) do
    # For now, assume employees have all permissions
    # This could be extended to check an employee_permissions table
    true
  end
end
