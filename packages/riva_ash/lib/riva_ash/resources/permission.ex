defmodule RivaAsh.Resources.Permission do
  @moduledoc """
  Represents a permission that can be assigned to employees.
  Permissions define what actions an employee can perform in the system.

  Examples of permissions:
  - can_create_reservations
  - can_manage_employees
  - can_give_permissions
  - can_view_reports
  - can_manage_business_settings

  ## Type Specification

  ### Permission Record Structure
  ```elixir
  %__MODULE__{
    id: String.t(),
    name: String.t(),
    description: String.t(),
    category: :reservations | :employees | :business | :reports | :system,
    is_assignable: boolean(),
    inserted_at: DateTime.t(),
    updated_at: DateTime.t(),
    archived_at: DateTime.t() | nil
  }
  ```
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource,
      AshAdmin.Resource
    ]

  import RivaAsh.ResourceHelpers

  standard_postgres("permissions")
  standard_archive()
  standard_paper_trail()

  policies do
    # Only admins and managers can manage permissions
    policy action_type([:create, :update, :destroy]) do
      authorize_if(actor_attribute_equals(:role, :admin))
      authorize_if(actor_attribute_equals(:role, :manager))
    end

    # All authenticated employees can read permissions
    policy action_type(:read) do
      authorize_if(actor_present())
    end
  end

  graphql do
    type(:permission)
  end

  json_api do
    type("permission")

    routes do
      base("/permissions")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for permission-specific actions
      get(:by_category, route: "/by-category/:category")
      get(:assignable, route: "/assignable")
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_name, args: [:name], action: :by_name)
    define(:by_category, args: [:category], action: :by_category)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :description, :category, :is_assignable])
      primary?(true)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_name do
      argument(:name, :string, allow_nil?: false)
      get?(true)
      filter(expr(name == ^arg(:name)))
    end

    read :by_category do
      argument(:category, :atom, allow_nil?: false)
      filter(expr(category == ^arg(:category)))
    end

    read :assignable do
      filter(expr(is_assignable == true))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("Unique name of the permission (e.g., 'can_create_reservations')")
    end

    attribute :description, :string do
      allow_nil?(false)
      public?(true)
      description("Human-readable description of what this permission allows")
    end

    attribute :category, :atom do
      constraints(one_of: [:reservations, :employees, :business, :reports, :system])
      allow_nil?(false)
      public?(true)
      description("Category this permission belongs to for organization")
    end

    attribute :is_assignable, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this permission can be assigned to employees (some may be role-only)")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
    create_timestamp(:archived_at)
  end

  relationships do
    has_many :employee_permissions, RivaAsh.Resources.EmployeePermission do
      destination_attribute(:permission_id)
      public?(true)
      description("Employee assignments for this permission")
    end

    many_to_many :employees, RivaAsh.Resources.Employee do
      through(RivaAsh.Resources.EmployeePermission)
      source_attribute_on_join_resource(:permission_id)
      destination_attribute_on_join_resource(:employee_id)
      public?(true)
      description("Employees who have this permission")
    end

    # Helper functions for business logic and data validation

    @doc """
    Checks if the permission is currently active (not archived).

    ## Parameters
    - permission: The permission record to check

    ## Returns
    - `true` if the permission is active, `false` otherwise
    """
    @spec active?(t()) :: boolean()
    def active?(permission) do
      case permission do
        %{archived_at: nil} -> true
        _ -> false
      end
    end

    @doc """
    Checks if the permission is assignable to employees.

    ## Parameters
    - permission: The permission record to check

    ## Returns
    - `true` if the permission is assignable, `false` otherwise
    """
    @spec assignable?(t()) :: boolean()
    def assignable?(permission), do: permission.is_assignable

    @doc """
    Gets the category description as a human-readable string.

    ## Parameters
    - permission: The permission record

    ## Returns
    - String with category description
    """
    @spec category_description(t()) :: String.t()
    def category_description(permission) do
      case permission.category do
        :reservations -> "Reservations"
        :employees -> "Employees"
        :business -> "Business"
        :reports -> "Reports"
        :system -> "System"
        _ -> "Unknown"
      end
    end

    @doc """
    Gets the permission information as a formatted string.

    ## Parameters
    - permission: The permission record

    ## Returns
    - String with formatted permission information
    """
    @spec formatted_info(t()) :: String.t()
    def formatted_info(permission) do
      with true <- active?(permission),
           category_desc <- category_description(permission),
           assignable_str <- if(permission.is_assignable, do: "Assignable", else: "Role-only") do
        "#{permission.name} - #{permission.description} (#{category_desc}, #{assignable_str})"
      else
        false ->
          "Archived permission: #{permission.name}"
      end
    end

    @doc """
    Gets the employee count for this permission.

    ## Parameters
    - permission: The permission record

    ## Returns
    - Integer with employee count
    """
    @spec employee_count(t()) :: non_neg_integer()
    def employee_count(permission) do
      case permission.employee_permissions do
        nil -> 0
        employee_permissions when is_list(employee_permissions) -> length(employee_permissions)
        _ -> 0
      end
    end

    @doc """
    Checks if this permission has any employees assigned.

    ## Parameters
    - permission: The permission record to check

    ## Returns
    - `true` if employees are assigned, `false` otherwise
    """
    @spec has_employees_assigned?(t()) :: boolean()
    def has_employees_assigned?(permission) do
      employee_count(permission) > 0
    end

    @doc """
    Gets the employee names assigned to this permission.

    ## Parameters
    - permission: The permission record

    ## Returns
    - List of employee names
    """
    @spec assigned_employee_names(t()) :: [String.t()]
    def assigned_employee_names(permission) do
      case permission.employee_permissions do
        nil ->
          []

        employee_permissions when is_list(employee_permissions) ->
          employee_permissions
          |> Enum.map(fn ep ->
            case ep.employee do
              %{name: name} when is_binary(name) and name != "" -> name
              _ -> "Unknown employee"
            end
          end)
          |> Enum.uniq()

        _ ->
          []
      end
    end

    @doc """
    Validates that the permission has all required relationships.

    ## Parameters
    - permission: The permission record to validate

    ## Returns
    - `{:ok, permission}` if valid
    - `{:error, reason}` if invalid
    """
    @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
    def validate_relationships(permission) do
      cond do
        is_nil(permission.employee_permissions) ->
          {:error, "Employee permissions relationship is missing"}

        true ->
          {:ok, permission}
      end
    end

    @doc """
    Checks if the permission can be deleted.

    ## Parameters
    - permission: The permission record to check

    ## Returns
    - `true` if can be deleted, `false` otherwise
    """
    @spec can_delete?(t()) :: boolean()
    def can_delete?(permission) do
      not has_employees_assigned?(permission) and active?(permission)
    end

    @doc """
    Gets the deletion reason for a permission.

    ## Parameters
    - permission: The permission record to check

    ## Returns
    - String with deletion reason or nil if can be deleted
    """
    @spec deletion_reason(t()) :: String.t() | nil
    def deletion_reason(permission) do
      cond do
        not active?(permission) ->
          "Permission is already archived"

        has_employees_assigned?(permission) ->
          "Cannot delete permission with assigned employees"

        true ->
          nil
      end
    end

    @doc """
    Validates the permission data.

    ## Parameters
    - permission: The permission record to validate

    ## Returns
    - `{:ok, permission}` if valid
    - `{:error, reason}` if invalid
    """
    @spec validate_data(t()) :: {:ok, t()} | {:error, String.t()}
    def validate_data(permission) do
      cond do
        not String.match?(permission.name, ~r/^[a-z_]+$/) ->
          {:error, "Name must contain only lowercase letters and underscores"}

        String.trim(permission.name) == "" ->
          {:error, "Name cannot be empty"}

        String.trim(permission.description) == "" ->
          {:error, "Description cannot be empty"}

        not Enum.member?([:reservations, :employees, :business, :reports, :system], permission.category) ->
          {:error, "Invalid category"}

        true ->
          {:ok, permission}
      end
    end

    @doc """
    Gets the permission name with category prefix.

    ## Parameters
    - permission: The permission record

    ## Returns
    - String with formatted permission name
    """
    @spec prefixed_name(t()) :: String.t()
    def prefixed_name(permission) do
      "#{permission.category}: #{permission.name}"
    end

    @doc """
    Checks if the permission belongs to a specific category.

    ## Parameters
    - permission: The permission record to check
    - category: The category to check against

    ## Returns
    - `true` if belongs to category, `false` otherwise
    """
    @spec belongs_to_category?(t(), atom()) :: boolean()
    def belongs_to_category?(permission, category) do
      permission.category == category
    end

    @doc """
    Gets all permissions in a specific category.

    ## Parameters
    - permissions: List of permission records
    - category: The category to filter by

    ## Returns
    - List of permissions in the specified category
    """
    @spec by_category([t()], atom()) :: [t()]
    def by_category(permissions, category) do
      Enum.filter(permissions, &belongs_to_category?(&1, category))
    end

    @doc """
    Gets all assignable permissions.

    ## Parameters
    - permissions: List of permission records

    ## Returns
    - List of assignable permissions
    """
    @spec assignable([t()]) :: [t()]
    def assignable(permissions) do
      Enum.filter(permissions, &assignable?/1)
    end

    @doc """
    Gets all non-assignable (role-only) permissions.

    ## Parameters
    - permissions: List of permission records

    ## Returns
    - List of role-only permissions
    """
    @spec role_only([t()]) :: [t()]
    def role_only(permissions) do
      Enum.filter(permissions, &(!assignable?(&1)))
    end
  end

  identities do
    identity(:unique_name, [:name])
  end

  validations do
    validate(match(~r/^[a-z_]+$/, :name),
      message: "must contain only lowercase letters and underscores"
    )

    validate(present([:name, :description, :category]))
  end
end
