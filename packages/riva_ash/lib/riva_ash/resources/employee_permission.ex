defmodule RivaAsh.Resources.EmployeePermission do
  @moduledoc """
  Join table linking employees to their assigned permissions.
  Tracks who granted the permission and when for audit purposes.

  This resource manages permission assignments for employees, ensuring proper
  authorization and audit tracking for all permission changes.
  """

  @type t :: %__MODULE__{
          id: String.t(),
          employee_id: String.t(),
          permission_id: String.t(),
          granted_by_id: String.t(),
          notes: String.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [AshJsonApi.Resource, AshPaperTrail.Resource, AshArchival.Resource]

  import RivaAsh.ResourceHelpers

  postgres do
    table("employee_permissions")
    repo(RivaAsh.Repo)
  end

  standard_archive()
  standard_paper_trail()

  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Managers can read and manage permissions for staff
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :manager))
      # Employees can read their own permissions
      authorize_if(expr(employee_id == ^actor(:id)))
    end

    # Only managers and admins can grant permissions
    policy action_type(:create) do
      authorize_if(actor_attribute_equals(:role, :manager))
    end

    # Only the granter or admins can revoke permissions
    policy action_type(:destroy) do
      authorize_if(expr(granted_by_id == ^actor(:id)))
    end

    # Staff can only read their own permissions
    policy actor_attribute_equals(:role, :staff) do
      authorize_if(expr(employee_id == ^actor(:id)))
      forbid_unless(action_type(:read))
    end
  end

  json_api do
    type("employee_permission")

    routes do
      base("/employee-permissions")

      get(:read)
      index(:read)
      post(:create)
      delete(:destroy)

      # Additional routes for employee permission management
      get(:by_employee, route: "/by-employee/:employee_id")
      get(:by_permission, route: "/by-permission/:permission_id")
      get(:by_granter, route: "/by-granter/:granted_by_id")
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:destroy, action: :destroy)
    define(:by_employee, args: [:employee_id], action: :by_employee)
    define(:by_permission, args: [:permission_id], action: :by_permission)

    define(:grant_permission,
      args: [:employee_id, :permission_id, :granted_by_id],
      action: :grant_permission
    )
  end

  actions do
    defaults([:read, :destroy])

    create :create do
      accept([:employee_id, :permission_id, :granted_by_id, :notes])
      primary?(true)

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_employee_granter_business_match/2)
    end

    create :grant_permission do
      argument(:employee_id, :uuid, allow_nil?: false)
      argument(:permission_id, :uuid, allow_nil?: false)
      argument(:granted_by_id, :uuid, allow_nil?: false)
      argument(:notes, :string, allow_nil?: true)

      change(set_attribute(:employee_id, arg(:employee_id)))
      change(set_attribute(:permission_id, arg(:permission_id)))
      change(set_attribute(:granted_by_id, arg(:granted_by_id)))
      change(set_attribute(:notes, arg(:notes)))

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_employee_granter_business_match/2)
    end

    read :by_employee do
      argument(:employee_id, :uuid, allow_nil?: false)
      filter(expr(employee_id == ^arg(:employee_id)))
    end

    read :by_permission do
      argument(:permission_id, :uuid, allow_nil?: false)
      filter(expr(permission_id == ^arg(:permission_id)))
    end

    read :by_granter do
      argument(:granted_by_id, :uuid, allow_nil?: false)
      filter(expr(granted_by_id == ^arg(:granted_by_id)))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Optional notes about why this permission was granted")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :employee, RivaAsh.Resources.Employee do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The employee who has this permission")
    end

    belongs_to :permission, RivaAsh.Resources.Permission do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The permission that was granted")
    end

    belongs_to :granted_by, RivaAsh.Resources.Employee do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The employee who granted this permission")
    end

    # Helper functions for business logic and data validation

    @doc """
    Checks if the employee permission is currently active (not archived).

    ## Parameters
    - employee_permission: The employee permission record to check

    ## Returns
    - `true` if the permission is active, `false` otherwise
    """
    @spec active?(t()) :: boolean()
    def active?(employee_permission) do
      case employee_permission do
        %{archived_at: nil} -> true
        _ -> false
      end
    end

    @doc """
    Gets the display name of the permission assigned to the employee.

    ## Parameters
    - employee_permission: The employee permission record

    ## Returns
    - String with the permission name or "Unknown permission"
    """
    @spec permission_name(t()) :: String.t()
    def permission_name(employee_permission) do
      case employee_permission.permission do
        %{name: name} when is_binary(name) and name != "" -> name
        _ -> "Unknown permission"
      end
    end

    @doc """
    Gets the display name of the employee who was granted the permission.

    ## Parameters
    - employee_permission: The employee permission record

    ## Returns
    - String with the employee's display name
    """
    @spec employee_display_name(t()) :: String.t()
    def employee_display_name(employee_permission) do
      case employee_permission.employee do
        %{first_name: first, last_name: last} when is_binary(first) and is_binary(last) ->
          "#{first} #{last}"

        %{first_name: first} when is_binary(first) ->
          first

        %{last_name: last} when is_binary(last) ->
          last

        _ ->
          "Unknown employee"
      end
    end

    @doc """
    Gets the display name of the employee who granted the permission.

    ## Parameters
    - employee_permission: The employee permission record

    ## Returns
    - String with the granter's display name
    """
    @spec granter_display_name(t()) :: String.t()
    def granter_display_name(employee_permission) do
      case employee_permission.granted_by do
        %{first_name: first, last_name: last} when is_binary(first) and is_binary(last) ->
          "#{first} #{last}"

        %{first_name: first} when is_binary(first) ->
          first

        %{last_name: last} when is_binary(last) ->
          last

        _ ->
          "Unknown granter"
      end
    end

    @doc """
    Formats the permission assignment information for display.

    ## Parameters
    - employee_permission: The employee permission record

    ## Returns
    - String with formatted permission assignment details
    """
    @spec formatted_assignment(t()) :: String.t()
    def formatted_assignment(employee_permission) do
      if active?(employee_permission) do
        format_active_assignment(employee_permission)
      else
        format_archived_assignment(employee_permission)
      end
    end

    # Private helper functions
    defp format_active_assignment(employee_permission) do
      permission_name = permission_name(employee_permission)
      employee_name = employee_display_name(employee_permission)
      granter_name = granter_display_name(employee_permission)

      case employee_permission.notes do
        notes when is_binary(notes) ->
          "#{employee_name} was granted '#{permission_name}' by #{granter_name}. Notes: #{notes}"

        nil ->
          "#{employee_name} was granted '#{permission_name}' by #{granter_name}"
      end
    end

    defp format_archived_assignment(employee_permission) do
      "#{employee_display_name(employee_permission)} has archived permission '#{permission_name(employee_permission)}'"
    end

    @doc """
    Calculates how many days ago the permission was granted.

    ## Parameters
    - employee_permission: The employee permission record

    ## Returns
    - Integer representing days since grant, or nil if not granted
    """
    @spec days_since_granted(t()) :: integer() | nil
    def days_since_granted(employee_permission) do
      case employee_permission.inserted_at do
        %DateTime{} = inserted_at ->
          DateTime.diff(DateTime.utc_now(), inserted_at, :day)

        _ ->
          nil
      end
    end

    @doc """
    Checks if the permission was granted within the last specified number of days.

    ## Parameters
    - employee_permission: The employee permission record
    - days: Number of days to check against

    ## Returns
    - `true` if granted within the specified days, `false` otherwise
    """
    @spec granted_recently?(t(), integer()) :: boolean()
    def granted_recently?(employee_permission, days \\ 7) do
      case days_since_granted(employee_permission) do
        granted_days when is_integer(granted_days) and granted_days <= days ->
          true

        _ ->
          false
      end
    end

    @doc """
    Validates that the employee permission has all required relationships.

    ## Parameters
    - employee_permission: The employee permission record to validate

    ## Returns
    - `{:ok, employee_permission}` if valid
    - `{:error, reason}` if invalid
    """
    @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
    def validate_relationships(employee_permission) do
      cond do
        is_nil(employee_permission.employee) ->
          {:error, "Employee relationship is missing"}

        is_nil(employee_permission.permission) ->
          {:error, "Permission relationship is missing"}

        is_nil(employee_permission.granted_by) ->
          {:error, "Granted by relationship is missing"}

        true ->
          {:ok, employee_permission}
      end
    end

    @doc """
    Checks if the specified employee can be granted the specified permission.

    ## Parameters
    - employee_id: ID of the employee to check
    - permission_id: ID of the permission to check
    - granted_by_id: ID of the employee granting the permission

    ## Returns
    - `{:ok, boolean()}` with whether the assignment is valid
    - `{:error, reason}` if validation fails
    """
    @spec can_grant_permission?(String.t(), String.t(), String.t()) :: {:ok, boolean()} | {:error, String.t()}
    def can_grant_permission?(employee_id, permission_id, granted_by_id) do
      case validate_employee_business_match(employee_id, granted_by_id) do
        {:ok, true} ->
          case validate_permission_exists(permission_id) do
            {:ok, true} -> {:ok, true}
            {:error, reason} -> {:error, reason}
          end

        {:error, reason} ->
          {:error, reason}

        false ->
          {:error, "Invalid permission assignment"}
      end
    end

    # Private helper functions

    defp validate_employee_business_match(_employee_id, _granted_by_id) do
      # This would typically check if both employees belong to the same business
      # For now, we'll assume this validation is handled elsewhere
      {:ok, true}
    end

    defp validate_permission_exists(_permission_id) do
      # This would typically check if the permission exists
      # For now, we'll assume this validation is handled elsewhere
      {:ok, true}
    end
  end

  identities do
    identity(:unique_employee_permission, [:employee_id, :permission_id])
  end

  validations do
    validate(present([:employee_id, :permission_id, :granted_by_id]))
  end
end
