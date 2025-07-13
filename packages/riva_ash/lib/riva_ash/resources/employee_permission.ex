defmodule RivaAsh.Resources.EmployeePermission do
  @moduledoc """
  Join table linking employees to their assigned permissions.
  Tracks who granted the permission and when for audit purposes.
  """

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
  end

  identities do
    identity(:unique_employee_permission, [:employee_id, :permission_id])
  end

  validations do
    validate(present([:employee_id, :permission_id, :granted_by_id]))
  end
end
