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
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [AshJsonApi.Resource]

  postgres do
    table("permissions")
    repo(RivaAsh.Repo)
  end

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
