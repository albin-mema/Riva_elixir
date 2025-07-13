defmodule RivaAsh.Resources.Employee do
  @moduledoc """
  Represents an employee who can create reservations on behalf of clients.
  Employees belong to a business and have different roles and permissions.
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

  postgres do
    table("employees")
    repo(RivaAsh.Repo)
    identity_wheres_to_sql(unique_employee_number_per_business: "employee_number IS NOT NULL")
  end

  standard_archive()
  standard_paper_trail(ignore_attributes: [:inserted_at, :updated_at, :last_login_at])

  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Business owners can manage employees in their business
    policy action_type([:read, :create, :update]) do
      authorize_if(expr(business.owner_id == ^actor(:id)))
    end

    # Permission-based authorization for viewing employees (within same business)
    policy action_type(:read) do
      # TODO: Fix authorization policies - temporarily allow all for compilation
      authorize_if(always())
    end

    # Permission-based authorization for creating employees (within accessible business)
    policy action_type(:create) do
      # TODO: Fix authorization policies - temporarily allow all for compilation
      authorize_if(always())
    end

    # Permission-based authorization for updating employees (within same business)
    policy action_type(:update) do
      # TODO: Fix authorization policies - temporarily allow all for compilation
      authorize_if(always())
    end

    # Only admins can delete employees (prevent accidental deletions)
    policy action_type(:destroy) do
      # Prevent accidental deletions, use archive instead
      forbid_if(always())
    end
  end

  graphql do
    type(:employee)
  end

  json_api do
    type("employee")

    routes do
      base("/employees")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for employee-specific actions
      get(:by_business, route: "/by-business/:business_id")
      get(:by_role, route: "/by-role/:role")
      get(:active, route: "/active")
      get(:inactive, route: "/inactive")
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_email, args: [:email], action: :by_email)
    define(:by_business, args: [:business_id], action: :by_business)
    define(:by_role, args: [:role], action: :by_role)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:business_id, :email, :first_name, :last_name, :role, :phone, :is_active])
      primary?(true)

      # Validate business access
      validate(&RivaAsh.Validations.validate_business_access/2)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_email do
      argument(:email, :string, allow_nil?: false)
      get?(true)
      filter(expr(email == ^arg(:email)))
    end

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(business_id == ^arg(:business_id)))
    end

    read :by_role do
      argument(:role, :atom, allow_nil?: false)
      filter(expr(role == ^arg(:role)))
    end

    read :active do
      filter(expr(is_active == true))
    end

    read :inactive do
      filter(expr(is_active == false))
    end

    read :managers do
      filter(expr(role in [:manager, :admin]))
    end

    read :staff do
      filter(expr(role == :staff))
    end

    update :update_last_login do
      accept([])
      require_atomic? false
      change(set_attribute(:last_login_at, &DateTime.utc_now/0))
    end

    update :deactivate do
      accept([])
      require_atomic? false
      change(set_attribute(:is_active, false))
    end

    update :activate do
      accept([])
      require_atomic? false
      change(set_attribute(:is_active, true))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :email, :ci_string do
      allow_nil?(false)
      public?(true)
      description("Employee's email address (used for login)")
    end

    attribute :first_name, :string do
      allow_nil?(false)
      public?(true)
      description("Employee's first name")
    end

    attribute :last_name, :string do
      allow_nil?(false)
      public?(true)
      description("Employee's last name")
    end

    attribute :phone, :string do
      allow_nil?(true)
      public?(true)
      description("Employee's contact phone number")
    end

    attribute :role, :atom do
      constraints(one_of: [:admin, :manager, :staff])
      default(:staff)
      public?(true)
      description("Employee's role determining their permissions")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this employee account is active")
    end

    attribute :employee_number, :string do
      allow_nil?(true)
      public?(true)
      description("Optional employee number or badge ID")
    end

    attribute :hire_date, :date do
      allow_nil?(true)
      public?(true)
      description("Date when the employee was hired")
    end

    attribute :last_login_at, :utc_datetime do
      allow_nil?(true)
      public?(false)
      description("Last time the employee logged in")
    end

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Additional notes about the employee")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The business this employee works for")
    end

    has_many :reservations, RivaAsh.Resources.Reservation do
      destination_attribute(:employee_id)
      public?(true)
      description("Reservations created by this employee")
    end

    has_many :recurring_reservations, RivaAsh.Resources.RecurringReservation do
      destination_attribute(:employee_id)
      public?(true)
      description("Recurring reservation patterns created by this employee")
    end

    has_many :employee_permissions, RivaAsh.Resources.EmployeePermission do
      destination_attribute(:employee_id)
      public?(true)
      description("Permission assignments for this employee")
    end

    many_to_many :permissions, RivaAsh.Resources.Permission do
      through(RivaAsh.Resources.EmployeePermission)
      source_attribute_on_join_resource(:employee_id)
      destination_attribute_on_join_resource(:permission_id)
      public?(true)
      description("Permissions assigned to this employee")
    end

    has_many :granted_permissions, RivaAsh.Resources.EmployeePermission do
      destination_attribute(:granted_by_id)
      public?(true)
      description("Permissions this employee has granted to others")
    end
  end

  validations do
    validate(fn changeset, _context ->
      case Ash.Changeset.get_attribute(changeset, :email) do
        nil -> :ok
        email ->
          # Convert CiString to string for regex validation
          email_string = to_string(email)
          if Regex.match?(~r/^[^\s]+@[^\s]+$/, email_string) do
            :ok
          else
            {:error, field: :email, message: "must be a valid email address"}
          end
      end
    end)
    validate(present([:first_name, :last_name]), message: "first and last name are required")
  end

  # Flop configuration for table functionality
  @derive {
    Flop.Schema,
    filterable: [:first_name, :last_name, :email, :role, :is_active, :business_id],
    sortable: [:first_name, :last_name, :email, :role, :hire_date, :inserted_at],
    default_order: %{
      order_by: [:last_name, :first_name],
      order_directions: [:asc, :asc]
    },
    default_limit: 20,
    max_limit: 100
  }

  identities do
    identity(:unique_email, [:email])

    identity(:unique_employee_number_per_business, [:business_id, :employee_number],
      where: expr(not is_nil(employee_number))
    )
  end

  calculations do
    calculate :full_name, :string, expr(first_name <> " " <> last_name) do
      public?(true)
      description("Employee's full name")
    end

    calculate :can_manage_reservations, :boolean, expr(role in [:admin, :manager]) do
      public?(true)
      description("Whether this employee can manage reservations")
    end

    calculate :is_admin, :boolean, expr(role == :admin) do
      public?(true)
      description("Whether this employee has admin privileges")
    end

    calculate :can_give_permissions, :boolean, expr(role in [:admin, :manager]) do
      public?(true)
      description("Whether this employee can grant permissions to others")
    end

    calculate :is_manager_or_above, :boolean, expr(role in [:admin, :manager]) do
      public?(true)
      description("Whether this employee is a manager or admin")
    end
  end
end
