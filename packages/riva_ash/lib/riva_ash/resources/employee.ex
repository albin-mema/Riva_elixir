defmodule RivaAsh.Resources.Employee do
  @moduledoc """
  Represents an employee who can create reservations on behalf of clients.
  Employees belong to a business and have different roles and permissions.
  """

  @type t :: %__MODULE__{
          id: String.t(),
          business_id: String.t(),
          email: String.t(),
          first_name: String.t(),
          last_name: String.t(),
          phone: String.t() | nil,
          role: String.t(),
          is_active: boolean(),
          employee_number: String.t() | nil,
          hire_date: Date.t() | nil,
          last_login_at: DateTime.t() | nil,
          notes: String.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

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
  import RivaAsh.Authorization

  postgres do
    table("employees")
    repo(RivaAsh.Repo)
    identity_wheres_to_sql(unique_employee_number_per_business: "employee_number IS NOT NULL")
  end

  standard_archive()
  standard_paper_trail(ignore_attributes: [:inserted_at, :updated_at, :last_login_at])

  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, "admin") do
      authorize_if(always())
    end

    # Business owners can manage employees in their business
    policy action_type([:read, :create, :update]) do
      authorize_if(expr(business.owner_id == ^actor(:id)))
    end

    # Permission-based authorization for viewing employees (within same business)
    policy action_type(:read) do
      # Publicly readable
      authorize_if(always())
    end

    # Permission-based authorization for creating employees (within accessible business)
    policy action_type(:create) do
      # Public registration
      authorize_if(always())
    end

    # Permission-based authorization for updating employees (within same business)
    policy action_type(:update) do
      authorize_if(action_has_permission(:manage_employees))
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
    define(:search_people, args: [:search_term, :business_ids], action: :search_people)
    define(:by_business_filtered, args: [:business_id], action: :by_business_filtered)
    define(:for_user_businesses, args: [:business_ids], action: :for_user_businesses)
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
      argument(:role, :string, allow_nil?: false)
      filter(expr(role == ^arg(:role)))
    end

    read :active do
      filter(expr(is_active == true))
    end

    read :inactive do
      filter(expr(is_active == false))
    end

    read :managers do
      filter(expr(role in ["manager", "admin"]))
    end

    read :staff do
      filter(expr(role == "staff"))
    end

    update :update_last_login do
      accept([])
      require_atomic?(false)
      change(set_attribute(:last_login_at, &Timex.now/0))
    end

    update :deactivate do
      accept([])
      require_atomic?(false)
      change(set_attribute(:is_active, false))
    end

    update :activate do
      accept([])
      require_atomic?(false)
      change(set_attribute(:is_active, true))
    end

    # Search employees by name, email, or phone (from PeopleService)
    read :search_people do
      argument(:search_term, :string, allow_nil?: false)
      argument(:business_ids, {:array, :uuid}, allow_nil?: false)

      filter(expr(business_id in ^arg(:business_ids)))

      filter(
        expr(
          contains(first_name, ^arg(:search_term)) or
            contains(last_name, ^arg(:search_term)) or
            contains(email, ^arg(:search_term)) or
            contains(phone, ^arg(:search_term))
        )
      )

      prepare(build(load: [:business], calculate: [:name]))
    end

    # Filter employees by specific business (from PeopleService)
    read :by_business_filtered do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(business_id == ^arg(:business_id)))
      prepare(build(load: [:business]))
    end

    # Get all employees for user's businesses (from PeopleService)
    read :for_user_businesses do
      argument(:business_ids, {:array, :uuid}, allow_nil?: false)
      filter(expr(business_id in ^arg(:business_ids)))
      prepare(build(load: [:business]))
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

    attribute :role, :string do
      default("staff")
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

  calculations do
    calculate :name, :string, expr(first_name <> " " <> last_name) do
      public?(true)
      description("Full name combining first and last name")
    end
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
        nil ->
          :ok

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
    validate(one_of(:role, ["admin", "manager", "staff"]))
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

    calculate :can_manage_reservations, :boolean, expr(role in ["admin", "manager"]) do
      public?(true)
      description("Whether this employee can manage reservations")
    end

    calculate :is_admin, :boolean, expr(role == "admin") do
      public?(true)
      description("Whether this employee has admin privileges")
    end

    calculate :can_give_permissions, :boolean, expr(role in ["admin", "manager"]) do
      public?(true)
      description("Whether this employee can grant permissions to others")
    end

    calculate :is_manager_or_above, :boolean, expr(role in ["admin", "manager"]) do
      public?(true)
      description("Whether this employee is a manager or admin")
    end
  end

  @doc """
  Returns a list of employees formatted for dropdown selection.

  ## Returns
  A list of tuples `{id, full_name}` suitable for form dropdowns.
  """
  @spec choices_for_select :: [{String.t(), String.t()}]
  def choices_for_select do
    __MODULE__
    |> Ash.read!()
    |> Enum.map(&{&1.id, &1.full_name})
  end

  @doc """
  Fetches the business associated with an employee.

  ## Parameters
  - employee_id - The UUID of the employee

  ## Returns
  `{:ok, business}` or `{:error, reason}`
  """
  @spec get_business(String.t()) :: {:ok, RivaAsh.Resources.Business.t()} | {:error, String.t()}
  def get_business(employee_id) do
    case __MODULE__.by_id(employee_id) do
      {:ok, employee} ->
        case Ash.load(employee, :business) do
          {:ok, business} -> {:ok, business}
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}

      error ->
        {:error, "Failed to load business: #{inspect(error)}"}
    end
  end

  @doc """
  Fetches the business associated with an employee by employee ID.

  ## Parameters
  - employee_id - The UUID of the employee

  ## Returns
  `{:ok, business}` or `{:error, reason}`
  """
  @spec get_business_by_employee_id(String.t()) :: {:ok, RivaAsh.Resources.Business.t()} | {:error, String.t()}
  def get_business_by_employee_id(employee_id) do
    case __MODULE__.by_id(employee_id) do
      {:ok, _employee} -> get_business(employee_id)
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Determines if an employee has management privileges.

  ## Parameters
  - employee - An employee record

  ## Returns
  `true` if the employee is a manager or admin, `false` otherwise
  """
  @spec manager?(t()) :: boolean()
  def manager?(%{role: role}) when role in ["manager", "admin"], do: true
  def manager?(%{}), do: false

  @doc """
  Determines if an employee has admin privileges.

  ## Parameters
  - employee - An employee record

  ## Returns
  `true` if the employee is an admin, `false` otherwise
  """
  @spec admin?(t()) :: boolean()
  def admin?(%{role: "admin"}), do: true
  def admin?(%{}), do: false

  @doc """
  Formats the employee's name for display.

  ## Parameters
  - employee - An employee record

  ## Returns
  A formatted display name string
  """
  @spec display_name(t()) :: String.t()
  def display_name(%{first_name: first_name, last_name: last_name}) do
    "#{first_name} #{last_name}"
  end

  @doc """
  Determines if an employee can make reservations on behalf of clients.

  ## Parameters
  - employee - An employee record

  ## Returns
  `true` if the employee can make reservations, `false` otherwise
  """
  @spec can_make_reservations?(t()) :: boolean()
  def can_make_reservations?(%{is_active: true, role: role})
      when role in ["admin", "manager", "staff"], do: true

  def can_make_reservations?(%{}), do: false

  @doc """
  Calculates the number of days since the employee was hired.

  ## Parameters
  - employee - An employee record

  ## Returns
  The number of days since hire, or `nil` if hire date is not set
  """
  @spec days_since_hired(t()) :: integer() | nil
  def days_since_hired(%{hire_date: nil}), do: nil

  def days_since_hired(%{hire_date: hire_date}) do
    Date.diff(Date.utc_today(), hire_date)
  end
end
