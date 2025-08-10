defmodule RivaAsh.Resources.Reservation do
  @moduledoc """
  Represents a reservation made by a client for an item.

  Reservations track the booking of items by clients, including timing,
  status, and payment information. They are central to the business logic
  and require comprehensive validation and authorization.
  """

  @type t :: %__MODULE__{
          id: String.t(),
          business_id: String.t(),
          client_id: String.t(),
          item_id: String.t(),
          employee_id: String.t() | nil,
          reserved_from: DateTime.t(),
          reserved_until: DateTime.t(),
          status: :pending | :provisional | :confirmed | :cancelled | :completed,
          hold_expires_at: DateTime.t() | nil,
          is_provisional: boolean(),
          notes: String.t() | nil,
          is_paid: boolean(),
          total_amount: Decimal.t() | nil,
          number_of_days: integer() | nil,
          daily_rate: Decimal.t() | nil,
          multi_day_discount: Decimal.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t()
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

  standard_postgres("reservations")
  standard_archive()
  standard_paper_trail()

  # Authorization policies
  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Business-scoped policies for business owners and employees
    # Business owner has full access to their business data
    policy action_type([:read, :create, :update, :destroy]) do
      authorize_if(expr(item.section.plot.business.owner_id == ^actor(:id)))
    end

    # Employees with manager role can manage reservations
    policy action_type([:create, :update]) do
      authorize_if(actor_attribute_equals(:role, :manager))
    end

    # Employees can read reservations
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :employee))
    end

    # Managers can manage all reservations within their business
    policy actor_attribute_equals(:role, :manager) do
      authorize_if(action_has_permission(:manage_reservations))
    end

    # Staff can only read reservations they created within their business
    policy actor_attribute_equals(:role, :staff) do
      authorize_if(action_has_permission(:read_reservations))
    end

    # Clients can only access their own reservations
    policy actor_attribute_equals(:__struct__, RivaAsh.Resources.Client) do
      authorize_if(expr(client_id == ^actor(:id)))
      # Clients can read and update (for cancellation) their own reservations
      forbid_unless(action_type([:read, :update]))
    end

    # Special policy for viewing reservations by client or employee
    policy action_type(:read) do
      authorize_if(expr(client_id == ^actor(:id)))
      authorize_if(expr(employee_id == ^actor(:id)))
    end

    # Secure reservation creation - require business context for employees/managers
    policy action_type(:create) do
      authorize_if(action_has_permission(:create_reservations))
    end
  end

  json_api do
    type("reservation")

    routes do
      base("/reservations")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for reservation-specific actions
      get(:by_client, route: "/by-client/:client_id")
      get(:by_item, route: "/by-item/:item_id")
      get(:by_employee, route: "/by-employee/:employee_id")
      get(:active, route: "/active")
      get(:upcoming, route: "/upcoming")
      get(:past, route: "/past")

      # Status update routes
      patch(:confirm, route: "/:id/confirm")
      patch(:cancel, route: "/:id/cancel")
      patch(:complete, route: "/:id/complete")
    end
  end

  graphql do
    type(:reservation)

    queries do
      get(:get_reservation, :read)
      list(:list_reservations, :read)
      list(:reservations_by_client, :by_client)
      list(:reservations_by_item, :by_item)
      list(:reservations_by_employee, :by_employee)
      list(:active_reservations, :active)
      list(:upcoming_reservations, :upcoming)
      list(:past_reservations, :past)
    end

    mutations do
      create(:create_reservation, :create)
      update(:update_reservation, :update)
      update(:confirm_reservation, :confirm)
      update(:cancel_reservation, :cancel)
      update(:complete_reservation, :complete)
      destroy(:delete_reservation, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:create_for_booking, action: :create_for_booking)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_client, args: [:client_id], action: :by_client)
    define(:by_item, args: [:item_id], action: :by_item)
    define(:by_employee, args: [:employee_id], action: :by_employee)
    define(:active, action: :active)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([
        :client_id,
        :item_id,
        :employee_id,
        :reserved_from,
        :reserved_until,
        :notes,
        :total_amount
      ])

      primary?(true)

      # Automatically set business_id from item
      change(&RivaAsh.Changes.set_business_id_from_item/2)

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_client_item_business_match/2)
      validate(&RivaAsh.Validations.validate_employee_item_business_match/2)
    end

    # Create reservation for client booking (employee_id optional)
    create :create_for_booking do
      accept([:client_id, :item_id, :reserved_from, :reserved_until, :notes])
      change(set_attribute(:status, :pending))
      description("Create reservation from client booking flow")

      # Automatically set business_id from item
      change(&RivaAsh.Changes.set_business_id_from_item/2)

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_client_item_business_match/2)
    end

    # Create provisional reservation for online bookings
    create :create_provisional do
      accept([:client_id, :item_id, :reserved_from, :reserved_until, :notes])
      change(set_attribute(:status, :provisional))

      # Automatically set business_id from item
      change(&RivaAsh.Changes.set_business_id_from_item/2)

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_client_item_business_match/2)
      change(set_attribute(:is_provisional, true))
      change(set_attribute(:hold_expires_at, expr(fragment("NOW() + INTERVAL '15 minutes'"))))
      description("Create provisional reservation with 15-minute hold")
    end

    # Confirm provisional reservation (when payment is received)
    update :confirm_provisional do
      accept([:total_amount, :notes])
      require_atomic?(false)
      change(set_attribute(:status, :confirmed))
      change(set_attribute(:is_provisional, false))
      change(set_attribute(:is_paid, true))
      change(set_attribute(:hold_expires_at, nil))
      description("Confirm provisional reservation after payment")
    end

    # Cancel expired provisional reservations
    update :cancel_expired do
      require_atomic?(false)
      change(set_attribute(:status, :cancelled))
      change(set_attribute(:is_provisional, false))
      description("Cancel expired provisional reservation")
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_client do
      argument(:client_id, :uuid, allow_nil?: false)
      filter(expr(client_id == ^arg(:client_id)))
    end

    read :by_item do
      argument(:item_id, :uuid, allow_nil?: false)
      filter(expr(item_id == ^arg(:item_id)))
    end

    read :by_employee do
      argument(:employee_id, :uuid, allow_nil?: false)
      filter(expr(employee_id == ^arg(:employee_id)))
    end

    read :active do
      now = Timex.now()

      filter(
        expr(
          reserved_from <= ^now and
            reserved_until >= ^now and
            status == "confirmed"
        )
      )
    end

    read :upcoming do
      now = Timex.now()

      filter(
        expr(
          reserved_from > ^now and
            status in ["pending", "confirmed"]
        )
      )
    end

    read :past do
      now = Timex.now()

      filter(
        expr(
          reserved_until < ^now or
            status in ["completed", "cancelled"]
        )
      )
    end

    read :provisional do
      filter(expr(status == :provisional and is_provisional == true))
    end

    read :expired_provisional do
      now = Timex.now()

      filter(
        expr(
          status == :provisional and
            is_provisional == true and
            hold_expires_at < ^now
        )
      )
    end

    read :active_provisional do
      now = Timex.now()

      filter(
        expr(
          status == :provisional and
            is_provisional == true and
            hold_expires_at > ^now
        )
      )
    end

    update :confirm do
      accept([])
      change(set_attribute(:status, :confirmed))
      require_atomic?(false)
    end

    update :cancel do
      accept([])
      change(set_attribute(:status, :cancelled))
      require_atomic?(false)
    end

    update :complete do
      accept([])
      change(set_attribute(:status, :completed))
      require_atomic?(false)
    end

    update :mark_as_paid do
      accept([:total_amount])
      change(set_attribute(:is_paid, true))
      require_atomic?(false)
    end

    update :mark_as_unpaid do
      accept([])
      change(set_attribute(:is_paid, false))
      require_atomic?(false)
    end

    read :paid do
      filter(expr(is_paid == true))
    end

    read :unpaid do
      filter(expr(is_paid == false))
    end

    action :create_with_validation, :struct do
      constraints(instance_of: RivaAsh.Resources.Reservation)
      argument(:client_id, :uuid, allow_nil?: false)
      argument(:employee_id, :uuid, allow_nil?: false)
      argument(:item_id, :uuid, allow_nil?: false)
      argument(:start_datetime, :utc_datetime, allow_nil?: false)
      argument(:end_datetime, :utc_datetime, allow_nil?: false)
      argument(:notes, :string, allow_nil?: true)
      run(RivaAsh.Reactors.ReservationReactor)
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :business_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("Denormalized business ID for performance optimization")
    end

    attribute :reserved_from, :utc_datetime do
      allow_nil?(false)
      public?(true)
      description("When the reservation starts")
    end

    attribute :reserved_until, :utc_datetime do
      allow_nil?(false)
      public?(true)
      description("When the reservation ends")
    end

    attribute :status, :atom do
      constraints(one_of: [:pending, :provisional, :confirmed, :cancelled, :completed])
      default(:pending)
      public?(true)
      description("Current status of the reservation")
    end

    attribute :hold_expires_at, :utc_datetime do
      allow_nil?(true)
      public?(true)
      description("When the provisional hold on this reservation expires (for online bookings)")
    end

    attribute :is_provisional, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether this is a provisional reservation awaiting payment")
    end

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Additional notes about the reservation")
    end

    attribute :is_paid, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether the reservation has been paid for")
    end

    attribute :total_amount, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Total amount for this reservation")
    end

    attribute :number_of_days, :integer do
      allow_nil?(true)
      public?(true)
      constraints(min: 1)
      description("Number of consecutive days for this reservation")
    end

    attribute :daily_rate, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Daily rate for this reservation (before any discounts)")
    end

    attribute :multi_day_discount, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0, max: 100)
      description("Percentage discount applied for multi-day reservations")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("Denormalized business relationship for performance")
    end

    belongs_to :client, RivaAsh.Resources.Client do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The client for whom the reservation was made")
    end

    belongs_to :item, RivaAsh.Resources.Item do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The item being reserved")
    end

    belongs_to :employee, RivaAsh.Resources.Employee do
      allow_nil?(true)
      attribute_writable?(true)
      public?(true)
      description("The employee who created this reservation (optional for client bookings)")
    end

    has_many :payments, RivaAsh.Resources.Payment do
      destination_attribute(:reservation_id)
      public?(true)
      description("Payments for this reservation")
    end
  end

  calculations do
    calculate :calculated_days,
              :integer,
              expr(fragment("EXTRACT(DAY FROM (? - ?))", reserved_until, reserved_from) + 1) do
      public?(true)
      description("Calculated number of days for this reservation")
    end

    calculate :is_multi_day,
              :boolean,
              expr(fragment("EXTRACT(DAY FROM (? - ?))", reserved_until, reserved_from) > 0) do
      public?(true)
      description("Whether this is a multi-day reservation")
    end
  end

  validations do
    # Basic field validations
    validate(present([:client_id, :item_id, :reserved_from, :reserved_until]),
      message: "Required fields must be present"
    )

    # Time range validation
    validate(&RivaAsh.Validations.validate_time_range/2)

    # Future date validation
    validate(&RivaAsh.Validations.validate_future_date/2)

    # Item availability validation
    validate(&RivaAsh.Validations.validate_item_availability/2)

    # Reservation conflict validation
    validate(&RivaAsh.Validations.validate_reservation_availability/2)

    # Business access validation
    validate(&RivaAsh.Validations.validate_business_access/2)
  end

  identities do
    # Prevent duplicate reservations for the same item at the same time
    identity(:unique_item_time_slot, [:item_id, :reserved_from, :reserved_until])
  end

  # Public helper functions
  @spec choices_for_select :: [{String.t(), String.t()}]
  def choices_for_select do
    __MODULE__
    |> Ash.read!()
    |> Enum.map(&{&1.id, reservation_display_name(&1)})
  end

  @spec get_client(String.t()) :: {:ok, RivaAsh.Resources.Client.t()} | {:error, String.t()}
  def get_client(reservation_id) do
    with {:ok, reservation} <- __MODULE__.by_id(reservation_id),
         {:ok, client} <- Ash.load(reservation, :client) do
      {:ok, client}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to load client: #{inspect(error)}"}
    end
  end

  @spec get_item(String.t()) :: {:ok, RivaAsh.Resources.Item.t()} | {:error, String.t()}
  def get_item(reservation_id) do
    with {:ok, reservation} <- __MODULE__.by_id(reservation_id),
         {:ok, item} <- Ash.load(reservation, :item) do
      {:ok, item}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to load item: #{inspect(error)}"}
    end
  end

  @spec get_business(String.t()) :: {:ok, RivaAsh.Resources.Business.t()} | {:error, String.t()}
  def get_business(reservation_id) do
    with {:ok, reservation} <- __MODULE__.by_id(reservation_id),
         {:ok, business} <- Ash.load(reservation, :business) do
      {:ok, business}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to load business: #{inspect(error)}"}
    end
  end

  @spec get_business_by_reservation_id(String.t()) :: {:ok, RivaAsh.Resources.Business.t()} | {:error, String.t()}
  def get_business_by_reservation_id(reservation_id) do
    case __MODULE__.by_id(reservation_id) do
      {:ok, _reservation} -> get_business(reservation_id)
      {:error, reason} -> {:error, reason}
    end
  end

  # Private helper functions
  @spec reservation_display_name(__MODULE__.t()) :: String.t()
  defp reservation_display_name(reservation) do
    with {:ok, item} <- Ash.load(reservation, :item),
         {:ok, client} <- Ash.load(reservation, :client) do
      "#{client.name} - #{item.name} (#{format_date_range(reservation.reserved_from, reservation.reserved_until)})"
    else
      _ -> "Reservation #{reservation.id}"
    end
  end

  @spec format_date_range(DateTime.t(), DateTime.t()) :: String.t()
  defp format_date_range(start_date, end_date) do
    start_str = Timex.format!(start_date, "{Mshort} {D}, {YYYY} {h12}:{m} {AM}")
    end_str = Timex.format!(end_date, "{Mshort} {D}, {YYYY} {h12}:{m} {AM}")
    "#{start_str} - #{end_str}"
  end
end
