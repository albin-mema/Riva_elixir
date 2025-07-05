defmodule RivaAsh.Resources.Reservation do
  @moduledoc """
  Represents a reservation made by a client for an item.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource
    ]

  # Configure versioning for this resource
  paper_trail do
    # Track all changes with full diffs
    change_tracking_mode(:full_diff)

    # Don't store timestamps in the changes
    ignore_attributes([:inserted_at, :updated_at])

    # Store action name for better auditing
    store_action_name?(true)

    # Store action inputs for better auditing
    store_action_inputs?(true)

    # Store resource identifier for better querying
    store_resource_identifier?(true)

    # Create versions on destroy (for soft deletes)
    create_version_on_destroy?(true)
  end

  postgres do
    table("reservations")
    repo(RivaAsh.Repo)
  end

  # Configure soft delete functionality
  archive do
    # Use archived_at field for soft deletes
    attribute(:archived_at)
    # Allow both soft and hard deletes
    base_filter?(false)
  end

  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Managers can manage all reservations
    policy actor_attribute_equals(:role, :manager) do
      authorize_if(always())
    end

    # Staff can only read reservations they created
    policy actor_attribute_equals(:role, :staff) do
      authorize_if(expr(employee_id == ^actor(:id)))
      forbid_unless(action_type(:read))
    end

    # Clients can only access their own reservations
    policy actor_attribute_equals(:__struct__, RivaAsh.Resources.Client) do
      authorize_if(expr(client_id == ^actor(:id)))
      # Clients can read and update (for cancellation) their own reservations
      forbid_unless(action_type([:read, :update]))
    end

    # All authenticated employees can create reservations
    policy action_type(:create) do
      authorize_if(actor_present())
    end

    # Allow public booking creation (no actor required for client bookings)
    policy action_type(:create) do
      # This allows the booking API to create reservations without authentication
      # The booking controller handles the business logic
      authorize_if(always())
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
      accept([:client_id, :item_id, :employee_id, :reserved_from, :reserved_until, :notes, :total_amount])
      primary?(true)
    end

    # Create reservation for client booking (employee_id optional)
    create :create_for_booking do
      accept([:client_id, :item_id, :reserved_from, :reserved_until, :notes])
      change(set_attribute(:status, :pending))
      description("Create reservation from client booking flow")
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
      now = DateTime.utc_now()

      filter(
        expr(
          reserved_from <= ^now and
            reserved_until >= ^now and
            status == "confirmed"
        )
      )
    end

    read :upcoming do
      now = DateTime.utc_now()

      filter(
        expr(
          reserved_from > ^now and
            status in ["pending", "confirmed"]
        )
      )
    end

    read :past do
      now = DateTime.utc_now()

      filter(
        expr(
          reserved_until < ^now or
            status in ["completed", "cancelled"]
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

    # TODO: Re-enable reactor action once reactor syntax is fixed
    # action :create_with_validation, :struct do
    #   constraints instance_of: RivaAsh.Resources.Reservation
    #   argument :client_id, :uuid, allow_nil?: false
    #   argument :employee_id, :uuid, allow_nil?: false
    #   argument :item_id, :uuid, allow_nil?: false
    #   argument :start_datetime, :utc_datetime, allow_nil?: false
    #   argument :end_datetime, :utc_datetime, allow_nil?: false
    #   argument :notes, :string, allow_nil?: true
    #   run RivaAsh.Reactors.ReservationReactor
    # end
  end

  attributes do
    uuid_primary_key(:id)

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
      constraints(one_of: [:pending, :confirmed, :cancelled, :completed])
      default(:pending)
      public?(true)
      description("Current status of the reservation")
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

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
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

  validations do
    validate({RivaAsh.Validations.ReservationTimeSlot, []})
  end
end
