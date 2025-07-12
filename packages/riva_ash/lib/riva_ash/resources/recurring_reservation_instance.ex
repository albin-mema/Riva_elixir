defmodule RivaAsh.Resources.RecurringReservationInstance do
  @moduledoc """
  Represents an individual reservation instance generated from a recurring reservation pattern.
  Each instance corresponds to one day in the consecutive day pattern.

  This resource links to the actual Reservation that gets created for each day,
  allowing tracking of which reservations belong to a recurring pattern.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [
      AshJsonApi.Resource,
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
    table("recurring_reservation_instances")
    repo(RivaAsh.Repo)
  end

  # Configure soft delete functionality
  archive do
    # Use archived_at field for soft deletes
    attribute(:archived_at)
    # Allow both soft and hard deletes
    base_filter?(false)
  end

  json_api do
    type("recurring_reservation_instance")

    routes do
      base("/recurring-reservation-instances")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for instance-specific actions
      get(:by_recurring_reservation, route: "/by-recurring-reservation/:recurring_reservation_id")
      get(:by_date, route: "/by-date/:scheduled_date")
      get(:pending, route: "/pending")
      get(:confirmed, route: "/confirmed")
      get(:failed, route: "/failed")

      # Instance management routes
      # post :create_reservation, route: "/:id/create-reservation"
      patch(:skip, route: "/:id/skip")
      patch(:retry, route: "/:id/retry")
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)

    define(:by_recurring_reservation,
      args: [:recurring_reservation_id],
      action: :by_recurring_reservation
    )

    define(:by_date, args: [:scheduled_date], action: :by_date)
    # define :create_reservation, args: [:id], action: :create_reservation
  end

  actions do
    defaults([:read, :destroy])

    update :update do
      accept([
        :recurring_reservation_id,
        :scheduled_date,
        :sequence_number,
        :status,
        :notes,
        :skip_reason,
        :reservation_id,
        :error_message,
        :failed_at,
        :created_at
      ])
      primary?(true)
      require_atomic?(false)
    end

    create :create do
      accept([
        :recurring_reservation_id,
        :scheduled_date,
        :sequence_number,
        :status,
        :notes,
        :skip_reason
      ])

      primary?(true)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_recurring_reservation do
      argument(:recurring_reservation_id, :uuid, allow_nil?: false)
      filter(expr(recurring_reservation_id == ^arg(:recurring_reservation_id)))
    end

    read :by_date do
      argument(:scheduled_date, :date, allow_nil?: false)
      filter(expr(scheduled_date == ^arg(:scheduled_date)))
    end

    read :pending do
      filter(expr(status == "pending"))
    end

    read :confirmed do
      filter(expr(status == "confirmed"))
    end

    read :failed do
      filter(expr(status == "failed"))
    end

    read :skipped do
      filter(expr(status == "skipped"))
    end

    update :skip do
      accept([:skip_reason])
      change(set_attribute(:status, :skipped))
    end

    update :retry do
      accept([])
      change(set_attribute(:status, :pending))
      change(set_attribute(:error_message, nil))
      change(set_attribute(:failed_at, nil))
    end

    # Create the actual reservation for this instance
    # Use RivaAsh.RecurringReservations.process_instance/1 instead
  end

  attributes do
    uuid_primary_key(:id)

    attribute :scheduled_date, :date do
      allow_nil?(false)
      public?(true)
      description("The date this instance is scheduled for")
    end

    attribute :sequence_number, :integer do
      allow_nil?(false)
      public?(true)
      description("The sequence number in the recurring pattern (1, 2, 3, etc.)")
    end

    attribute :status, :atom do
      constraints(one_of: [:pending, :confirmed, :failed, :skipped, :cancelled])
      default(:pending)
      public?(true)
      description("Status of this individual instance")
    end

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Notes specific to this instance")
    end

    attribute :skip_reason, :string do
      allow_nil?(true)
      public?(true)
      description("Reason why this instance was skipped")
    end

    attribute :error_message, :string do
      allow_nil?(true)
      public?(true)
      description("Error message if reservation creation failed")
    end

    attribute :created_at, :utc_datetime do
      allow_nil?(true)
      public?(true)
      description("When the actual reservation was created for this instance")
    end

    attribute :failed_at, :utc_datetime do
      allow_nil?(true)
      public?(true)
      description("When this instance failed to create a reservation")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :recurring_reservation, RivaAsh.Resources.RecurringReservation do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The recurring reservation pattern this instance belongs to")
    end

    belongs_to :reservation, RivaAsh.Resources.Reservation do
      allow_nil?(true)
      attribute_writable?(true)
      public?(true)
      description("The actual reservation created for this instance (if successful)")
    end
  end

  validations do
    validate(compare(:sequence_number, greater_than: 0),
      message: "Sequence number must be positive"
    )
  end

  identities do
    identity(:unique_recurring_sequence, [:recurring_reservation_id, :sequence_number])
    identity(:unique_recurring_date, [:recurring_reservation_id, :scheduled_date])
  end

  calculations do
    calculate :is_past_due,
              :boolean,
              expr(scheduled_date < ^Date.utc_today() and status == "pending") do
      public?(true)
      description("Whether this instance is past its scheduled date and still pending")
    end

    calculate :days_until_scheduled,
              :integer,
              expr(date_diff(scheduled_date, ^Date.utc_today(), "day")) do
      public?(true)
      description("Number of days until this instance is scheduled (negative if past)")
    end
  end
end
