defmodule RivaAsh.Resources.AvailabilityException do
  @moduledoc """
  Represents exceptions to regular item schedules.
  Used for holidays, maintenance, special events, or one-time availability changes.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshArchival.Resource]

  postgres do
    table("availability_exceptions")
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
    type("availability_exception")

    routes do
      base("/availability-exceptions")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for exception-specific actions
      get(:by_item, route: "/by-item/:item_id")
      get(:by_date, route: "/by-date/:date")
      get(:upcoming, route: "/upcoming")
      get(:active, route: "/active")
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_item, args: [:item_id], action: :by_item)
    define(:by_date, args: [:date], action: :by_date)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:item_id, :date, :start_time, :end_time, :is_available, :reason, :exception_type])
      primary?(true)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_item do
      argument(:item_id, :uuid, allow_nil?: false)
      filter(expr(item_id == ^arg(:item_id)))
    end

    read :by_date do
      argument(:date, :date, allow_nil?: false)
      filter(expr(date == ^arg(:date)))
    end

    read :upcoming do
      today = Date.utc_today()
      filter(expr(date >= ^today))
    end

    read :active do
      today = Date.utc_today()
      now = Time.utc_now()

      filter(
        expr(
          date == ^today and
            start_time <= ^now and
            end_time >= ^now
        )
      )
    end

    read :unavailable_exceptions do
      filter(expr(is_available == false))
    end

    read :available_exceptions do
      filter(expr(is_available == true))
    end

    read :by_type do
      argument(:exception_type, :atom, allow_nil?: false)
      filter(expr(exception_type == ^arg(:exception_type)))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :date, :date do
      allow_nil?(false)
      public?(true)
      description("The specific date this exception applies to")
    end

    attribute :start_time, :time do
      allow_nil?(true)
      public?(true)
      description("Start time for this exception (null means all day)")
    end

    attribute :end_time, :time do
      allow_nil?(true)
      public?(true)
      description("End time for this exception (null means all day)")
    end

    attribute :is_available, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether the item is available (true) or blocked (false) during this exception")
    end

    attribute :reason, :string do
      allow_nil?(true)
      public?(true)
      description("Reason for this availability exception")
    end

    attribute :exception_type, :atom do
      constraints(
        one_of: [:holiday, :maintenance, :special_event, :closure, :extended_hours, :other]
      )

      default(:other)
      public?(true)
      description("Type of exception")
    end

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Additional notes about this exception")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :item, RivaAsh.Resources.Item do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The item this exception applies to")
    end
  end

  validations do
    # Time range validation (when both times are present)
    validate(&RivaAsh.Validations.validate_time_range/2)

    # Date validation - should not be in the past for future exceptions
    validate(&RivaAsh.Validations.validate_future_date/2)

    # Required fields
    validate(present([:item_id, :date, :exception_type]),
      message: "Item, date, and exception type are required"
    )

    # Logical validation - if it's a closure, times are optional
    # If it's extended hours, times should be present
    validate(present([:start_time, :end_time]),
      where: [attribute_equals(:exception_type, :extended_hours)],
      message: "Start and end times are required for extended hours exceptions"
    )
  end

  identities do
    identity(:unique_item_date_time, [:item_id, :date, :start_time, :end_time])
  end
end
