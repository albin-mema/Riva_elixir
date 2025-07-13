defmodule RivaAsh.Resources.ItemSchedule do
  @moduledoc """
  Represents recurring availability schedules for items.
  Defines when items are available on a weekly recurring basis.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshArchival.Resource]

  postgres do
    table("item_schedules")
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
    type("item_schedule")

    routes do
      base("/item-schedules")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for schedule-specific actions
      get(:by_item, route: "/by-item/:item_id")
      get(:by_day, route: "/by-day/:day_of_week")
      get(:available_schedules, route: "/available")
      get(:unavailable_schedules, route: "/unavailable")
      get(:active, route: "/active")
      get(:by_time_range, route: "/by-time-range/:start_time/:end_time")
      get(:weekdays, route: "/weekdays")
      get(:weekends, route: "/weekends")
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_item, args: [:item_id], action: :by_item)
    define(:by_day, args: [:day_of_week], action: :by_day)
    define(:available_schedules, action: :available_schedules)
    define(:unavailable_schedules, action: :unavailable_schedules)
    define(:active, action: :active)
    define(:by_time_range, args: [:start_time, :end_time], action: :by_time_range)
    define(:weekdays, action: :weekdays)
    define(:weekends, action: :weekends)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:item_id, :day_of_week, :start_time, :end_time, :is_available])
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

    read :by_day do
      argument(:day_of_week, :integer, allow_nil?: false)
      filter(expr(day_of_week == ^arg(:day_of_week)))
    end

    read :available_schedules do
      filter(expr(is_available == true))
    end

    read :unavailable_schedules do
      filter(expr(is_available == false))
    end

    read :active do
      # Non-archived schedules are considered active
      filter(expr(is_nil(archived_at)))
    end

    read :by_time_range do
      argument(:start_time, :time, allow_nil?: false)
      argument(:end_time, :time, allow_nil?: false)
      filter(expr(start_time >= ^arg(:start_time) and end_time <= ^arg(:end_time)))
    end

    read :weekdays do
      # Monday (1) through Friday (5) in 0-6 format
      filter(expr(day_of_week >= 1 and day_of_week <= 5))
    end

    read :weekends do
      # Saturday (6) and Sunday (0)
      filter(expr(day_of_week == 0 or day_of_week == 6))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :day_of_week, :integer do
      allow_nil?(false)
      public?(true)
      constraints(min: 0, max: 6)
      description("Day of week (0=Sunday, 1=Monday, ..., 6=Saturday)")
    end

    attribute :start_time, :time do
      allow_nil?(false)
      public?(true)
      description("Start time for this availability window")
    end

    attribute :end_time, :time do
      allow_nil?(false)
      public?(true)
      description("End time for this availability window")
    end

    attribute :is_available, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether the item is available (true) or blocked (false) during this time")
    end

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Optional notes about this schedule entry")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :item, RivaAsh.Resources.Item do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The item this schedule applies to")
    end
  end

  validations do
    # Time range validation
    validate(&RivaAsh.Validations.validate_time_range/2)

    # Day of week validation (0=Sunday, 1=Monday, ..., 6=Saturday)
    validate(compare(:day_of_week, greater_than_or_equal_to: 0),
      message: "Day of week must be between 0 and 6 (0=Sunday, 6=Saturday)")
    validate(compare(:day_of_week, less_than_or_equal_to: 6),
      message: "Day of week must be between 0 and 6 (0=Sunday, 6=Saturday)")

    # Required fields
    validate(present([:item_id, :day_of_week, :start_time, :end_time]),
      message: "All schedule fields are required")
  end

  calculations do
    calculate :is_weekday, :boolean, expr(day_of_week >= 1 and day_of_week <= 5) do
      public?(true)
      description("Whether this schedule is for a weekday (Monday-Friday)")
    end

    calculate :is_weekend, :boolean, expr(day_of_week == 0 or day_of_week == 6) do
      public?(true)
      description("Whether this schedule is for a weekend (Saturday-Sunday)")
    end

    calculate :day_type, :string, expr(
      if(day_of_week >= 1 and day_of_week <= 5, "weekday", "weekend")
    ) do
      public?(true)
      description("Day type as string: 'weekday' or 'weekend'")
    end

    calculate :day_name, :string, expr(
      cond do
        day_of_week == 0 -> "Sunday"
        day_of_week == 1 -> "Monday"
        day_of_week == 2 -> "Tuesday"
        day_of_week == 3 -> "Wednesday"
        day_of_week == 4 -> "Thursday"
        day_of_week == 5 -> "Friday"
        day_of_week == 6 -> "Saturday"
        true -> "Unknown"
      end
    ) do
      public?(true)
      description("Human-readable day name")
    end

    calculate :duration_minutes, :integer, expr(
      fragment("EXTRACT(EPOCH FROM (? - ?)) / 60", end_time, start_time)
    ) do
      public?(true)
      description("Duration of this schedule window in minutes")
    end
  end

  identities do
    identity(:unique_item_day_time, [:item_id, :day_of_week, :start_time, :end_time])
  end
end
