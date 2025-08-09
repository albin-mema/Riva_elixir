
defmodule RivaAsh.Resources.ItemSchedule do
  @moduledoc """
  Represents recurring availability schedules for items.
  Defines when items are available on a weekly recurring basis.

  This resource manages weekly recurring schedules for item availability,
  supporting both available and blocked time periods for each day of the week.
  """

  @type t :: %__MODULE__{
          id: String.t(),
          item_id: String.t(),
          day_of_week: integer(),
          start_time: Time.t(),
          end_time: Time.t(),
          is_available: boolean(),
          notes: String.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshArchival.Resource, AshPaperTrail.Resource]

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

  # Enable audit trail for this resource
  paper_trail do
    change_tracking_mode(:full_diff)
    ignore_attributes([:inserted_at, :updated_at])
    store_action_name?(true)
    store_action_inputs?(true)
    store_resource_identifier?(true)
    create_version_on_destroy?(true)
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
      message: "Day of week must be between 0 and 6 (0=Sunday, 6=Saturday)"
    )

    validate(compare(:day_of_week, less_than_or_equal_to: 6),
      message: "Day of week must be between 0 and 6 (0=Sunday, 6=Saturday)"
    )

    # Required fields
    validate(present([:item_id, :day_of_week, :start_time, :end_time]),
      message: "All schedule fields are required"
    )
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

    calculate :day_type,
              :string,
              expr(if day_of_week >= 1 and day_of_week <= 5, do: "weekday", else: "weekend") do
      public?(true)
      description("Day type as string: 'weekday' or 'weekend'")
    end

    calculate :day_name,
              :string,
              expr(
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

    calculate :duration_minutes,
              :integer,
              expr(fragment("EXTRACT(EPOCH FROM (? - ?)) / 60", end_time, start_time)) do
      public?(true)
      description("Duration of this schedule window in minutes")
    end

    # Helper functions for business logic and data validation

    @doc """
    Checks if the item schedule is currently active (not archived).

    ## Parameters
    - item_schedule: The item schedule record to check

    ## Returns
    - `true` if the schedule is active, `false` otherwise
    """
    @spec active?(t()) :: boolean()
    def active?(item_schedule) do
      case item_schedule do
        %{archived_at: nil} -> true
        _ -> false
      end
    end

    @doc """
    Checks if the item is available during this schedule.

    ## Parameters
    - item_schedule: The item schedule record to check

    ## Returns
    - `true` if available, `false` if blocked
    """
    @spec available?(t()) :: boolean()
    def available?(item_schedule), do: item_schedule.is_available

    @doc """
    Gets the day name for the schedule.

    ## Parameters
    - item_schedule: The item schedule record

    ## Returns
    - String with the day name
    """
    @spec day_name(t()) :: String.t()
    def day_name(item_schedule) do
      case item_schedule.day_of_week do
        0 -> "Sunday"
        1 -> "Monday"
        2 -> "Tuesday"
        3 -> "Wednesday"
        4 -> "Thursday"
        5 -> "Friday"
        6 -> "Saturday"
        _ -> "Unknown"
      end
    end

    @doc """
    Checks if the schedule is for a weekday.

    ## Parameters
    - item_schedule: The item schedule record to check

    ## Returns
    - `true` if weekday, `false` if weekend
    """
    @spec weekday?(t()) :: boolean()
    def weekday?(item_schedule) do
      item_schedule.day_of_week >= 1 and item_schedule.day_of_week <= 5
    end

    @doc """
    Checks if the schedule is for a weekend.

    ## Parameters
    - item_schedule: The item schedule record to check

    ## Returns
    - `true` if weekend, `false` if weekday
    """
    @spec weekend?(t()) :: boolean()
    def weekend?(item_schedule) do
      item_schedule.day_of_week == 0 or item_schedule.day_of_week == 6
    end

    @doc """
    Gets the duration of the schedule in minutes.

    ## Parameters
    - item_schedule: The item schedule record

    ## Returns
    - Integer with duration in minutes
    """
    @spec duration_minutes(t()) :: integer()
    def duration_minutes(item_schedule) do
      # Calculate the difference between end_time and start_time in minutes
      start_seconds = Time.to_second_after_midnight(item_schedule.start_time)
      end_seconds = Time.to_second_after_midnight(item_schedule.end_time)

      # Handle overnight schedules
      if end_seconds < start_seconds do
        end_seconds + 24 * 3600 - start_seconds
      else
        end_seconds - start_seconds
      end
      |> div(60)
    end

    @doc """
    Gets the formatted duration of the schedule.

    ## Parameters
    - item_schedule: The item schedule record

    ## Returns
    - String with formatted duration
    """
    @spec formatted_duration(t()) :: String.t()
    def formatted_duration(item_schedule) do
      duration_minutes(item_schedule) |> format_duration_minutes()
    end

    @doc """
    Formats the time range for display.

    ## Parameters
    - item_schedule: The item schedule record

    ## Returns
    - String with formatted time range
    """
    @spec formatted_time_range(t()) :: String.t()
    def formatted_time_range(item_schedule) do
      "#{Time.to_string(item_schedule.start_time)} - #{Time.to_string(item_schedule.end_time)}"
    end

    @doc """
    Checks if the schedule covers a specific time.

    ## Parameters
    - item_schedule: The item schedule record to check
    - time: Time to check against

    ## Returns
    - `true` if the time is within the schedule, `false` otherwise
    """
    @spec covers_time?(t(), Time.t()) :: boolean()
    def covers_time?(item_schedule, time) do
      start_seconds = Time.to_second_after_midnight(item_schedule.start_time)
      end_seconds = Time.to_second_after_midnight(item_schedule.end_time)
      target_seconds = Time.to_second_after_midnight(time)

      # Handle overnight schedules
      if end_seconds < start_seconds do
        target_seconds >= start_seconds or target_seconds <= end_seconds
      else
        target_seconds >= start_seconds and target_seconds <= end_seconds
      end
    end

    @doc """
    Checks if the schedule overlaps with another schedule.

    ## Parameters
    - item_schedule: The item schedule record to check
    - other_schedule: Another item schedule record to compare against

    ## Returns
    - `true` if overlapping, `false` otherwise
    """
    @spec overlaps_with?(t(), t()) :: boolean()
    def overlaps_with?(item_schedule, other_schedule) do
      # Only check if both schedules are for the same day
      if item_schedule.day_of_week == other_schedule.day_of_week do
        covers_time?(item_schedule, other_schedule.start_time) or
          covers_time?(item_schedule, other_schedule.end_time) or
          covers_time?(other_schedule, item_schedule.start_time)
      else
        false
      end
    end

    @doc """
    Validates that the item schedule has all required relationships.

    ## Parameters
    - item_schedule: The item schedule record to validate

    ## Returns
    - `{:ok, item_schedule}` if valid
    - `{:error, reason}` if invalid
    """
    @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
    def validate_relationships(item_schedule) do
      cond do
        is_nil(item_schedule.item) ->
          {:error, "Item relationship is missing"}

        true ->
          {:ok, item_schedule}
      end
    end

    @doc """
    Gets the item name associated with this schedule.

    ## Parameters
    - item_schedule: The item schedule record

    ## Returns
    - String with the item name
    """
    @spec item_name(t()) :: String.t()
    def item_name(item_schedule) do
      case item_schedule.item do
        %{name: name} when is_binary(name) and name != "" -> name
        _ -> "Unknown item"
      end
    end

    @doc """
    Formats the complete schedule information for display.

    ## Parameters
    - item_schedule: The item schedule record

    ## Returns
    - String with complete schedule information
    """
    @spec formatted_info(t()) :: String.t()
    def formatted_info(item_schedule) do
      with true <- active?(item_schedule),
           item_name <- item_name(item_schedule),
           day_name <- day_name(item_schedule),
           time_range <- formatted_time_range(item_schedule),
           duration <- formatted_duration(item_schedule),
           availability <- if(item_schedule.is_available, do: "Available", else: "Blocked") do
        "#{item_name} on #{day_name}: #{time_range} (#{duration}) - #{availability}"
      else
        false ->
          "Archived schedule for #{item_name(item_schedule)}"
      end
    end

    @doc """
    Checks if the schedule is valid (start time before end time).

    ## Parameters
    - item_schedule: The item schedule record to check

    ## Returns
    - `true` if valid, `false` otherwise
    """
    @spec valid?(t()) :: boolean()
    def valid?(item_schedule) do
      start_seconds = Time.to_second_after_midnight(item_schedule.start_time)
      end_seconds = Time.to_second_after_midnight(item_schedule.end_time)

      # Allow overnight schedules (end time before start time)
      start_seconds != end_seconds
    end

    @doc """
    Gets the next occurrence of this schedule.

    ## Parameters
    - item_schedule: The item schedule record

    ## Returns
    - `{:ok, DateTime.t()}` with the next occurrence, or `{:error, reason}`
    """
    @spec next_occurrence(t()) :: {:ok, DateTime.t()} | {:error, String.t()}
    def next_occurrence(item_schedule) do
      with true <- valid?(item_schedule),
           true <- active?(item_schedule) do
        # Get the current date and time
        now = DateTime.utc_now()

        # Calculate the next occurrence
        days_until_next =
          case item_schedule.day_of_week do
            # Sunday
            0 ->
              case now.weekday do
                7 -> 0
                _ -> 7 - now.weekday
              end

            _ ->
              case now.weekday do
                day when day < item_schedule.day_of_week -> item_schedule.day_of_week - day
                _ -> 7 - (now.weekday - item_schedule.day_of_week)
              end
          end

        # Create the next occurrence datetime
        next_date = Date.add(now, days_until_next)
        next_datetime = DateTime.new!(next_date, item_schedule.start_time, "Etc/UTC")

        {:ok, next_datetime}
      else
        false -> {:error, "Invalid schedule"}
        false -> {:error, "Schedule is not active"}
      end
    end

    # Private helper functions

    defp format_duration_minutes(minutes) when minutes < 60, do: "#{minutes} minutes"
    defp format_duration_minutes(minutes) when minutes == 60, do: "1 hour"
    defp format_duration_minutes(minutes) when minutes < 1440, do: "#{div(minutes, 60)} hours"
    defp format_duration_minutes(minutes), do: "#{div(minutes, 1440)} days"
  end

  identities do
    identity(:unique_item_day_time, [:item_id, :day_of_week, :start_time, :end_time])
  end
end
