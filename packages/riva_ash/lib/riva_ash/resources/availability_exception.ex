defmodule RivaAsh.Resources.AvailabilityException do
  @moduledoc """
  Represents exceptions to regular item schedules.

  Availability exceptions allow for temporary changes to an item's regular availability,
  such as maintenance periods, holidays, special events, or one-time availability changes.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshArchival.Resource, AshPaperTrail.Resource]

  @type t :: %RivaAsh.Resources.AvailabilityException{
          id: String.t(),
          date: Date.t(),
          start_time: Time.t() | nil,
          end_time: Time.t() | nil,
          is_available: boolean(),
          reason: String.t() | nil,
          exception_type: atom(),
          notes: String.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

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

  @doc """
  Determines if the availability exception is currently active.
  """
  @spec is_active?(t()) :: boolean()
  def is_active?(exception) when is_struct(exception) do
    %{date: date, start_time: start_time, end_time: end_time} = exception
    today = Date.utc_today()
    now = Time.utc_now()

    case Date.compare(date, today) do
      :eq ->
        case {start_time, end_time} do
          # All-day exception
          {nil, nil} -> true
          {start, end_time} -> Time.compare(now, start) != :lt and Time.compare(now, end_time) == :lt
        end

      :gt ->
        true

      :lt ->
        false
    end
  end

  @doc """
  Calculates the duration of the availability exception in minutes.
  Returns 0 for all-day exceptions (when start_time and end_time are nil).
  """
  @spec duration_minutes(t()) :: integer()
  def duration_minutes(exception) when is_struct(exception) do
    %{start_time: start_time, end_time: end_time} = exception

    case {start_time, end_time} do
      {nil, nil} ->
        0

      {start_time, end_time} ->
        start_seconds = Time.to_second_after_midnight(start_time)
        end_seconds = Time.to_second_after_midnight(end_time)
        end_seconds - start_seconds
    end
  end

  @doc """
  Determines if the exception is for all-day (no specific start/end times).
  """
  @spec is_all_day?(t()) :: boolean()
  def is_all_day?(exception) when is_struct(exception) do
    %{start_time: start_time, end_time: end_time} = exception
    start_time == nil and end_time == nil
  end

  @doc """
  Gets a human-readable description of the exception type.
  """
  @spec exception_type_description(atom()) :: String.t()
  def exception_type_description(:holiday), do: "Holiday"
  def exception_type_description(:maintenance), do: "Maintenance"
  def exception_type_description(:special_event), do: "Special Event"
  def exception_type_description(:closure), do: "Closure"
  def exception_type_description(:extended_hours), do: "Extended Hours"
  def exception_type_description(:other), do: "Other"

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
      constraints(one_of: [:holiday, :maintenance, :special_event, :closure, :extended_hours, :other])

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
