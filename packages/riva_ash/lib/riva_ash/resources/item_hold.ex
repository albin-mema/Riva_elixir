defmodule RivaAsh.Resources.ItemHold do
  @moduledoc """
  Represents a temporary hold on an item during the booking process.
  Holds prevent double-booking during the 15-minute window while a customer
  completes their provisional reservation.

  Hold lifecycle:
  1. Created when customer starts booking process
  2. Automatically expires after 15 minutes
  3. Released when provisional reservation is created or cancelled
  4. Can be manually released by the system

  This resource manages temporary holds to prevent conflicts during the
  reservation process, ensuring items are not double-booked.
  """

  @type t :: %__MODULE__{
          id: String.t(),
          item_id: String.t(),
          client_id: String.t(),
          reserved_from: DateTime.t(),
          reserved_until: DateTime.t(),
          expires_at: DateTime.t(),
          hold_duration_minutes: integer(),
          is_active: boolean(),
          released_at: DateTime.t() | nil,
          notes: String.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshArchival.Resource
    ]

  import Ash.Expr
  require Ash.Query

  postgres do
    table("item_holds")
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
    type("item_hold")

    routes do
      base("/item-holds")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for hold management
      get(:by_item, route: "/by-item/:item_id")
      get(:by_client, route: "/by-client/:client_id")
      get(:active, route: "/active")
      get(:expired, route: "/expired")
    end
  end

  graphql do
    type(:item_hold)

    queries do
      get(:get_item_hold, :read)
      list(:list_item_holds, :read)
      list(:holds_by_item, :by_item)
      list(:holds_by_client, :by_client)
      list(:active_holds, :active)
      list(:expired_holds, :expired)
    end

    mutations do
      create(:create_item_hold, :create)
      update(:release_hold, :release)
      destroy(:delete_item_hold, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:release, action: :release)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_item, args: [:item_id], action: :by_item)
    define(:by_client, args: [:client_id], action: :by_client)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:item_id, :client_id, :reserved_from, :reserved_until, :hold_duration_minutes])
      primary?(true)

      # Set expiration time based on duration (default 15 minutes)
      change(set_attribute(:expires_at, expr(fragment("NOW() + INTERVAL '15 minutes'"))))
      change(set_attribute(:is_active, true))
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

    read :by_client do
      argument(:client_id, :uuid, allow_nil?: false)
      filter(expr(client_id == ^arg(:client_id)))
    end

    read :active do
      now = Timex.now()
      filter(expr(is_active == true and expires_at > ^now))
    end

    read :expired do
      now = Timex.now()
      filter(expr(is_active == true and expires_at <= ^now))
    end

    read :for_time_range do
      argument(:item_id, :uuid, allow_nil?: false)
      argument(:start_time, :utc_datetime, allow_nil?: false)
      argument(:end_time, :utc_datetime, allow_nil?: false)

      filter(
        expr(
          item_id == ^arg(:item_id) and
            is_active == true and
            reserved_from < ^arg(:end_time) and
            reserved_until > ^arg(:start_time)
        )
      )
    end

    # Release a hold (mark as inactive)
    update :release do
      accept([])
      require_atomic?(false)
      change(set_attribute(:is_active, false))
      change(set_attribute(:released_at, expr(fragment("NOW()"))))
      description("Release an active hold")
    end

    # Extend hold duration
    update :extend do
      argument(:additional_minutes, :integer, allow_nil?: false)
      require_atomic?(false)

      change(
        set_attribute(
          :expires_at,
          expr(fragment("? + INTERVAL '? minutes'", expires_at, ^arg(:additional_minutes)))
        )
      )

      description("Extend hold expiration time")
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :reserved_from, :utc_datetime do
      allow_nil?(false)
      public?(true)
      description("Start time of the hold period")
    end

    attribute :reserved_until, :utc_datetime do
      allow_nil?(false)
      public?(true)
      description("End time of the hold period")
    end

    attribute :expires_at, :utc_datetime do
      allow_nil?(false)
      public?(true)
      description("When this hold expires")
    end

    attribute :hold_duration_minutes, :integer do
      allow_nil?(false)
      default(15)
      public?(true)
      constraints(min: 1, max: 60)
      description("Duration of the hold in minutes")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this hold is currently active")
    end

    attribute :released_at, :utc_datetime do
      allow_nil?(true)
      public?(true)
      description("When the hold was manually released")
    end

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Additional notes about the hold")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :item, RivaAsh.Resources.Item do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The item being held")
    end

    belongs_to :client, RivaAsh.Resources.Client do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The client who initiated the hold")
    end
  end

  identities do
    # Prevent multiple active holds for the same item/time period
    identity(:unique_active_hold, [:item_id, :reserved_from, :reserved_until, :is_active])
  end

  validations do
    # Ensure hold period is valid
    validate(compare(:reserved_until, greater_than: :reserved_from),
      message: "End time must be after start time"
    )

    # Ensure hold doesn't exceed maximum duration
    validate(compare(:hold_duration_minutes, less_than_or_equal_to: 60),
      message: "Hold duration cannot exceed 60 minutes"
    )

    # Custom validation to check for overlapping active holds
    validate(
      fn changeset, _context ->
        item_id = Ash.Changeset.get_attribute(changeset, :item_id)
        reserved_from = Ash.Changeset.get_attribute(changeset, :reserved_from)
        reserved_until = Ash.Changeset.get_attribute(changeset, :reserved_until)
        is_active = Ash.Changeset.get_attribute(changeset, :is_active)

        # Only check for overlaps if this is an active hold
        if is_active && item_id && reserved_from && reserved_until do
          case check_for_overlapping_holds(item_id, reserved_from, reserved_until, changeset) do
            :ok -> :ok
            {:error, message} -> {:error, field: :reserved_from, message: message}
          end
        else
          :ok
        end
      end,
      message: "Cannot create overlapping holds for the same item"
    )

    # Custom validation to ensure hold hasn't expired
    validate(
      fn changeset, _context ->
        expires_at = Ash.Changeset.get_attribute(changeset, :expires_at)

        if expires_at && Timex.compare(expires_at, Timex.now()) == :lt do
          {:error, field: :expires_at, message: "Hold expiration time cannot be in the past"}
        else
          :ok
        end
      end,
      message: "Cannot create holds that have already expired"
    )
  end

  # Helper function to check for overlapping holds
  defp check_for_overlapping_holds(item_id, reserved_from, reserved_until, changeset) do
    # Get the ID of the current record if it's an update
    current_id = Ash.Changeset.get_data(changeset, :id)

    query =
      __MODULE__
      |> Ash.Query.filter(
        expr(
          item_id == ^item_id and
            is_active == true and
            reserved_from < ^reserved_until and
            reserved_until > ^reserved_from
        )
      )

    # Exclude current record if this is an update
    query =
      if current_id do
        Ash.Query.filter(query, expr(id != ^current_id))
      else
        query
      end

    case Ash.read(query, domain: RivaAsh.Domain) do
      {:ok, []} -> :ok
      {:ok, _overlapping_holds} -> {:error, "Overlapping active hold exists for this time period"}
      # If we can't check, allow the operation
      {:error, _} -> :ok
    end
  end

  # Helper functions for business logic and data validation

  @doc """
  Checks if the item hold is currently active.

  ## Parameters
  - item_hold: The item hold record to check

  ## Returns
  - `true` if the hold is active, `false` otherwise
  """
  @spec active?(t()) :: boolean()
  def active?(item_hold) do
    case item_hold do
      %{is_active: true, archived_at: nil} ->
        # Check if the hold has expired
        case DateTime.compare(item_hold.expires_at, DateTime.utc_now()) do
          :gt -> true
          _ -> false
        end

      _ ->
        false
    end
  end

  @doc """
  Checks if the item hold has expired.

  ## Parameters
  - item_hold: The item hold record to check

  ## Returns
  - `true` if the hold has expired, `false` otherwise
  """
  @spec expired?(t()) :: boolean()
  def expired?(item_hold) do
    case item_hold do
      %{is_active: true} ->
        case DateTime.compare(item_hold.expires_at, DateTime.utc_now()) do
          :lt -> true
          _ -> false
        end

      _ ->
        false
    end
  end

  @doc """
  Gets the remaining time until the hold expires.

  ## Parameters
  - item_hold: The item hold record

  ## Returns
  - `{:ok, seconds}` with remaining seconds, or `{:error, reason}` if invalid
  """
  @spec remaining_time(t()) :: {:ok, integer()} | {:error, String.t()}
  def remaining_time(item_hold) do
    case active?(item_hold) do
      true ->
        remaining = DateTime.diff(item_hold.expires_at, DateTime.utc_now(), :second)
        {:ok, remaining}

      false ->
        {:error, "Hold is not active"}
    end
  end

  @doc """
  Formats the remaining time in a human-readable format.

  ## Parameters
  - item_hold: The item hold record

  ## Returns
  - String with formatted remaining time or "Expired"
  """
  @spec formatted_remaining_time(t()) :: String.t()
  def formatted_remaining_time(item_hold) do
    case remaining_time(item_hold) do
      {:ok, seconds} when seconds > 0 ->
        format_duration(seconds)

      {:ok, _} ->
        "Expired"

      {:error, _} ->
        "Invalid hold"
    end
  end

  @doc """
  Gets the duration of the hold in minutes.

  ## Parameters
  - item_hold: The item hold record

  ## Returns
  - Integer with duration in minutes
  """
  @spec duration_minutes(t()) :: integer()
  def duration_minutes(item_hold) do
    DateTime.diff(item_hold.reserved_until, item_hold.reserved_from, :second) |> div(60)
  end

  @doc """
  Gets the hold duration in a human-readable format.

  ## Parameters
  - item_hold: The item hold record

  ## Returns
  - String with formatted duration
  """
  @spec formatted_duration(t()) :: String.t()
  def formatted_duration(item_hold) do
    duration_minutes(item_hold) |> format_duration_minutes()
  end

  @doc """
  Checks if the hold is for the specified time range.

  ## Parameters
  - item_hold: The item hold record
  - start_time: Start time to check against
  - end_time: End time to check against

  ## Returns
  - `true` if the hold overlaps with the specified time range, `false` otherwise
  """
  @spec overlaps_with?(t(), DateTime.t(), DateTime.t()) :: boolean()
  def overlaps_with?(item_hold, start_time, end_time) do
    DateTime.compare(item_hold.reserved_until, start_time) == :gt and
      DateTime.compare(item_hold.reserved_from, end_time) == :lt
  end

  @doc """
  Releases the hold by marking it as inactive.

  ## Parameters
  - item_hold: The item hold record to release

  ## Returns
  - `{:ok, updated_hold}` if successful
  - `{:error, reason}` if failed
  """
  @spec release(t()) :: {:ok, t()} | {:error, String.t()}
  def release(item_hold) do
    case Ash.update(item_hold, action: :release, domain: RivaAsh.Domain) do
      {:ok, updated_hold} -> {:ok, updated_hold}
      {:error, reason} -> {:error, "Failed to release hold: #{inspect(reason)}"}
    end
  end

  @doc """
  Extends the hold duration by the specified number of minutes.

  ## Parameters
  - item_hold: The item hold record to extend
  - additional_minutes: Number of minutes to add

  ## Returns
  - `{:ok, updated_hold}` if successful
  - `{:error, reason}` if failed
  """
  @spec extend(t(), integer()) :: {:ok, t()} | {:error, String.t()}
  def extend(item_hold, additional_minutes) when additional_minutes > 0 do
    case Ash.update(item_hold,
           action: :extend,
           arguments: [additional_minutes: additional_minutes],
           domain: RivaAsh.Domain
         ) do
      {:ok, updated_hold} -> {:ok, updated_hold}
      {:error, reason} -> {:error, "Failed to extend hold: #{inspect(reason)}"}
    end
  end

  def extend(_item_hold, _additional_minutes) do
    {:error, "Additional minutes must be positive"}
  end

  @doc """
  Validates that the hold has all required relationships.

  ## Parameters
  - item_hold: The item hold record to validate

  ## Returns
  - `{:ok, item_hold}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_relationships(item_hold) do
    cond do
      is_nil(item_hold.item) ->
        {:error, "Item relationship is missing"}

      is_nil(item_hold.client) ->
        {:error, "Client relationship is missing"}

      true ->
        {:ok, item_hold}
    end
  end

  @doc """
  Checks if the hold can be extended.

  ## Parameters
  - item_hold: The item hold record to check

  ## Returns
  - `true` if the hold can be extended, `false` otherwise
  """
  @spec can_extend?(t()) :: boolean()
  def can_extend?(item_hold) do
    case active?(item_hold) do
      true ->
        case remaining_time(item_hold) do
          {:ok, remaining} -> remaining > 300
          _ -> false
        end

      _ ->
        false
    end
  end

  @doc """
  Gets the maximum extension time for the hold.

  ## Parameters
  - item_hold: The item hold record

  ## Returns
  - Integer with maximum additional minutes allowed
  """
  @spec max_extension_minutes(t()) :: integer()
  def max_extension_minutes(item_hold) do
    case active?(item_hold) do
      true ->
        case remaining_time(item_hold) do
          {:ok, remaining} -> min(div(remaining, 60), 10)
          _ -> 0
        end

      _ ->
        0
    end
  end

  @doc """
  Formats the hold information for display.

  ## Parameters
  - item_hold: The item hold record

  ## Returns
  - String with formatted hold information
  """
  @spec formatted_info(t()) :: String.t()
  def formatted_info(item_hold) do
    case active?(item_hold) do
      true ->
        case item_hold.item.name do
          item_name ->
            case display_name(item_hold.client) do
              client_name ->
                case formatted_remaining_time(item_hold) do
                  remaining_time -> "#{client_name} has hold on '#{item_name}' until #{remaining_time}"
                end
            end
        end

      false ->
        "Hold on '#{item_hold.item.name}' has expired"
    end
  end

  # Private helper functions

  defp format_duration(seconds) when seconds < 60, do: "#{seconds} seconds"
  defp format_duration(seconds) when seconds < 3600, do: "#{div(seconds, 60)} minutes"
  defp format_duration(seconds), do: "#{div(seconds, 3600)} hours"

  defp format_duration_minutes(minutes) when minutes < 60, do: "#{minutes} minutes"
  defp format_duration_minutes(minutes) when minutes == 60, do: "1 hour"
  defp format_duration_minutes(minutes), do: "#{div(minutes, 60)} hours"

  defp display_name(client) do
    case client do
      %{first_name: first, last_name: last} when is_binary(first) and is_binary(last) ->
        "#{first} #{last}"

      %{first_name: first} when is_binary(first) ->
        first

      %{last_name: last} when is_binary(last) ->
        last

      _ ->
        "Unknown client"
    end
  end
end
