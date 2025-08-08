defmodule RivaAsh.Resources.Item do
  @moduledoc """
  Represents an individual item that can optionally belong to a section.
  Items are the basic inventory units in the system.

  Items can be reserved by clients and have various availability patterns.
  They belong to a business and can be categorized by item type.
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

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          description: String.t() | nil,
          business_id: String.t(),
          section_id: String.t() | nil,
          item_type_id: String.t() | nil,
          is_active: boolean(),
          is_always_available: boolean(),
          capacity: integer(),
          minimum_duration_minutes: integer() | nil,
          maximum_duration_minutes: integer() | nil,
          is_public_searchable: boolean(),
          public_description: String.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  standard_postgres("items")
  standard_archive()
  standard_admin([:name, :section, :item_type, :is_active, :is_always_available])

  # Authorization policies
  policies do
    # Admin bypass (for both User and Employee actors)
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Business owner has full access to their business data
    policy action_type([:read, :create, :update, :destroy]) do
      authorize_if(expr(section.plot.business.owner_id == ^actor(:id)))
    end

    # Employees with manager role can manage items
    policy action_type([:create, :update]) do
      authorize_if(actor_attribute_equals(:role, :manager))
    end

    # Employees can read items
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :employee))
    end

    # Staff employees can read items
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :staff))
    end

    # Regular users can read items (for business owners who are Users, not Employees)
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :user))
    end

    # Public search action requires no authentication
    policy action(:public_search) do
      authorize_if(always())
    end
  end

  json_api do
    type("item")

    routes do
      base("/items")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Business-scoped routes
      get(:by_business, route: "/by-business/:business_id")
      get(:by_business_active, route: "/by-business/:business_id/active")

      # Section-specific routes
      get(:by_section, route: "/by-section/:section_id")
      get(:unassigned, route: "/unassigned")

      # Status routes
      get(:active, route: "/active")
      get(:inactive, route: "/inactive")

      # Availability routes
      get(:always_available, route: "/always-available")
      get(:scheduled_availability, route: "/scheduled-availability")
      get(:with_schedules, route: "/with-schedules")
      get(:available_now, route: "/available-now")
      get(:available_for_date, route: "/available-for-date/:date")
    end
  end

  graphql do
    type(:item)

    queries do
      get(:get_item, :read)
      list(:list_items, :read)
      list(:active_items, :active)
      list(:inactive_items, :inactive)
      list(:items_by_business, :by_business)
      list(:available_items, :available_now)
    end

    mutations do
      create(:create_item, :create)
      update(:update_item, :update)
      destroy(:delete_item, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_business, args: [:business_id], action: :by_business)
    define(:by_section, args: [:section_id], action: :by_section)
    define(:unassigned, action: :unassigned)
    define(:active, action: :active)
    define(:inactive, action: :inactive)
    define(:always_available, action: :always_available)
    define(:scheduled_availability, action: :scheduled_availability)
    define(:with_schedules, action: :with_schedules)
    define(:available_now, action: :available_now)
    define(:available_for_date, args: [:date], action: :available_for_date)
    define(:public_search, action: :public_search)
    define(:for_user_businesses, args: [:business_ids], action: :for_user_businesses)
    define(:with_status_counts, args: [:business_ids], action: :with_status_counts)
  end

  actions do
    defaults([:read, :destroy])

    update :update do
      accept([
        :name,
        :section_id,
        :item_type_id,
        :is_active,
        :is_always_available,
        :is_public_searchable,
        :public_description
      ])

      primary?(true)
      require_atomic?(false)

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_section_business_match/2)
      validate(&RivaAsh.Validations.validate_item_type_business_match/2)
    end

    create :create do
      accept([
        :name,
        :section_id,
        :item_type_id,
        :business_id,
        :is_active,
        :is_always_available,
        :is_public_searchable,
        :public_description
      ])

      primary?(true)

      # Validate business access
      validate(&RivaAsh.Validations.validate_business_access/2)

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_section_business_match/2)
      validate(&RivaAsh.Validations.validate_item_type_business_match/2)
    end

    # Standard read actions are already defined below

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(section.plot.business_id == ^arg(:business_id)))
    end

    read :by_business_active do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(section.plot.business_id == ^arg(:business_id) and is_active == true))
    end

    read :by_section do
      argument(:section_id, :uuid, allow_nil?: false)
      filter(expr(section_id == ^arg(:section_id)))
    end

    read :unassigned do
      filter(expr(is_nil(section_id)))
    end

    read :active do
      filter(expr(is_active == true))
    end

    read :inactive do
      filter(expr(is_active == false))
    end

    read :always_available do
      filter(expr(is_always_available == true and is_active == true and is_nil(archived_at)))
    end

    read :scheduled_availability do
      filter(expr(is_always_available == false and is_active == true and is_nil(archived_at)))
    end

    read :with_schedules do
      prepare(build(load: [:schedules]))
    end

    read :available_now do
      filter(
        expr(
          is_active == true and
            is_nil(archived_at) and
            not exists(
              reservations,
              status in [:confirmed, :pending] and
                reserved_from <= now() and
                reserved_until >= now()
            )
        )
      )
    end

    read :available_for_date do
      argument(:date, :date, allow_nil?: false)

      filter(
        expr(
          # Just to use the date argument
          is_active == true and
            is_nil(archived_at) and
            ^arg(:date) >= date("1970-01-01")
        )
      )

      prepare(fn query, _ ->
        {:ok, apply_availability_filter(query, query.arguments.date)}
      end)
    end

    read :public_search do
      # Public search action for unregistered users
      # No authorization required, only returns publicly searchable items from publicly searchable businesses
      filter(
        expr(
          is_public_searchable == true and
            is_active == true and
            is_nil(archived_at) and
            business.is_public_searchable == true and
            is_nil(business.archived_at)
        )
      )

      # Allow searching by name, description, and business name
      argument(:search_term, :string, allow_nil?: true)
      argument(:business_id, :uuid, allow_nil?: true)

      prepare(fn query, _context ->
        query
        |> apply_business_filter(Ash.Query.get_argument(query, :business_id))
        |> apply_search_filter(Ash.Query.get_argument(query, :search_term))
        |> apply_active_filter()
      end)
    end

    # Bulk operations
    action :bulk_update_status do
      argument(:ids, {:array, :uuid}, allow_nil?: false)
      argument(:is_active, :boolean, allow_nil?: false)

      run(fn input, context ->
        ids = input.arguments.ids
        is_active = input.arguments.is_active

        try do
          # Use bulk update functionality
          case Ash.bulk_update(__MODULE__, :update, %{is_active: is_active},
                 domain: RivaAsh.Domain,
                 actor: context[:actor],
                 filter: [id: [in: ids]]
               ) do
            %Ash.BulkResult{records: records, errors: []} ->
              {:ok, records}

            %Ash.BulkResult{records: records, errors: errors} ->
              # Log errors but return successful records
              require Logger

              Enum.each(errors, fn error ->
                Logger.error("Bulk update error: #{inspect(error)}")
              end)

              {:ok, records}

            {:error, error} ->
              {:error, "Failed to perform bulk update: #{inspect(error)}"}
          end
        rescue
          e -> {:error, "Exception during bulk update: #{inspect(e)}"}
        end
      end)
    end

    # Get items for user's businesses (from InventoryService)
    read :for_user_businesses do
      argument(:business_ids, {:array, :uuid}, allow_nil?: false)
      filter(expr(business_id in ^arg(:business_ids)))
      prepare(build(load: [:business, :section], calculate: [:current_status]))
    end

    # Get items with status counts (from InventoryService)
    read :with_status_counts do
      argument(:business_ids, {:array, :uuid}, allow_nil?: false)
      filter(expr(business_id in ^arg(:business_ids)))
      prepare(build(calculate: [:current_status]))
    end
  end

  attributes do
    standard_attributes()
    name_attribute(description: "The name of the item")
    description_attribute()
    active_attribute()

    attribute :is_always_available, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether the item is always available (true) or follows a schedule (false)")
    end

    attribute :capacity, :integer do
      allow_nil?(false)
      default(1)
      public?(true)
      constraints(min: 1, max: 100)
      description("How many concurrent reservations this item can handle")
    end

    attribute :minimum_duration_minutes, :integer do
      allow_nil?(true)
      public?(true)
      constraints(min: 15, max: 1440)
      description("Minimum reservation duration in minutes")
    end

    attribute :maximum_duration_minutes, :integer do
      allow_nil?(true)
      public?(true)
      constraints(min: 15, max: 10_080)
      description("Maximum reservation duration in minutes")
    end

    # Business relationship as attribute for performance
    attribute :business_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("The business this item belongs to")
    end

    attribute :is_public_searchable, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether this item appears in global search for unregistered users")
    end

    attribute :public_description, :string do
      allow_nil?(true)
      public?(true)
      description("Public-facing description shown in global search results")
      constraints(max_length: 500, trim?: true)
    end
  end

  relationships do
    business_relationship()

    belongs_to :section, RivaAsh.Resources.Section do
      allow_nil?(true)
      attribute_writable?(true)
      public?(true)
      description("The section this item belongs to (optional)")
    end

    belongs_to :item_type, RivaAsh.Resources.ItemType do
      allow_nil?(true)
      attribute_writable?(true)
      public?(true)
      description("The type/category of this item")
    end

    has_many :reservations, RivaAsh.Resources.Reservation do
      destination_attribute(:item_id)
      public?(true)
      description("Reservations for this item")
    end

    has_many :item_positions, RivaAsh.Resources.ItemPosition do
      destination_attribute(:item_id)
      public?(true)
      description("Positions of this item in various layouts")
    end

    has_many :schedules, RivaAsh.Resources.ItemSchedule do
      destination_attribute(:item_id)
      public?(true)
      description("Recurring availability schedules for this item")
    end

    has_many :availability_exceptions, RivaAsh.Resources.AvailabilityException do
      destination_attribute(:item_id)
      public?(true)
      description("Availability exceptions for this item")
    end

    has_many :recurring_reservations, RivaAsh.Resources.RecurringReservation do
      destination_attribute(:item_id)
      public?(true)
      description("Recurring reservation patterns for this item")
    end
  end

  # Aggregates for performance
  aggregates do
    count(:reservation_count, :reservations)
    count(:active_reservation_count, :reservations, filter: expr(status == :confirmed))
  end

  # Calculations for frequently accessed data
  calculations do
    calculate(
      :is_available_now,
      :boolean,
      expr(
        is_active and
          is_nil(archived_at) and
          not exists(
            reservations,
            status == :confirmed and
              reserved_from <= now() and reserved_until >= now()
          )
      )
    )

    # Calculate current status (from InventoryService)
    calculate(
      :current_status,
      :atom,
      expr(
        cond do
          not is_active or not is_nil(archived_at) ->
            :inactive

          exists(reservations, status == :confirmed and reserved_from <= now() and reserved_until >= now()) ->
            :occupied

          exists(item_holds, status == :active and start_time <= now() and (is_nil(end_time) or end_time >= now())) ->
            :hold

          true ->
            :available
        end
      )
    ) do
      public?(true)
      description("Current status of the item: available, occupied, hold, or inactive")
    end
  end

  validations do
    validate(present([:name, :business_id]), message: "Name and business are required")
    validate(&RivaAsh.Validations.sanitize_text_input/2)
    validate(&RivaAsh.Validations.validate_business_access/2)

    # Duration constraints
    validate(compare(:maximum_duration_minutes, greater_than: :minimum_duration_minutes),
      where: [present(:minimum_duration_minutes), present(:maximum_duration_minutes)],
      message: "Maximum duration must be greater than minimum duration"
    )
  end

  identities do
    identity(:unique_name_per_business, [:name, :business_id])
  end

  # Helper functions for business logic and data validation

  @doc """
  Checks if the item is currently active (not archived).

  ## Parameters
  - item: The item record to check

  ## Returns
  - `true` if the item is active, `false` otherwise
  """
  @spec active?(t()) :: boolean()
  def active?(item) do
    case item do
      %{archived_at: nil} -> true
      _ -> false
    end
  end

  @doc """
  Checks if the item is available for reservation right now.

  ## Parameters
  - item: The item record to check

  ## Returns
  - `true` if the item is available now, `false` otherwise
  """
  @spec available_now?(t()) :: boolean()
  def available_now?(item) do
    case item do
      %{is_active: true, archived_at: nil} ->
        # Check if there are any active reservations
        case item.reservations do
          [] ->
            true

          reservations ->
            Enum.all?(reservations, fn reservation ->
              reservation.status not in [:confirmed, :pending] or
                DateTime.compare(reservation.reserved_until, DateTime.utc_now()) == :lt
            end)
        end

      _ ->
        false
    end
  end

  @doc """
  Gets the display name of the item with section information if available.

  ## Parameters
  - item: The item record

  ## Returns
  - String with the item name and section if available
  """
  @spec display_name(t()) :: String.t()
  def display_name(item) do
    case item.section do
      %{name: section_name} when is_binary(section_name) and section_name != "" ->
        "#{item.name} (#{section_name})"

      _ ->
        item.name
    end
  end

  @doc """
  Gets the formatted capacity information for the item.

  ## Parameters
  - item: The item record

  ## Returns
  - String with capacity information
  """
  @spec capacity_info(t()) :: String.t()
  def capacity_info(item) do
    "Capacity: #{item.capacity}"
  end

  @doc """
  Gets the duration range for the item in a human-readable format.

  ## Parameters
  - item: The item record

  ## Returns
  - String with duration information or "No duration limits"
  """
  @spec duration_range(t()) :: String.t()
  def duration_range(item) do
    cond do
      is_nil(item.minimum_duration_minutes) and is_nil(item.maximum_duration_minutes) ->
        "No duration limits"

      is_nil(item.minimum_duration_minutes) ->
        "Up to #{format_duration(item.maximum_duration_minutes)}"

      is_nil(item.maximum_duration_minutes) ->
        "From #{format_duration(item.minimum_duration_minutes)}"

      true ->
        "#{format_duration(item.minimum_duration_minutes)} - #{format_duration(item.maximum_duration_minutes)}"
    end
  end

  @doc """
  Formats minutes into a human-readable duration string.

  ## Parameters
  - minutes: Number of minutes to format

  ## Returns
  - String with formatted duration
  """
  @spec format_duration(integer() | nil) :: String.t()
  def format_duration(nil), do: "No limit"
  def format_duration(minutes) when minutes < 60, do: "#{minutes} minutes"
  def format_duration(minutes) when minutes == 60, do: "1 hour"
  def format_duration(minutes) when minutes < 1440, do: "#{div(minutes, 60)} hours"
  def format_duration(minutes), do: "#{div(minutes, 1440)} days"

  @doc """
  Checks if the item has duration constraints.

  ## Parameters
  - item: The item record to check

  ## Returns
  - `true` if the item has duration constraints, `false` otherwise
  """
  @spec has_duration_constraints?(t()) :: boolean()
  def has_duration_constraints?(item) do
    not (is_nil(item.minimum_duration_minutes) and is_nil(item.maximum_duration_minutes))
  end

  @doc """
  Validates that the duration constraints are valid.

  ## Parameters
  - item: The item record to validate

  ## Returns
  - `{:ok, item}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_duration_constraints(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_duration_constraints(item) do
    cond do
      not is_nil(item.minimum_duration_minutes) and not is_nil(item.maximum_duration_minutes) ->
        if item.minimum_duration_minutes >= item.maximum_duration_minutes do
          {:error, "Minimum duration must be less than maximum duration"}
        else
          {:ok, item}
        end

      true ->
        {:ok, item}
    end
  end

  @doc """
  Checks if the item is always available or follows a schedule.

  ## Parameters
  - item: The item record to check

  ## Returns
  - `true` if always available, `false` if scheduled
  """
  @spec always_available?(t()) :: boolean()
  def always_available?(item), do: item.is_always_available

  @doc """
  Checks if the item appears in public search results.

  ## Parameters
  - item: The item record to check

  ## Returns
  - `true` if publicly searchable, `false` otherwise
  """
  @spec public_searchable?(t()) :: boolean()
  def public_searchable?(item), do: item.is_public_searchable

  @doc """
  Gets the public-facing description of the item.

  ## Parameters
  - item: The item record

  ## Returns
  - String with public description or regular description if no public description
  """
  @spec public_description(t()) :: String.t()
  def public_description(item) do
    case item.public_description do
      desc when is_binary(desc) and desc != "" -> desc
      _ -> item.description || "No description available"
    end
  end

  @doc """
  Formats the item information for display in search results.

  ## Parameters
  - item: The item record

  ## Returns
  - String with formatted item information
  """
  @spec formatted_search_result(t()) :: String.t()
  def formatted_search_result(item) do
    case active?(item) do
      true ->
        case item.business.name do
          business_name -> "#{display_name(item)} - #{business_name} #{capacity_info(item)}"
        end

      false ->
        "#{display_name(item)} - Inactive"
    end
  end

  @doc """
  Calculates the current reservation count for the item.

  ## Parameters
  - item: The item record

  ## Returns
  - Integer with current reservation count
  """
  @spec current_reservation_count(t()) :: integer()
  def current_reservation_count(item) do
    Enum.filter(item.reservations || [], fn reservation ->
      reservation.status in [:confirmed, :pending] and
        DateTime.compare(reservation.reserved_until, DateTime.utc_now()) == :gt
    end)
    |> length()
  end

  @doc """
  Checks if the item has available capacity for new reservations.

  ## Parameters
  - item: The item record

  ## Returns
  - `true` if capacity is available, `false` otherwise
  """
  @spec has_available_capacity?(t()) :: boolean()
  def has_available_capacity?(item) do
    current_count = current_reservation_count(item)
    current_count < item.capacity
  end

  @doc """
  Gets the available capacity for the item.

  ## Parameters
  - item: The item record

  ## Returns
  - Integer with available capacity
  """
  @spec available_capacity(t()) :: integer()
  def available_capacity(item) do
    max(0, item.capacity - current_reservation_count(item))
  end

  @doc """
  Validates that the item has all required relationships.

  ## Parameters
  - item: The item record to validate

  ## Returns
  - `{:ok, item}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_relationships(item) do
    cond do
      is_nil(item.business) ->
        {:error, "Business relationship is missing"}

      not is_nil(item.section) and is_nil(item.section.plot) ->
        {:error, "Section plot relationship is missing"}

      true ->
        {:ok, item}
    end
  end

  # Private helper functions

  defp format_minutes(minutes) when minutes < 60, do: "#{minutes} minutes"
  defp format_minutes(minutes) when minutes == 60, do: "1 hour"
  defp format_minutes(minutes) when minutes < 1440, do: "#{div(minutes, 60)} hours"
  defp format_minutes(minutes), do: "#{div(minutes, 1440)} days"

  # Private helper functions for filtering
  @spec apply_availability_filter(Ash.Query.t(), Date.t()) :: Ash.Query.t()
  defp apply_availability_filter(query, _date), do: query

  @spec apply_business_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_business_filter(query, nil), do: query
  defp apply_business_filter(query, _business_id), do: query

  @spec apply_search_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_search_filter(query, nil), do: query
  defp apply_search_filter(query, _search_term), do: query

  @spec apply_active_filter(Ash.Query.t()) :: Ash.Query.t()
  defp apply_active_filter(query), do: query
end
