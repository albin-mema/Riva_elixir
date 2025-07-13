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

  standard_postgres("items")
  standard_archive()
  standard_admin([:name, :section, :item_type, :is_active, :is_always_available])

  # Authorization policies
  policies do
    # Admin bypass
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

  standard_graphql(:item)

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
  end

  actions do
    defaults([:read, :destroy])

    update :update do
      accept([:name, :section_id, :item_type_id, :is_active, :is_always_available])
      primary?(true)
      require_atomic? false

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_section_business_match/2)
      validate(&RivaAsh.Validations.validate_item_type_business_match/2)
    end

    create :create do
      accept([:name, :section_id, :item_type_id, :business_id, :is_active, :is_always_available])
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
      filter(expr(
        is_active == true and
        is_nil(archived_at) and
        not exists(reservations,
          status in [:confirmed, :pending] and
          reserved_from <= now() and
          reserved_until >= now()
        )
      ))
    end

    read :available_for_date do
      argument(:date, :date, allow_nil?: false)

      filter(expr(
        is_active == true and
        is_nil(archived_at) and
        not exists(reservations,
          status in [:confirmed, :pending] and
          fragment("DATE(reserved_from) <= ? AND DATE(reserved_until) >= ?",
            ^arg(:date), ^arg(:date))
        )
      ))
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
                               filter: [id: [in: ids]]) do
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
      constraints(min: 15, max: 10080)
      description("Maximum reservation duration in minutes")
    end

    # Business relationship as attribute for performance
    attribute :business_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("The business this item belongs to")
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
    calculate(:is_available_now, :boolean, expr(
      is_active and
      is_nil(archived_at) and
      not exists(reservations, status == :confirmed and
        reserved_from <= now() and reserved_until >= now())
    ))
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

  # Helper function for admin dropdowns
  def choices_for_select do
    RivaAsh.ResourceHelpers.choices_for_select(__MODULE__)
  end
end
