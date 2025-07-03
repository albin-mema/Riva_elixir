defmodule RivaAsh.Resources.Item do
  @moduledoc """
  Represents an individual item that can optionally belong to a section.
  Items are the basic inventory units in the system.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshArchival.Resource]

  postgres do
    table("items")
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
    type("item")

    routes do
      base("/items")

      get(:read)
      index(:read)
      post(:create)
      delete(:destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_section, args: [:section_id], action: :by_section)
    define(:unassigned, action: :unassigned)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :section_id])
      primary?(true)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
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

    read :always_available do
      filter(expr(is_always_available == true and is_active == true))
    end

    read :scheduled_availability do
      filter(expr(is_always_available == false and is_active == true))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the item")
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("Description of the item")
    end

    attribute :is_always_available, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)

      description(
        "Whether this item is always available (true) or has specific availability rules (false)"
      )
    end

    attribute :capacity, :integer do
      allow_nil?(false)
      default(1)
      public?(true)
      description("How many concurrent reservations this item can handle")
    end

    attribute :minimum_duration_minutes, :integer do
      allow_nil?(true)
      public?(true)
      description("Minimum reservation duration in minutes")
    end

    attribute :maximum_duration_minutes, :integer do
      allow_nil?(true)
      public?(true)
      description("Maximum reservation duration in minutes")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this item is currently active and bookable")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :section, RivaAsh.Resources.Section do
      allow_nil?(true)
      attribute_writable?(true)
      public?(true)
      description("The section this item belongs to (optional)")
    end

    has_many :reservations, RivaAsh.Resources.Reservation do
      destination_attribute(:item_id)
      public?(true)
      description("Reservations for this item")
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

  identities do
    identity(:unique_name, [:name])
  end
end
