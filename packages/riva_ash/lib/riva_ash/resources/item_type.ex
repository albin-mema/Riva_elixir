defmodule RivaAsh.Resources.ItemType do
  @moduledoc """
  Represents a type/category of items that can be reserved.
  Item types help categorize and organize different kinds of reservable resources.

  Examples:
  - Table (for restaurants)
  - Room (for hotels/meeting spaces)
  - Equipment (for rental businesses)
  - Court (for sports facilities)
  - Workstation (for coworking spaces)
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
  import RivaAsh.Authorization

  standard_postgres("item_types")
  standard_archive()
  standard_admin([:name, :business, :description, :color, :icon, :is_active])

  # Authorization policies
  policies do
    # TODO: Re-enable business_scoped_policies() after fixing macro
    # business_scoped_policies()
    # TODO: Re-enable employee_accessible_policies() after fixing macro
    # employee_accessible_policies(:manage_item_types)
  end

  json_api do
    type("item_type")

    routes do
      base("/item-types")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for item type specific actions
      get(:by_business, route: "/by-business/:business_id")
      get(:active, route: "/active")
      get(:with_items, route: "/with-items")
    end
  end

  graphql do
    type(:item_type)

    queries do
      get(:get_item_type, :read)
      list(:list_item_types, :read)
      list(:item_types_by_business, :by_business)
      list(:active_item_types, :active)
      list(:item_types_with_items, :with_items)
    end

    mutations do
      create(:create_item_type, :create)
      update(:update_item_type, :update)
      destroy(:delete_item_type, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_business, args: [:business_id], action: :by_business)
    define(:active, action: :active)
    define(:with_items, action: :with_items)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :description, :business_id, :color, :icon, :is_active])
      primary?(true)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(business_id == ^arg(:business_id)))
    end

    read :active do
      filter(expr(is_active == true and is_nil(archived_at)))
    end

    read :with_items do
      # Load items relationship - handled by GraphQL automatically
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the item type (e.g., 'Table', 'Room', 'Equipment')")
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("A detailed description of this item type")
    end

    attribute :color, :string do
      allow_nil?(true)
      public?(true)
      description("Hex color code for visual representation (e.g., '#FF5733')")
    end

    attribute :icon, :string do
      allow_nil?(true)
      public?(true)
      description("Icon identifier for visual representation")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this item type is currently active and can be used")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The business this item type belongs to")
    end

    has_many :items, RivaAsh.Resources.Item do
      destination_attribute(:item_type_id)
      public?(true)
      description("Items of this type")
    end

    has_many :pricing_rules, RivaAsh.Resources.Pricing do
      destination_attribute(:item_type_id)
      public?(true)
      description("Pricing rules for this item type")
    end
  end

  identities do
    identity(:unique_name_per_business, [:name, :business_id])
  end

  validations do
    validate(match(:color, ~r/^#[0-9A-Fa-f]{6}$/),
      message: "Color must be a valid hex color code (e.g., #FF5733)",
      where: [present(:color)]
    )
  end

  # Helper function for admin dropdowns
  def choices_for_select do
    RivaAsh.Resources.ItemType
    |> Ash.read!()
    |> Enum.map(fn item_type ->
      business_name = if item_type.business do
        item_type.business.name
      else
        "Unknown Business"
      end
      {item_type.id, "#{item_type.name} (#{business_name})"}
    end)
  end
end
