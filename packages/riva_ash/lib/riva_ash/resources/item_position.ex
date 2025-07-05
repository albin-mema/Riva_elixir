defmodule RivaAsh.Resources.ItemPosition do
  @moduledoc """
  Represents the physical position of an item within a layout.
  This resource tracks where items are placed in the spatial organization of a section.

  Position types:
  - Grid-based: Uses row and column coordinates
  - Free-form: Uses x,y coordinates
  - Linear: Uses sequence/order position
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshGraphql.Resource, AshArchival.Resource]

  postgres do
    table("item_positions")
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
    type("item_position")

    routes do
      base("/item-positions")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for item position specific actions
      get(:by_layout, route: "/by-layout/:layout_id")
      get(:by_item, route: "/by-item/:item_id")
      get(:by_section, route: "/by-section/:section_id")
    end
  end

  graphql do
    type(:item_position)

    queries do
      get(:get_item_position, :read)
      list(:list_item_positions, :read)
      list(:positions_by_layout, :by_layout)
      list(:positions_by_item, :by_item)
      list(:positions_by_section, :by_section)
    end

    mutations do
      create(:create_item_position, :create)
      update(:update_item_position, :update)
      destroy(:delete_item_position, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_layout, args: [:layout_id], action: :by_layout)
    define(:by_item, args: [:item_id], action: :by_item)
    define(:by_section, args: [:section_id], action: :by_section)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([
        :item_id,
        :layout_id,
        :grid_row,
        :grid_column,
        :x_coordinate,
        :y_coordinate,
        :width,
        :height,
        :rotation_degrees,
        :z_index,
        :is_visible
      ])
      primary?(true)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_layout do
      argument(:layout_id, :uuid, allow_nil?: false)
      filter(expr(layout_id == ^arg(:layout_id)))
    end

    read :by_item do
      argument(:item_id, :uuid, allow_nil?: false)
      filter(expr(item_id == ^arg(:item_id)))
    end

    read :by_section do
      argument(:section_id, :uuid, allow_nil?: false)
      filter(expr(layout.section_id == ^arg(:section_id)))
    end
  end

  attributes do
    uuid_primary_key(:id)

    # Grid-based positioning (for grid layouts)
    attribute :grid_row, :integer do
      allow_nil?(true)
      public?(true)
      constraints(min: 1)
      description("Row position in grid layout (1-based)")
    end

    attribute :grid_column, :integer do
      allow_nil?(true)
      public?(true)
      constraints(min: 1)
      description("Column position in grid layout (1-based)")
    end

    # Free-form positioning (for free layouts)
    attribute :x_coordinate, :decimal do
      allow_nil?(true)
      public?(true)
      description("X coordinate for free-form positioning")
    end

    attribute :y_coordinate, :decimal do
      allow_nil?(true)
      public?(true)
      description("Y coordinate for free-form positioning")
    end

    # Visual properties
    attribute :width, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Width of the item in the layout")
    end

    attribute :height, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Height of the item in the layout")
    end

    attribute :rotation_degrees, :decimal do
      allow_nil?(true)
      default(0)
      public?(true)
      constraints(min: 0, max: 360)
      description("Rotation of the item in degrees (0-360)")
    end

    attribute :z_index, :integer do
      allow_nil?(true)
      default(0)
      public?(true)
      description("Z-index for layering (higher values appear on top)")
    end

    attribute :is_visible, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether the item is visible in the layout")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :item, RivaAsh.Resources.Item do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The item being positioned")
    end

    belongs_to :layout, RivaAsh.Resources.Layout do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The layout this position belongs to")
    end
  end

  identities do
    # Ensure each item can only have one position per layout
    identity(:unique_item_per_layout, [:item_id, :layout_id])

    # Ensure grid positions are unique within a layout (when both are present)
    identity(:unique_grid_position, [:layout_id, :grid_row, :grid_column])
  end

  validations do
    # Grid positioning validation - simplified for now
    # TODO: Add custom validation functions for layout-specific rules

    # Ensure grid position is within layout bounds
    # Note: These validations would need custom validation functions
    # For now, we'll rely on application-level validation

    # Ensure dimensions are positive
    validate(compare(:width, greater_than: 0),
      message: "Width must be greater than 0",
      where: [present(:width)]
    )

    validate(compare(:height, greater_than: 0),
      message: "Height must be greater than 0",
      where: [present(:height)]
    )
  end

  # TODO: Add calculations for position validation
  # calculations do
  #   calculate :is_within_bounds, :boolean, expr(true) do
  #     public?(true)
  #     description("Whether the position is within the layout bounds")
  #   end
  # end
end
