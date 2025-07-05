defmodule RivaAsh.Resources.Layout do
  @moduledoc """
  Represents the spatial layout configuration for a section.
  Layouts define how items are organized within a section's physical space.
  
  Layout types:
  - grid: Items arranged in a grid pattern with rows and columns
  - free: Items can be positioned freely with x,y coordinates
  - linear: Items arranged in a single row or column
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshGraphql.Resource, AshArchival.Resource]

  postgres do
    table("layouts")
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
    type("layout")

    routes do
      base("/layouts")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for layout specific actions
      get(:by_section, route: "/by-section/:section_id")
      get(:by_business, route: "/by-business/:business_id")
      get(:active, route: "/active")
    end
  end

  graphql do
    type(:layout)

    queries do
      get(:get_layout, :read)
      list(:list_layouts, :read)
      list(:layouts_by_section, :by_section)
      list(:layouts_by_business, :by_business)
      list(:active_layouts, :active)
    end

    mutations do
      create(:create_layout, :create)
      update(:update_layout, :update)
      destroy(:delete_layout, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_section, args: [:section_id], action: :by_section)
    define(:by_business, args: [:business_id], action: :by_business)
    define(:active, action: :active)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([
        :name,
        :section_id,
        :layout_type,
        :grid_rows,
        :grid_columns,
        :width,
        :height,
        :background_color,
        :background_image_url,
        :is_active
      ])
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

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(section.business_id == ^arg(:business_id)))
    end

    read :active do
      filter(expr(is_active == true and is_nil(archived_at)))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the layout (e.g., 'Main Floor', 'Dining Area')")
    end

    attribute :layout_type, :atom do
      constraints(one_of: [:grid, :free, :linear])
      default(:grid)
      public?(true)
      description("Type of layout: grid, free, or linear")
    end

    attribute :grid_rows, :integer do
      allow_nil?(true)
      public?(true)
      constraints(min: 1, max: 100)
      description("Number of rows in grid layout (required for grid type)")
    end

    attribute :grid_columns, :integer do
      allow_nil?(true)
      public?(true)
      constraints(min: 1, max: 100)
      description("Number of columns in grid layout (required for grid type)")
    end

    attribute :width, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Width of the layout area (in meters or pixels)")
    end

    attribute :height, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Height of the layout area (in meters or pixels)")
    end

    attribute :background_color, :string do
      allow_nil?(true)
      public?(true)
      description("Background color for the layout (hex color code)")
    end

    attribute :background_image_url, :string do
      allow_nil?(true)
      public?(true)
      description("URL to background image for the layout")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this layout is currently active")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :section, RivaAsh.Resources.Section do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The section this layout belongs to")
    end

    has_many :item_positions, RivaAsh.Resources.ItemPosition do
      destination_attribute(:layout_id)
      public?(true)
      description("Item positions within this layout")
    end
  end

  identities do
    identity(:unique_name_per_section, [:name, :section_id])
  end

  validations do
    validate(match(:background_color, ~r/^#[0-9A-Fa-f]{6}$/),
      message: "Background color must be a valid hex color code (e.g., #FF5733)",
      where: [present(:background_color)]
    )

    validate(present([:grid_rows, :grid_columns]),
      message: "Grid rows and columns are required for grid layout type",
      where: [attribute_equals(:layout_type, :grid)]
    )

    validate(compare(:width, greater_than: 0),
      message: "Width must be greater than 0",
      where: [present(:width)]
    )

    validate(compare(:height, greater_than: 0),
      message: "Height must be greater than 0",
      where: [present(:height)]
    )
  end
end
