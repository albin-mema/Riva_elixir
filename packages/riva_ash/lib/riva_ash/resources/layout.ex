defmodule RivaAsh.Resources.Layout do
  @moduledoc """
  Represents the spatial layout configuration for a plot.
  Layouts define how items are organized within a plot's physical space.

  Layout types:
  - grid: Items arranged in a grid pattern with rows and columns
  - free: Items can be positioned freely with x,y coordinates
  - linear: Items arranged in a single row or column
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
          plot_id: String.t(),
          layout_type: :grid | :free | :linear,
          grid_rows: integer() | nil,
          grid_columns: integer() | nil,
          width: Decimal.t() | nil,
          height: Decimal.t() | nil,
          background_color: String.t() | nil,
          background_image_url: String.t() | nil,
          is_active: boolean(),
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  standard_postgres("layouts")
  standard_archive()

  # Authorization policies
  policies do
    # Admin bypass
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Business owner has full access to their business data
    policy action_type([:read, :create, :update, :destroy]) do
      authorize_if(expr(business.owner_id == ^actor(:id)))
    end

    # Employees with manager role can manage layouts
    policy action_type([:create, :update]) do
      authorize_if(actor_attribute_equals(:role, :manager))
    end

    # Employees can read layouts
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :employee))
    end
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
      get(:by_business, route: "/by-business/:business_id")
      get(:active, route: "/active")
    end
  end

  graphql do
    type(:layout)

    queries do
      get(:get_layout, :read)
      list(:list_layouts, :read)
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
    define(:by_business, args: [:business_id], action: :by_business)
    define(:active, action: :active)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([
        :name,
        :plot_id,
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

      # Note: Layout doesn't have direct business_id, so we validate plot exists
      # Business context is enforced through plot relationship
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_plot do
      argument(:plot_id, :uuid, allow_nil?: false)
      filter(expr(plot_id == ^arg(:plot_id)))
    end

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(plot.business_id == ^arg(:business_id)))
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
    belongs_to :plot, RivaAsh.Resources.Plot do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The plot this layout belongs to")
    end

    has_many :item_positions, RivaAsh.Resources.ItemPosition do
      destination_attribute(:layout_id)
      public?(true)
      description("Item positions within this layout")
    end
  end

  # Helper function for admin dropdowns
  @spec choices_for_select :: [{String.t(), String.t()}]
  def choices_for_select do
    __MODULE__
    |> Ash.read!()
    |> Enum.map(&{&1.id, &1.name})
  end

  # Private helper functions for filtering
  @spec apply_plot_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_plot_filter(query, nil), do: query

  defp apply_plot_filter(query, plot_id) when is_binary(plot_id) do
    query
    |> Ash.Query.filter(plot_id: ^plot_id)
  end

  @spec apply_business_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_business_filter(query, nil), do: query

  defp apply_business_filter(query, business_id) when is_binary(business_id) do
    query
    |> Ash.Query.join(:left, [layout: l], p in assoc(l, :plot), as: :plot)
    |> Ash.Query.filter(plot: [business_id: ^business_id])
  end

  @spec apply_active_filter(Ash.Query.t()) :: Ash.Query.t()
  defp apply_active_filter(query) do
    query
    |> Ash.Query.filter(is_active: true, is_nil: :archived_at)
  end

  @spec apply_layout_type_filter(Ash.Query.t(), atom() | nil) :: Ash.Query.t()
  defp apply_layout_type_filter(query, nil), do: query

  defp apply_layout_type_filter(query, layout_type) when is_atom(layout_type) do
    query
    |> Ash.Query.filter(layout_type: ^layout_type)
  end

  identities do
    identity(:unique_name_per_plot, [:name, :plot_id])
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

  # Helper functions for business logic and data validation

  @doc """
  Checks if the layout is currently active (not archived).

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - `true` if the layout is active, `false` otherwise
  """
  @spec active?(t()) :: boolean()
  def active?(layout) do
    case layout do
      %{archived_at: nil} -> true
      _ -> false
    end
  end

  @doc """
  Gets the layout type as a human-readable string.

  ## Parameters
  - layout: The layout record

  ## Returns
  - String with layout type description
  """
  @spec layout_type_description(t()) :: String.t()
  def layout_type_description(layout) do
    case layout.layout_type do
      :grid -> "Grid layout"
      :free -> "Free-form layout"
      :linear -> "Linear layout"
      _ -> "Unknown layout type"
    end
  end

  @doc """
  Checks if the layout uses grid positioning.

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - `true` if grid layout, `false` otherwise
  """
  @spec grid_layout?(t()) :: boolean()
  def grid_layout?(layout), do: layout.layout_type == :grid

  @doc """
  Checks if the layout uses free-form positioning.

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - `true` if free-form layout, `false` otherwise
  """
  @spec free_form_layout?(t()) :: boolean()
  def free_form_layout?(layout), do: layout.layout_type == :free

  @doc """
  Checks if the layout uses linear positioning.

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - `true` if linear layout, `false` otherwise
  """
  @spec linear_layout?(t()) :: boolean()
  def linear_layout?(layout), do: layout.layout_type == :linear

  @doc """
  Gets the formatted dimensions of the layout.

  ## Parameters
  - layout: The layout record

  ## Returns
  - String with formatted dimensions
  """
  @spec formatted_dimensions(t()) :: String.t()
  def formatted_dimensions(layout) do
    cond do
      not is_nil(layout.width) and not is_nil(layout.height) ->
        "#{Decimal.to_string(layout.width)} x #{Decimal.to_string(layout.height)}"

      not is_nil(layout.width) ->
        "Width: #{Decimal.to_string(layout.width)}"

      not is_nil(layout.height) ->
        "Height: #{Decimal.to_string(layout.height)}"

      true ->
        "No dimensions specified"
    end
  end

  @doc """
  Gets the formatted grid information for grid layouts.

  ## Parameters
  - layout: The layout record

  ## Returns
  - String with grid information or "Not a grid layout"
  """
  @spec grid_info(t()) :: String.t()
  def grid_info(layout) do
    if grid_layout?(layout) do
      case {layout.grid_rows, layout.grid_columns} do
        {rows, cols} when is_integer(rows) and is_integer(cols) and rows > 0 and cols > 0 ->
          "#{rows} x #{cols} grid"

        {rows, _} when is_integer(rows) and rows > 0 ->
          "#{rows} rows"

        {_, cols} when is_integer(cols) and cols > 0 ->
          "#{cols} columns"

        _ ->
          "Invalid grid configuration"
      end
    else
      "Not a grid layout"
    end
  end

  @doc """
  Gets the background information for the layout.

  ## Parameters
  - layout: The layout record

  ## Returns
  - String with background information
  """
  @spec background_info(t()) :: String.t()
  def background_info(layout) do
    cond do
      not is_nil(layout.background_color) and not is_nil(layout.background_image_url) ->
        "Color: #{layout.background_color}, Image: #{layout.background_image_url}"

      not is_nil(layout.background_color) ->
        "Background color: #{layout.background_color}"

      not is_nil(layout.background_image_url) ->
        "Background image: #{layout.background_image_url}"

      true ->
        "No background specified"
    end
  end

  @doc """
  Checks if the layout has a valid background color.

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - `true` if color is valid, `false` otherwise
  """
  @spec has_valid_background_color?(t()) :: boolean()
  def has_valid_background_color?(layout) do
    case layout.background_color do
      nil -> true
      color when is_binary(color) -> String.match?(color, ~r/^#[0-9A-Fa-f]{6}$/)
      _ -> false
    end
  end

  @doc """
  Gets the item position count for this layout.

  ## Parameters
  - layout: The layout record

  ## Returns
  - Integer with the number of item positions
  """
  @spec item_position_count(t()) :: integer()
  def item_position_count(layout) do
    length(layout.item_positions || [])
  end

  @doc """
  Checks if the layout has any item positions.

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - `true` if positions exist, `false` otherwise
  """
  @spec has_item_positions?(t()) :: boolean()
  def has_item_positions?(layout), do: item_position_count(layout) > 0

  @doc """
  Validates that the layout has all required relationships.

  ## Parameters
  - layout: The layout record to validate

  ## Returns
  - `{:ok, layout}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_relationships(layout) do
    cond do
      is_nil(layout.plot) ->
        {:error, "Plot relationship is missing"}

      true ->
        {:ok, layout}
    end
  end

  @doc """
  Gets the plot name associated with this layout.

  ## Parameters
  - layout: The layout record

  ## Returns
  - String with the plot name
  """
  @spec plot_name(t()) :: String.t()
  def plot_name(layout) do
    case layout.plot do
      %{name: name} when is_binary(name) and name != "" -> name
      _ -> "Unknown plot"
    end
  end

  @doc """
  Formats the complete layout information for display.

  ## Parameters
  - layout: The layout record

  ## Returns
  - String with complete layout information
  """
  @spec formatted_info(t()) :: String.t()
  def formatted_info(layout) do
    case is_active?(layout) do
      true ->
        name = layout.name
        plot_name = plot_name(layout)
        layout_type = layout_type_description(layout)
        dimensions = formatted_dimensions(layout)
        grid_info = grid_info(layout)
        background = background_info(layout)
        "#{name} in #{plot_name}: #{layout_type}, #{dimensions}, #{grid_info}, #{background}"
      false ->
        "Archived layout: #{layout.name}"
    end
  end

  @doc """
  Checks if the layout configuration is valid.

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - `true` if configuration is valid, `false` otherwise
  """
  @spec valid_configuration?(t()) :: boolean()
  def valid_configuration?(layout) do
    cond do
      # Grid layouts must have rows and columns
      grid_layout?(layout) ->
        is_integer(layout.grid_rows) and layout.grid_rows > 0 and
          is_integer(layout.grid_columns) and layout.grid_columns > 0

      # Free-form and linear layouts can have optional dimensions
      true ->
        true
    end
  end

  @doc """
  Gets the business name associated with this layout.

  ## Parameters
  - layout: The layout record

  ## Returns
  - String with the business name
  """
  @spec business_name(t()) :: String.t()
  def business_name(layout) do
    case layout.plot do
      %{business: %{name: name}} when is_binary(name) and name != "" -> name
      _ -> "Unknown business"
    end
  end

  @doc """
  Checks if the layout can be safely deleted.

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - `true` if can be deleted, `false` otherwise
  """
  @spec can_delete?(t()) :: boolean()
  def can_delete?(layout) do
    not has_item_positions?(layout)
  end

  @doc """
  Gets the reason why the layout cannot be deleted.

  ## Parameters
  - layout: The layout record to check

  ## Returns
  - String with deletion reason or empty string if can be deleted
  """
  @spec deletion_reason(t()) :: String.t()
  def deletion_reason(layout) do
    if has_item_positions?(layout) do
      "Cannot delete layout with associated item positions"
    else
      ""
    end
  end

  # Helper function for admin dropdowns
  @spec choices_for_select :: [{String.t(), String.t()}]
  def choices_for_select do
    __MODULE__
    |> Ash.read!()
    |> Enum.map(&{&1.id, &1.name})
  end

  # Private helper functions for filtering
  @spec apply_plot_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
end
