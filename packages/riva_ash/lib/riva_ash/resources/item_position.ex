defmodule RivaAsh.Resources.ItemPosition do
  @moduledoc """
  Represents the physical position of an item within a layout.
  This resource tracks where items are placed in the spatial organization of a section.

  Position types:
  - Grid-based: Uses row and column coordinates
  - Free-form: Uses x,y coordinates
  - Linear: Uses sequence/order position

  This resource manages spatial positioning of items within layouts, supporting
  multiple positioning systems and ensuring no overlaps between items.
  """

  @type t :: %__MODULE__{
          id: String.t(),
          item_id: String.t(),
          layout_id: String.t(),
          grid_row: integer() | nil,
          grid_column: integer() | nil,
          x_coordinate: Decimal.t() | nil,
          y_coordinate: Decimal.t() | nil,
          width: Decimal.t() | nil,
          height: Decimal.t() | nil,
          rotation_degrees: Decimal.t(),
          z_index: integer(),
          is_visible: boolean(),
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshGraphql.Resource, AshArchival.Resource]

  import Ash.Expr
  require Ash.Query
  alias RivaAsh.Resources.Layout

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

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_item_layout_business_match/2)
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
    # Ensure dimensions are positive
    validate(compare(:width, greater_than: 0),
      message: "Width must be greater than 0",
      where: [present(:width)]
    )

    validate(compare(:height, greater_than: 0),
      message: "Height must be greater than 0",
      where: [present(:height)]
    )

    # Custom validation to ensure position is within layout bounds
    validate(
      fn changeset, _context ->
        layout_id = Ash.Changeset.get_attribute(changeset, :layout_id)
        grid_row = Ash.Changeset.get_attribute(changeset, :grid_row)
        grid_column = Ash.Changeset.get_attribute(changeset, :grid_column)
        width = Ash.Changeset.get_attribute(changeset, :width) || 1
        height = Ash.Changeset.get_attribute(changeset, :height) || 1

        if layout_id && grid_row && grid_column do
          case check_layout_bounds(layout_id, grid_row, grid_column, width, height) do
            :ok -> :ok
            {:error, message} -> {:error, field: :grid_row, message: message}
          end
        else
          :ok
        end
      end,
      message: "Position must be within layout boundaries"
    )

    # Custom validation to prevent overlapping positions
    validate(
      fn changeset, _context ->
        layout_id = Ash.Changeset.get_attribute(changeset, :layout_id)
        grid_row = Ash.Changeset.get_attribute(changeset, :grid_row)
        grid_column = Ash.Changeset.get_attribute(changeset, :grid_column)
        width = Ash.Changeset.get_attribute(changeset, :width) || 1
        height = Ash.Changeset.get_attribute(changeset, :height) || 1

        if layout_id && grid_row && grid_column do
          case check_position_overlap(layout_id, grid_row, grid_column, width, height, changeset) do
            :ok -> :ok
            {:error, message} -> {:error, field: :grid_row, message: message}
          end
        else
          :ok
        end
      end,
      message: "Position overlaps with existing item"
    )
  end

  calculations do
    calculate :is_within_bounds, :boolean do
      public?(true)
      description("Whether the position is within the layout bounds")

      calculation(fn records, _context ->
        Enum.map(records, fn record ->
          case get_layout_dimensions(record.layout_id) do
            {:ok, {max_rows, max_columns}} ->
              width = record.width || 1
              height = record.height || 1

              record.grid_row >= 1 &&
                record.grid_column >= 1 &&
                record.grid_row + height - 1 <= max_rows &&
                record.grid_column + width - 1 <= max_columns

            {:error, _unmatched} ->
              false
          end
        end)
      end)
    end

    calculate :occupied_cells, {:array, :map} do
      public?(true)
      description("List of all grid cells occupied by this item")

      calculation(fn records, _context ->
        Enum.map(records, fn record ->
          width = record.width || 1
          height = record.height || 1

          for row <- record.grid_row..(record.grid_row + height - 1),
              col <- record.grid_column..(record.grid_column + width - 1) do
            %{row: row, column: col}
          end
        end)
      end)
    end
  end

  # Helper function to check layout bounds
  defp check_layout_bounds(layout_id, grid_row, grid_column, width, height) do
    case get_layout_dimensions(layout_id) do
      {:ok, {max_rows, max_columns}} ->
        cond do
          grid_row < 1 || grid_column < 1 ->
            {:error, "Grid position must be positive (starting from 1)"}

          grid_row + height - 1 > max_rows ->
            {:error, "Item extends beyond layout height (#{max_rows} rows)"}

          grid_column + width - 1 > max_columns ->
            {:error, "Item extends beyond layout width (#{max_columns} columns)"}

          true ->
            :ok
        end

      {:error, _} ->
        {:error, "Could not validate layout bounds"}
    end
  end

  # Helper function to check for position overlaps
  defp check_position_overlap(layout_id, grid_row, grid_column, width, height, changeset) do
    import Ash.Expr

    # Get the ID of the current record if it's an update
    current_id = Ash.Changeset.get_data(changeset, :id)

    # Calculate all cells this item will occupy
    occupied_cells =
      for row <- grid_row..(grid_row + height - 1),
          col <- grid_column..(grid_column + width - 1) do
        {row, col}
      end

    # Find existing positions in the same layout
    query =
      __MODULE__
      |> Ash.Query.filter(expr(layout_id == ^layout_id))
      |> Ash.Query.load([:occupied_cells])

    # Exclude current record if this is an update
    query =
      if current_id do
        Ash.Query.filter(query, expr(id != ^current_id))
      else
        query
      end

    case Ash.read(query, domain: RivaAsh.Domain) do
      {:ok, existing_positions} ->
        overlapping =
          Enum.any?(existing_positions, fn pos ->
            existing_cells = Enum.map(pos.occupied_cells, fn cell -> {cell.row, cell.column} end)
            Enum.any?(occupied_cells, fn cell -> cell in existing_cells end)
          end)

        if overlapping do
          {:error, "Position overlaps with existing item"}
        else
          :ok
        end

      {:error, _} ->
        # If we can't check, allow the operation
        :ok
    end
  end

  # Helper functions for business logic and data validation

  @doc """
  Checks if the item position is currently active (not archived).

  ## Parameters
  - item_position: The item position record to check

  ## Returns
  - `true` if the position is active, `false` otherwise
  """
  @spec active?(t()) :: boolean()
  def active?(item_position) do
    case item_position do
      %{archived_at: nil} -> true
      _ -> false
    end
  end

  @doc """
  Checks if the item position uses grid-based positioning.

  ## Parameters
  - item_position: The item position record to check

  ## Returns
  - `true` if using grid positioning, `false` otherwise
  """
  @spec grid_positioned?(t()) :: boolean()
  def grid_positioned?(item_position) do
    not (is_nil(item_position.grid_row) or is_nil(item_position.grid_column))
  end

  @doc """
  Checks if the item position uses free-form positioning.

  ## Parameters
  - item_position: The item position record to check

  ## Returns
  - `true` if using free-form positioning, `false` otherwise
  """
  @spec free_form_positioned?(t()) :: boolean()
  def free_form_positioned?(item_position) do
    not (is_nil(item_position.x_coordinate) or is_nil(item_position.y_coordinate))
  end

  @doc """
  Gets the position type as an atom.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - `:grid`, `:free_form`, or `:none`
  """
  @spec position_type(t()) :: :grid | :free_form | :none
  def position_type(item_position) do
    cond do
      grid_positioned?(item_position) -> :grid
      free_form_positioned?(item_position) -> :free_form
      true -> :none
    end
  end

  @doc """
  Gets the formatted position information.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - String with formatted position information
  """
  @spec formatted_position(t()) :: String.t()
  def formatted_position(item_position) do
    case position_type(item_position) do
      :grid ->
        "Grid position: Row #{item_position.grid_row}, Column #{item_position.grid_column}"

      :free_form ->
        "Free-form position: X #{item_position.x_coordinate}, Y #{item_position.y_coordinate}"

      :none ->
        "No position specified"
    end
  end

  @doc """
  Gets the formatted dimensions information.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - String with formatted dimensions information
  """
  @spec formatted_dimensions(t()) :: String.t()
  def formatted_dimensions(item_position) do
    cond do
      not is_nil(item_position.width) and not is_nil(item_position.height) ->
        "#{item_position.width} x #{item_position.height}"

      not is_nil(item_position.width) ->
        "Width: #{item_position.width}"

      not is_nil(item_position.height) ->
        "Height: #{item_position.height}"

      true ->
        "No dimensions specified"
    end
  end

  @doc """
  Gets the formatted rotation information.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - String with formatted rotation information
  """
  @spec formatted_rotation(t()) :: String.t()
  def formatted_rotation(item_position) do
    case item_position.rotation_degrees do
      0 -> "No rotation"
      rotation -> "#{rotation}°"
    end
  end

  @doc """
  Checks if the item is visible in the layout.

  ## Parameters
  - item_position: The item position record to check

  ## Returns
  - `true` if visible, `false` otherwise
  """
  @spec visible?(t()) :: boolean()
  def visible?(item_position), do: item_position.is_visible

  @doc """
  Gets the z-index information for layering.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - String with z-index information
  """
  @spec z_index_info(t()) :: String.t()
  def z_index_info(item_position) do
    "Z-index: #{item_position.z_index}"
  end

  @doc """
  Checks if the item position overlaps with another position.

  ## Parameters
  - item_position: The item position record to check
  - other_position: Another item position record to compare against

  ## Returns
  - `true` if overlapping, `false` otherwise
  """
  @spec overlaps_with?(t(), t()) :: boolean()
  def overlaps_with?(item_position, other_position) do
    # Only check if both positions use grid positioning
    if grid_positioned?(item_position) and grid_positioned?(other_position) do
      width1 = item_position.width || 1
      height1 = item_position.height || 1
      width2 = other_position.width || 1
      height2 = other_position.height || 1

      # Check if rectangles overlap
      not (item_position.grid_row + height1 - 1 < other_position.grid_row or
             other_position.grid_row + height2 - 1 < item_position.grid_row or
             item_position.grid_column + width1 - 1 < other_position.grid_column or
             other_position.grid_column + width2 - 1 < item_position.grid_column)
    else
      false
    end
  end

  @doc """
  Validates that the item position has all required relationships.

  ## Parameters
  - item_position: The item position record to validate

  ## Returns
  - `{:ok, item_position}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_relationships(item_position) do
    cond do
      is_nil(item_position.item) ->
        {:error, "Item relationship is missing"}

      is_nil(item_position.layout) ->
        {:error, "Layout relationship is missing"}

      true ->
        {:ok, item_position}
    end
  end

  @doc """
  Gets the item name associated with this position.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - String with the item name
  """
  @spec item_name(t()) :: String.t()
  def item_name(item_position) do
    case item_position.item do
      %{name: name} when is_binary(name) and name != "" -> name
      _ -> "Unknown item"
    end
  end

  @doc """
  Gets the layout name associated with this position.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - String with the layout name
  """
  @spec layout_name(t()) :: String.t()
  def layout_name(item_position) do
    case item_position.layout do
      %{name: name} when is_binary(name) and name != "" -> name
      _ -> "Unknown layout"
    end
  end

  @doc """
  Formats the complete item position information for display.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - String with complete position information
  """
  @spec formatted_info(t()) :: String.t()
  def formatted_info(item_position) do
    with true <- active?(item_position),
         item_name <- item_name(item_position),
         layout_name <- layout_name(item_position),
         position <- formatted_position(item_position),
         dimensions <- formatted_dimensions(item_position),
         rotation <- formatted_rotation(item_position) do
      "#{item_name} in #{layout_name}: #{position}, #{dimensions}, #{rotation}"
    else
      false ->
        "Archived position for #{item_name(item_position)}"
    end
  end

  @doc """
  Calculates the total area occupied by the item in the layout.

  ## Parameters
  - item_position: The item position record

  ## Returns
  - Decimal with the total area, or 0 if no dimensions specified
  """
  @spec total_area(t()) :: Decimal.t()
  def total_area(item_position) do
    with width when not is_nil(width) <- item_position.width,
         height when not is_nil(height) <- item_position.height do
      Decimal.mult(width, height)
    else
      _ -> Decimal.new(0)
    end
  end

  @doc """
  Checks if the item position has valid dimensions.

  ## Parameters
  - item_position: The item position record to check

  ## Returns
  - `true` if dimensions are valid, `false` otherwise
  """
  @spec valid_dimensions?(t()) :: boolean()
  def valid_dimensions?(item_position) do
    cond do
      is_nil(item_position.width) and is_nil(item_position.height) ->
        true

      not is_nil(item_position.width) and Decimal.compare(item_position.width, Decimal.new(0)) == :lt ->
        false

      not is_nil(item_position.height) and Decimal.compare(item_position.height, Decimal.new(0)) == :lt ->
        false

      true ->
        true
    end
  end

  # Helper function to get layout dimensions
  defp get_layout_dimensions(layout_id) do
    case Layout.by_id(layout_id) do
      {:ok, layout} ->
        {:ok, {layout.grid_rows, layout.grid_columns}}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
