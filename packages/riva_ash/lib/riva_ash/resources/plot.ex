defmodule RivaAsh.Resources.Plot do
  @moduledoc """
  Represents a physical land area owned or managed by a business.
  Plots are the top-level spatial organization unit that can contain multiple sections.

  A plot represents a real-world physical space that a business manages,
  such as a fairground, market area, parking lot, or event venue.
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

  @derive {
    Flop.Schema,
    filterable: [:name, :description, :business_id, :is_active],
    sortable: [:name, :description, :inserted_at, :updated_at, :is_active],
    default_limit: 10,
    max_limit: 100
  }

  import RivaAsh.ResourceHelpers

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          description: String.t() | nil,
          business_id: String.t(),
          address: String.t() | nil,
          total_area: Decimal.t() | nil,
          area_unit: String.t(),
          coordinates: map() | nil,
          is_active: boolean(),
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  standard_postgres("plots")
  standard_archive()
  standard_admin([:name, :business, :description, :total_area, :is_active])

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

    # Employees with manager role can manage plots
    policy action_type([:create, :update]) do
      authorize_if(actor_attribute_equals(:role, :manager))
    end

    # Employees can read plots
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :employee))
    end
  end

  json_api do
    type("plot")

    routes do
      base("/plots")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for plot specific actions
      get(:by_business, route: "/by-business/:business_id")
      get(:active, route: "/active")
      get(:with_sections, route: "/with-sections")
    end
  end

  graphql do
    type(:plot)

    queries do
      get(:get_plot, :read)
      list(:list_plots, :read)
      list(:plots_by_business, :by_business)
      list(:active_plots, :active)
    end

    mutations do
      create(:create_plot, :create)
      update(:update_plot, :update)
      destroy(:delete_plot, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_business, args: [:business_id], action: :by_business)
  end

  policies do
    # Default policy - deny all
    default_access_type(:strict)

    # Allow reading plots for authenticated users
    policy action_type(:read) do
      authorize_if(always())
    end

    # Allow business owners to manage their plots
    policy action_type([:create, :update, :destroy]) do
      authorize_if(relates_to_actor_via(:business))
    end

    # Allow employees to read plots for their business
    policy action_type(:read) do
      authorize_if(relates_to_actor_via([:business, :employees]))
    end
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([
        :name,
        :description,
        :business_id,
        :address,
        :total_area,
        :area_unit,
        :coordinates,
        :is_active
      ])

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

    read :with_sections do
      # Load sections relationship - handled by GraphQL automatically
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the plot")
      constraints(min_length: 2, max_length: 100, trim?: true)
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("A detailed description of the plot")
      constraints(max_length: 1000, trim?: true)
    end

    attribute :address, :string do
      allow_nil?(true)
      public?(true)
      description("Physical address of the plot")
      constraints(max_length: 500, trim?: true)
    end

    attribute :total_area, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Total area of the plot")
    end

    attribute :area_unit, :string do
      allow_nil?(true)
      public?(true)
      default("sqft")
      constraints(max_length: 10)
      description("Unit of measurement for the area (sqft, sqm, acres, etc.)")
    end

    attribute :coordinates, :map do
      allow_nil?(true)
      public?(true)
      description("GPS coordinates or boundary coordinates of the plot")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether the plot is currently active and available for use")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
    create_timestamp(:archived_at)
  end

  # Helper functions for business logic and data validation

  @doc """
  Checks if the plot is currently active (not archived).

  ## Parameters
  - plot: The plot record to check

  ## Returns
  - `true` if the plot is active, `false` otherwise
  """
  @spec active?(t()) :: boolean()
  def active?(plot) do
    case plot do
      %{archived_at: nil} -> true
      _ -> false
    end
  end

  @doc """
  Checks if the plot has a valid area measurement.

  ## Parameters
  - plot: The plot record to check

  ## Returns
  - `true` if has valid area, `false` otherwise
  """
  @spec has_valid_area?(t()) :: boolean()
  def has_valid_area?(plot) do
    cond do
      is_nil(plot.total_area) -> false
      Decimal.compare(plot.total_area, Decimal.new(0)) == :lt -> false
      true -> true
    end
  end

  @doc """
  Gets the area unit description as a human-readable string.

  ## Parameters
  - plot: The plot record

  ## Returns
  - String with area unit description
  """
  @spec area_unit_description(t()) :: String.t()
  def area_unit_description(plot) do
    case plot.area_unit do
      "sqft" -> "Square Feet"
      "sqm" -> "Square Meters"
      "acres" -> "Acres"
      "hectares" -> "Hectares"
      "sqmi" -> "Square Miles"
      "sqkm" -> "Square Kilometers"
      _ -> plot.area_unit || "Unknown"
    end
  end

  @doc """
  Gets the formatted total area.

  ## Parameters
  - plot: The plot record

  ## Returns
  - String with formatted total area or "No area specified"
  """
  @spec formatted_total_area(t()) :: String.t()
  def formatted_total_area(plot) do
    cond do
      is_nil(plot.total_area) -> "No area specified"
      has_valid_area?(plot) -> "#{Decimal.to_string(plot.total_area)} #{area_unit_description(plot)}"
      true -> "Invalid area"
    end
  end

  @doc """
  Gets the formatted address.

  ## Parameters
  - plot: The plot record

  ## Returns
  - String with formatted address or "No address specified"
  """
  @spec formatted_address(t()) :: String.t()
  def formatted_address(plot) do
    case plot.address do
      nil ->
        "No address specified"

      address when is_binary(address) ->
        if String.trim(address) != "" do
          address
        else
          "No address specified"
        end

      _ ->
        "No address specified"
    end
  end

  @doc """
  Gets the section count for this plot.

  ## Parameters
  - plot: The plot record

  ## Returns
  - Integer with section count
  """
  @spec section_count(t()) :: non_neg_integer()
  def section_count(plot) do
    case plot.sections do
      nil -> 0
      sections when is_list(sections) -> length(sections)
      _ -> 0
    end
  end

  @doc """
  Checks if this plot has any sections.

  ## Parameters
  - plot: The plot record to check

  ## Returns
  - `true` if sections exist, `false` otherwise
  """
  @spec has_sections?(t()) :: boolean()
  def has_sections?(plot), do: section_count(plot) > 0

  @doc """
  Gets the layout count for this plot.

  ## Parameters
  - plot: The plot record

  ## Returns
  - Integer with layout count
  """
  @spec layout_count(t()) :: non_neg_integer()
  def layout_count(plot) do
    case plot.layouts do
      nil -> 0
      layouts when is_list(layouts) -> length(layouts)
      _ -> 0
    end
  end

  @doc """
  Checks if this plot has any layouts.

  ## Parameters
  - plot: The plot record to check

  ## Returns
  - `true` if layouts exist, `false` otherwise
  """
  @spec has_layouts?(t()) :: boolean()
  def has_layouts?(plot), do: layout_count(plot) > 0

  @doc """
  Validates that the plot has all required relationships.

  ## Parameters
  - plot: The plot record to validate

  ## Returns
  - `{:ok, plot}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_relationships(plot) do
    cond do
      is_nil(plot.business) ->
        {:error, "Business relationship is missing"}

      true ->
        {:ok, plot}
    end
  end

  @doc """
  Gets the business name for this plot.

  ## Parameters
  - plot: The plot record

  ## Returns
  - String with business name
  """
  @spec business_name(t()) :: String.t()
  def business_name(plot) do
    case plot.business do
      %{name: name} when is_binary(name) and name != "" -> name
      _ -> "Unknown business"
    end
  end

  @doc """
  Formats the complete plot information for display.

  ## Parameters
  - plot: The plot record

  ## Returns
  - String with complete plot information
  """
  @spec formatted_info(t()) :: String.t()
  def formatted_info(plot) do
    case active?(plot) do
      true ->
        business_name = business_name(plot)
        address = formatted_address(plot)
        total_area = formatted_total_area(plot)
        section_count = section_count(plot)
        layout_count = layout_count(plot)
        "#{business_name} - #{plot.name}: #{address}, Area: #{total_area}, Sections: #{section_count}, Layouts: #{layout_count}"
      false ->
        "Archived plot: #{plot.name}"
    end
  end

  @doc """
  Checks if the plot can be deleted.

  ## Parameters
  - plot: The plot record to check

  ## Returns
  - `true` if can be deleted, `false` otherwise
  """
  @spec can_delete?(t()) :: boolean()
  def can_delete?(plot) do
    not has_sections?(plot) and not has_layouts?(plot) and active?(plot)
  end

  @doc """
  Gets the deletion reason for a plot.

  ## Parameters
  - plot: The plot record to check

  ## Returns
  - String with deletion reason or nil if can be deleted
  """
  @spec deletion_reason(t()) :: String.t() | nil
  def deletion_reason(plot) do
    cond do
      not active?(plot) ->
        "Plot is already archived"

      has_sections?(plot) ->
        "Cannot delete plot with sections"

      has_layouts?(plot) ->
        "Cannot delete plot with layouts"

      true ->
        nil
    end
  end

  @doc """
  Validates the plot data.

  ## Parameters
  - plot: The plot record to validate

  ## Returns
  - `{:ok, plot}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_data(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_data(plot) do
    cond do
      String.trim(plot.name) == "" ->
        {:error, "Name cannot be empty"}

      not has_valid_area?(plot) and not is_nil(plot.total_area) ->
        {:error, "Total area must be greater than or equal to 0"}

      not is_nil(plot.area_unit) and String.trim(plot.area_unit) == "" ->
        {:error, "Area unit cannot be empty"}

      true ->
        {:ok, plot}
    end
  end

  @doc """
  Gets the plot name with business prefix.

  ## Parameters
  - plot: The plot record

  ## Returns
  - String with formatted plot name
  """
  @spec prefixed_name(t()) :: String.t()
  def prefixed_name(plot) do
    "#{business_name(plot)} - #{plot.name}"
  end

  @doc """
  Checks if the plot has coordinates.

  ## Parameters
  - plot: The plot record to check

  ## Returns
  - `true` if has coordinates, `false` otherwise
  """
  @spec has_coordinates?(t()) :: boolean()
  def has_coordinates?(plot), do: not is_nil(plot.coordinates)

  @doc """
  Gets the coordinate information as a formatted string.

  ## Parameters
  - plot: The plot record

  ## Returns
  - String with coordinate information or "No coordinates specified"
  """
  @spec formatted_coordinates(t()) :: String.t()
  def formatted_coordinates(plot) do
    case plot.coordinates do
      nil ->
        "No coordinates specified"

      coordinates when is_map(coordinates) ->
        case {Map.get(coordinates, :lat), Map.get(coordinates, :lng)} do
          {lat, lng} when is_number(lat) and is_number(lng) ->
            "Lat: #{lat}, Lng: #{lng}"

          _ ->
            "Coordinates: #{inspect(coordinates)}"
        end

      _ ->
        "Invalid coordinates"
    end
  end

  @doc """
  Gets all active plots for a business.

  ## Parameters
  - plots: List of plot records
  - business_id: The business ID to filter by

  ## Returns
  - List of active plots for the business
  """
  @spec active_for_business([t()], String.t()) :: [t()]
  def active_for_business(plots, business_id) do
    plots
    |> Enum.filter(&(&1.business_id == business_id))
    |> Enum.filter(&active?/1)
  end

  @doc """
  Gets all inactive plots for a business.

  ## Parameters
  - plots: List of plot records
  - business_id: The business ID to filter by

  ## Returns
  - List of inactive plots for the business
  """
  @spec inactive_for_business([t()], String.t()) :: [t()]
  def inactive_for_business(plots, business_id) do
    plots
    |> Enum.filter(&(&1.business_id == business_id))
    |> Enum.filter(&(not active?(&1)))
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The business that owns or manages this plot")
    end

    has_many :sections, RivaAsh.Resources.Section do
      destination_attribute(:plot_id)
      public?(true)
      description("Sections within this plot")
    end

    has_many :layouts, RivaAsh.Resources.Layout do
      destination_attribute(:plot_id)
      public?(true)
      description("Visual layouts for this plot")
    end
  end

  identities do
    identity(:unique_name_per_business, [:name, :business_id])
  end

  # Helper function for admin dropdowns
  @spec choices_for_select :: [{String.t(), String.t()}]
  def choices_for_select do
    __MODULE__
    |> Ash.read!()
    |> Enum.map(&{&1.id, &1.name})
  end

  # Private helper functions for filtering
  @spec apply_business_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_business_filter(query, nil), do: query

  defp apply_business_filter(query, business_id) do
    Ash.Query.filter(query, expr(business_id == ^business_id))
  end

  @spec apply_active_filter(Ash.Query.t()) :: Ash.Query.t()
  defp apply_active_filter(query) do
    Ash.Query.filter(query, expr(active == true and is_nil(archived_at)))
  end
end
