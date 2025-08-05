defmodule RivaAshWeb.Components.Forms.PlotForm do
  @moduledoc """
  Plot creation and editing form component.
  
  This component follows the functional core, imperative shell pattern,
  with pure functions for data transformation and validation, and
  the LiveView component handling UI state and side effects.
  
  ## Styleguide Compliance
  
  This module follows the Riva Ash styleguide principles:
  
  - **Functional Programming**: Uses pure functions, pattern matching, and pipelines
  - **Type Safety**: Comprehensive type specifications with @spec annotations
  - **Single Level of Abstraction**: Each function has a clear, focused responsibility
  - **Error Handling**: Consistent use of result tuples and guard clauses
  - **Immutable Data**: All transformations use immutable data structures
  - **Security**: Proper input validation and safe rendering patterns
  - **Phoenix/Ash Integration**: Follows Phoenix LiveView and Ash framework patterns
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button

  @type assigns :: %{
    optional(:form) => map(),
    optional(:businesses) => list(),
    optional(:editing) => boolean(),
    optional(:on_submit) => String.t(),
    optional(:on_change) => String.t(),
    optional(:on_cancel) => String.t(),
    optional(:loading) => boolean(),
    optional(:class) => String.t(),
    optional(:rest) => any()
  }

  @type plot_form_data :: %{
    name: String.t(),
    description: String.t(),
    business_id: String.t() | integer(),
    address: String.t(),
    total_area: number(),
    latitude: number(),
    longitude: number()
  }

  @doc """
  Renders a plot form.
  
  ## Examples
      <.plot_form
        form={@form}
        businesses={@businesses}
        editing={@editing}
        on_submit="save_plot"
        on_change="validate_plot"
        on_cancel="cancel_plot"
        loading={@loading}
      />
  """
  attr(:form, :map, required: true)
  attr(:businesses, :list, default: [])
  attr(:editing, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec plot_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def plot_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_plot_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:businesses, [])
    |> Map.put_new(:editing, false)
    |> Map.put_new(:loading, false)
    |> Map.put_new(:class, "")
  end

  @spec render_plot_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_plot_form(assigns) do
    ~H"""
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.render_basic_fields form={@form} />
      <.render_location_fields form={@form} />
      <.render_form_actions editing={@editing} loading={@loading} on_cancel={@on_cancel} />
    </form>
    """
  end

  @spec render_basic_fields(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_basic_fields(assigns) do
    ~H"""
    <.form_field field={@form[:name]} label="Plot Name" required />
    <.textarea_field field={@form[:description]} label="Description" />
    <.select_field field={@form[:business_id]} label="Business" options={@businesses} required />
    <.textarea_field field={@form[:address]} label="Address" />
    <.form_field field={@form[:total_area]} label="Total Area (sq ft)" type="number" />
    """
  end

  @spec render_location_fields(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_location_fields(assigns) do
    ~H"""
    <div>
      <h3>Location Details</h3>
      <.form_field field={@form[:latitude]} label="Latitude" type="number" />
      <.form_field field={@form[:longitude]} label="Longitude" type="number" />
    </div>
    """
  end

  @spec render_form_actions(editing :: boolean(), loading :: boolean(), on_cancel :: String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div>
      <.button type="submit" loading={@loading}>
        <%= if @editing, do: "Update Plot", else: "Create Plot" %>
      </.button>
      <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates plot form data.
  
  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_plot_data(map()) :: {:ok, plot_form_data()} | {:error, map()}
  def validate_plot_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_coordinates(params),
         :ok <- validate_area(params) do
      {:ok, transform_plot_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:name, :business_id]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))
    
    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_coordinates(params) when is_map(params) do
    with {:ok, latitude} <- get_valid_coordinate(params[:latitude], :latitude),
         {:ok, longitude} <- get_valid_coordinate(params[:longitude], :longitude) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_area(params) when is_map(params) do
    case params[:total_area] do
      nil -> :ok
      area when is_number(area) and area >= 0 -> :ok
      _ -> {:error, %{area: "must be a positive number"}}
    end
  end

  defp get_valid_coordinate(nil, _field), do: :ok
  defp get_valid_coordinate(value, field) when is_number(value) do
    if value >= -180 and value <= 180 do
      :ok
    else
      {:error, %{field => "must be between -180 and 180"}}
    end
  end
  defp get_valid_coordinate(_value, field), do: {:error, %{field => "must be a number"}}

  @doc """
  Transforms raw plot data into structured format.
  """
  @spec transform_plot_data(map()) :: plot_form_data()
  def transform_plot_data(params) do
    %{
      name: Map.get(params, :name, "") |> String.trim(),
      description: Map.get(params, :description, "") |> String.trim(),
      business_id: Map.get(params, :business_id),
      address: Map.get(params, :address, "") |> String.trim(),
      total_area: Map.get(params, :total_area, 0) |> parse_number(),
      latitude: Map.get(params, :latitude) |> parse_number(),
      longitude: Map.get(params, :longitude) |> parse_number()
    }
  end

  defp parse_number(nil), do: nil
  defp parse_number(value) when is_binary(value), do: Float.parse(value) |> elem(0)
  defp parse_number(value) when is_number(value), do: value
  defp parse_number(_), do: nil
end
