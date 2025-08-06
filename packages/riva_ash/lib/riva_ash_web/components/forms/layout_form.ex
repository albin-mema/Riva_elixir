defmodule RivaAshWeb.Components.Forms.LayoutForm do
  @moduledoc """
  Layout configuration form component.

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
  import RivaAshWeb.Components.Atoms.Toggle

  @type assigns :: %{
          optional(:form) => map(),
          optional(:plots) => list(),
          optional(:editing) => boolean(),
          optional(:on_submit) => String.t(),
          optional(:on_change) => String.t(),
          optional(:on_cancel) => String.t(),
          optional(:loading) => boolean(),
          optional(:class) => String.t(),
          optional(:rest) => any()
        }

  @type layout_form_data :: %{
          name: String.t(),
          plot_id: String.t() | integer(),
          layout_type: String.t(),
          grid_rows: integer(),
          grid_columns: integer(),
          width: integer(),
          height: integer(),
          background_color: String.t(),
          background_image_url: String.t(),
          is_active: boolean()
        }

  @doc """
  Renders a layout configuration form.

  ## Examples
      <.layout_form
        form={@form}
        plots={@plots}
        editing={@editing}
        on_submit="save_layout"
        on_change="validate_layout"
        on_cancel="cancel_layout"
        loading={@loading}
      />
  """
  attr(:form, :map, required: true)
  attr(:plots, :list, default: [])
  attr(:editing, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec layout_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def layout_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_layout_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:plots, [])
    |> Map.put_new(:editing, false)
    |> Map.put_new(:loading, false)
    |> Map.put_new(:class, "")
  end

  @spec render_layout_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_layout_form(assigns) do
    ~H"""
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.render_basic_fields form={@form} plots={@plots} />
      <.render_layout_type_field form={@form} />
      <.render_grid_configuration form={@form} />
      <.render_dimensions form={@form} />
      <.render_appearance form={@form} />
      <.render_active_toggle form={@form} />
      <.render_form_actions editing={@editing} loading={@loading} on_cancel={@on_cancel} />
    </form>
    """
  end

  @spec render_basic_fields(form :: map(), plots :: list()) :: Phoenix.LiveView.Rendered.t()
  defp render_basic_fields(assigns) do
    ~H"""
    <.form_field field={@form[:name]} label="Layout Name" required />
    <.select_field field={@form[:plot_id]} label="Plot" options={plot_options(@plots)} required />
    """
  end

  @spec render_layout_type_field(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_layout_type_field(assigns) do
    ~H"""
    <div>
      <h3>Layout Type</h3>
      <.select_field field={@form[:layout_type]} label="Type" options={layout_type_options()} required />
    </div>
    """
  end

  @spec render_grid_configuration(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_grid_configuration(assigns) do
    ~H"""
    <div :if={show_grid_configuration?(@form[:layout_type].value)}>
      <h3>Grid Configuration</h3>
      <.form_field field={@form[:grid_rows]} label="Number of Rows" type="number" />
      <.form_field field={@form[:grid_columns]} label="Number of Columns" type="number" />
    </div>
    """
  end

  @spec render_dimensions(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_dimensions(assigns) do
    ~H"""
    <div>
      <h3>Dimensions</h3>
      <.form_field field={@form[:width]} label="Width (pixels)" type="number" />
      <.form_field field={@form[:height]} label="Height (pixels)" type="number" />
    </div>
    """
  end

  @spec render_appearance(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_appearance(assigns) do
    ~H"""
    <div>
      <h3>Appearance</h3>
      <.form_field field={@form[:background_color]} label="Background Color" type="color" />
      <.form_field field={@form[:background_image_url]} label="Background Image URL" type="url" />
    </div>
    """
  end

  @spec render_active_toggle(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_active_toggle(assigns) do
    ~H"""
    <div>
      <.toggle field={@form[:is_active]} label="Active Layout" />
    </div>
    """
  end

  @spec render_form_actions(editing :: boolean(), loading :: boolean(), on_cancel :: String.t()) ::
          Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div>
      <.button type="submit" loading={@loading}>
        <%= if @editing, do: "Update Layout", else: "Create Layout" %>
      </.button>
      <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates layout form data.

  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_layout_data(map()) :: {:ok, layout_form_data()} | {:error, map()}
  def validate_layout_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_layout_type(params[:layout_type]),
         :ok <- validate_dimensions(params) do
      {:ok, transform_layout_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:name, :plot_id, :layout_type]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_layout_type(nil), do: {:error, %{layout_type: "layout type is required"}}

  defp validate_layout_type(type) when is_binary(type) do
    valid_types = ["grid", "free", "linear"]

    if type in valid_types do
      :ok
    else
      {:error, %{layout_type: "must be one of: #{Enum.join(valid_types, ", ")}"}}
    end
  end

  defp validate_layout_type(_), do: {:error, %{layout_type: "must be a string"}}

  defp validate_dimensions(params) when is_map(params) do
    with {:ok, width} <- validate_dimension(params[:width], :width),
         {:ok, height} <- validate_dimension(params[:height], :height) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_dimension(nil, _field), do: :ok

  defp validate_dimension(value, field) when is_number(value) and value > 0 do
    {:ok, value}
  end

  defp validate_dimension(_value, field), do: {:error, %{field => "must be a positive number"}}

  @doc """
  Transforms raw layout data into structured format.
  """
  @spec transform_layout_data(map()) :: layout_form_data()
  def transform_layout_data(params) do
    %{
      name: Map.get(params, :name, "") |> String.trim(),
      plot_id: Map.get(params, :plot_id),
      layout_type: Map.get(params, :layout_type, "free") |> String.trim(),
      grid_rows: Map.get(params, :grid_rows, 1) |> parse_integer(),
      grid_columns: Map.get(params, :grid_columns, 1) |> parse_integer(),
      width: Map.get(params, :width, 800) |> parse_integer(),
      height: Map.get(params, :height, 600) |> parse_integer(),
      background_color: Map.get(params, :background_color, "#ffffff") |> String.trim(),
      background_image_url: Map.get(params, :background_image_url, "") |> String.trim(),
      is_active: Map.get(params, :is_active, true) |> parse_boolean()
    }
  end

  defp parse_integer(nil), do: nil
  defp parse_integer(value) when is_binary(value), do: Integer.parse(value) |> elem(0)
  defp parse_integer(value) when is_integer(value), do: value
  defp parse_integer(_), do: nil

  defp parse_boolean(nil), do: false
  defp parse_boolean(value) when is_boolean(value), do: value
  defp parse_boolean("true"), do: true
  defp parse_boolean("1"), do: true
  defp parse_boolean("false"), do: false
  defp parse_boolean("0"), do: false
  defp parse_boolean(_), do: false

  # UI Helper functions

  @spec plot_options(list()) :: list({String.t(), String.t() | integer()})
  defp plot_options(plots) when is_list(plots) do
    Enum.map(plots, fn plot ->
      {plot.name, plot.id}
    end)
  end

  @spec layout_type_options() :: list({String.t(), String.t()})
  defp layout_type_options do
    Application.compile_env(:riva_ash, :layout_type_options, [
      {"Grid Layout", "grid"},
      {"Free Layout", "free"},
      {"Linear Layout", "linear"}
    ])
  end

  @spec show_grid_configuration?(String.t() | nil) :: boolean()
  defp show_grid_configuration?(type) when type == "grid", do: true
  defp show_grid_configuration?(_), do: false
end
