alias RivaAshWeb.Components.Forms, as: Forms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered
alias RivaAshWeb.Components.UI.Select, as: Select

defmodule RivaAshWeb.Components.Forms.SectionForm do
  @moduledoc """
  Section creation and editing form component using atomic design system.

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
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button

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

  @type section_form_data :: %{
          name: String.t(),
          description: String.t(),
          plot_id: String.t() | integer()
        }

  @doc """
  Renders a section form.

  ## Examples
      <.section_form
        form={@form}
        plots={@plots}
        editing={@editing}
        on_submit="save_section"
        on_change="validate_section"
        on_cancel="cancel_section"
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

  @spec section_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def section_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_section_form()
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

  @spec render_section_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_section_form(assigns) do
    ~H"""
    <.card variant="elevated" class={@class} {@rest}>
      <:body>
        <form phx-submit={@on_submit} phx-change={@on_change} class="space-y-6">
          <.render_basic_fields form={@form} plots={@plots} />
          <.render_form_actions editing={@editing} loading={@loading} on_cancel={@on_cancel} />
        </form>
      </:body>
    </.card>
    """
  end

  @spec render_basic_fields(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_basic_fields(assigns) do
    ~H"""
    <.form_field
      field={@form[:name]}
      label="Section Name"
      required
      helper_text="Enter a name for the section"
    />
    <.form_field
      field={@form[:description]}
      label="Description"
      type="textarea"
      helper_text="Provide a description for the section"
    />
    <.form_field
      field={@form[:plot_id]}
      label="Plot"
      type="select"
      required
      helper_text="Select the plot this section belongs to"
    >
      <:input>
        <RivaAshWeb.Components.UI.Select.select options={plot_options(@plots)} prompt="Select a plot" />
      </:input>
    </.form_field>
    """
  end

  @spec render_form_actions(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div class="flex justify-end space-x-3 pt-4 border-t">
      <.button type="button" variant="outline" phx-click={@on_cancel} disabled={@loading}>
        Cancel
      </.button>
      <.button type="submit" loading={@loading}>
        <%= if @editing, do: "Update Section", else: "Create Section" %>
      </.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates section form data.

  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_section_data(map()) :: {:ok, section_form_data()} | {:error, map()}
  def validate_section_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_plot_exists(params[:plot_id], @plots) do
      {:ok, transform_section_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:name, :plot_id]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_plot_exists(nil, _), do: {:error, %{plot_id: "plot is required"}}

  defp validate_plot_exists(plot_id, plots) when is_list(plots) do
    case Enum.find(plots, fn plot -> plot.id == plot_id end) do
      %{id: ^plot_id} -> :ok
      _ -> {:error, %{plot_id: "plot not found"}}
    end
  end

  @doc """
  Transforms raw section data into structured format.
  """
  @spec transform_section_data(map()) :: section_form_data()
  def transform_section_data(params) do
    %{
      name: Map.get(params, :name, "") |> String.trim(),
      description: Map.get(params, :description, "") |> String.trim(),
      plot_id: parse_id(params[:plot_id])
    }
  end

  defp parse_id(nil), do: nil
  defp parse_id(value) when is_binary(value), do: Integer.parse(value) |> elem(0)
  defp parse_id(value) when is_integer(value), do: value
  defp parse_id(_), do: nil

  # UI Helper functions

  @spec plot_options(list()) :: list({String.t(), integer()})
  defp plot_options(plots) when is_list(plots) do
    Enum.map(plots, fn plot ->
      {plot.name, plot.id}
    end)
  end
end
