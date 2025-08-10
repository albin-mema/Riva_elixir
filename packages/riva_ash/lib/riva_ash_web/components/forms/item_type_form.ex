alias RivaAshWeb.Components.Forms, as: Forms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Forms.ItemTypeForm do
  @moduledoc """
  Item type configuration form component.

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
  import RivaAshWeb.Components.UI.Button
  import RivaAshWeb.Components.Atoms.Toggle

  @type assigns :: %{
          optional(:form) => map(),
          optional(:editing) => boolean(),
          optional(:on_submit) => String.t(),
          optional(:on_change) => String.t(),
          optional(:on_cancel) => String.t(),
          optional(:loading) => boolean(),
          optional(:class) => String.t(),
          optional(:rest) => any()
        }

  @type item_type_form_data :: %{
          name: String.t(),
          description: String.t(),
          default_capacity: integer(),
          default_duration: integer(),
          requires_approval: boolean(),
          allows_recurring: boolean(),
          base_price: float(),
          price_unit: String.t()
        }

  @doc """
  Renders an item type form.

  ## Examples
      <.item_type_form
        form={@form}
        editing={@editing}
        on_submit="save_item_type"
        on_change="validate_item_type"
        on_cancel="cancel_item_type"
        loading={@loading}
      />
  """
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec item_type_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def item_type_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_item_type_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:editing, false)
    |> Map.put_new(:loading, false)
    |> Map.put_new(:class, "")
  end

  @spec render_item_type_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_item_type_form(assigns) do
    ~H"""
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.render_basic_fields form={@form} />
      <.render_default_properties form={@form} />
      <.render_pricing_fields form={@form} />
      <.render_form_actions editing={@editing} loading={@loading} on_cancel={@on_cancel} />
    </form>
    """
  end

  @spec render_basic_fields(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_basic_fields(assigns) do
    ~H"""
    <.form_field field={@form[:name]} label="Item Type Name" required />
    <.form_field field={@form[:description]} label="Description" type="textarea" />
    """
  end

  @spec render_default_properties(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_default_properties(assigns) do
    ~H"""
    <div>
      <h3>Default Properties</h3>
      <.form_field field={@form[:default_capacity]} label="Default Capacity" type="number" />
      <.form_field field={@form[:default_duration]} label="Default Duration (minutes)" type="number" />
      <.toggle field={@form[:requires_approval]} label="Requires Approval" />
      <.toggle field={@form[:allows_recurring]} label="Allows Recurring Reservations" />
    </div>
    """
  end

  @spec render_pricing_fields(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_pricing_fields(assigns) do
    ~H"""
    <div>
      <h3>Pricing</h3>
      <.form_field field={@form[:base_price]} label="Base Price" type="number">
        <:input>
          <input type="number" step="0.01" name={@form[:base_price].name} value={@form[:base_price].value} class="input" />
        </:input>
      </.form_field>
      <.form_field field={@form[:price_unit]} label="Price Unit" type="select" options={price_unit_options()} />
    </div>
    """
  end

  @spec render_form_actions(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div>
      <.button type="submit" loading={@loading}>
        <%= if @editing, do: "Update Item Type", else: "Create Item Type" %>
      </.button>
      <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates item type form data.

  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_item_type_data(map()) :: {:ok, item_type_form_data()} | {:error, map()}
  def validate_item_type_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_pricing_fields(params) do
      {:ok, transform_item_type_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:name, :price_unit]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_pricing_fields(params) when is_map(params) do
    with {:ok, _base_price} <- validate_price(params[:base_price]),
         :ok <- validate_price_unit(params[:price_unit]) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_price(nil), do: :ok
  defp validate_price(price) when is_number(price) and price >= 0, do: {:ok, price}
  defp validate_price(_), do: {:error, %{base_price: "must be a positive number"}}

  defp validate_price_unit(nil), do: {:error, %{price_unit: "price unit is required"}}

  defp validate_price_unit(unit) when is_binary(unit) do
    valid_units = ["hour", "day", "week", "fixed"]

    if unit in valid_units do
      :ok
    else
      {:error, %{price_unit: "must be one of: #{Enum.join(valid_units, ", ")}"}}
    end
  end

  defp validate_price_unit(_), do: {:error, %{price_unit: "must be a string"}}

  @doc """
  Transforms raw item type data into structured format.
  """
  @spec transform_item_type_data(map()) :: item_type_form_data()
  def transform_item_type_data(params) do
    %{
      name: Map.get(params, :name, "") |> String.trim(),
      description: Map.get(params, :description, "") |> String.trim(),
      default_capacity: Map.get(params, :default_capacity, 1) |> parse_integer(),
      default_duration: Map.get(params, :default_duration, 60) |> parse_integer(),
      requires_approval: Map.get(params, :requires_approval, false) |> parse_boolean(),
      allows_recurring: Map.get(params, :allows_recurring, false) |> parse_boolean(),
      base_price: Map.get(params, :base_price, 0.0) |> parse_float(),
      price_unit: Map.get(params, :price_unit, "fixed") |> String.trim()
    }
  end

  defp parse_integer(nil), do: nil
  defp parse_integer(value) when is_binary(value), do: Integer.parse(value) |> elem(0)
  defp parse_integer(value) when is_integer(value), do: value
  defp parse_integer(_), do: nil

  defp parse_float(nil), do: nil
  defp parse_float(value) when is_binary(value), do: Float.parse(value) |> elem(0)
  defp parse_float(value) when is_float(value), do: value
  defp parse_float(value) when is_integer(value), do: value / 1.0
  defp parse_float(_), do: nil

  defp parse_boolean(nil), do: false
  defp parse_boolean(value) when is_boolean(value), do: value
  defp parse_boolean("true"), do: true
  defp parse_boolean("1"), do: true
  defp parse_boolean("false"), do: false
  defp parse_boolean("0"), do: false
  defp parse_boolean(_), do: false

  @doc """
  Gets price unit options from configuration.
  """
  @spec price_unit_options() :: list({String.t(), String.t()})
  defp price_unit_options do
    Application.get_env(:riva_ash, :price_unit_options, [
      {"Per Hour", "hour"},
      {"Per Day", "day"},
      {"Per Week", "week"},
      {"Fixed", "fixed"}
    ])
  end
end
