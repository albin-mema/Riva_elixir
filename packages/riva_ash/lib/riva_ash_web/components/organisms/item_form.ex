defmodule RivaAshWeb.Components.Organisms.ItemForm do
  @moduledoc """
  Item creation and edit form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Toggle
  # removed unused import to reduce warnings

  @doc """
  Renders an item form with positioning options.
  """
  attr(:form, :map, required: true)
  attr(:sections, :list, default: [])
  attr(:item_types, :list, default: [])
  attr(:editing, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec item_form(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def item_form(assigns) do
    # Render item form using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:form_class, build_form_class())
    |> Map.put_new(:basic_info_class, build_basic_info_class())
    |> Map.put_new(:section_type_class, build_section_type_class(assigns.sections, assigns.item_types))
    |> Map.put_new(:availability_class, build_availability_class())
    |> Map.put_new(:capacity_class, build_capacity_class())
    |> Map.put_new(:public_settings_class, build_public_settings_class())
    |> Map.put_new(:position_class, build_position_class())
    |> Map.put_new(:actions_class, build_actions_class())
    |> render_item_form_component()
  end

  # Private helper for item form rendering
  @spec render_item_form_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_item_form_component(assigns) do
    ~H"""
    <!-- Item form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest} class={@form_class}>
      <div class={@basic_info_class}>
        <.form_field field={@form[:name]} label="Item Name" required />
        <.form_field field={@form[:description]} label="Description" type="textarea" />
      </div>

      <div class={@section_type_class}>
        <.form_field field={@form[:section_id]} label="Section" type="select">
          <:input>
            <RivaAshWeb.Components.UI.Select.select options={@sections} prompt="Select a section" />
          </:input>
        </.form_field>
        <.form_field field={@form[:item_type_id]} label="Item Type" type="select">
          <:input>
            <RivaAshWeb.Components.UI.Select.select options={@item_types} prompt="Select an item type" />
          </:input>
        </.form_field>
      </div>

      <div class={@availability_class}>
        <.toggle field={@form[:is_active]} label="Active" />
        <.toggle field={@form[:is_always_available]} label="Always Available" />
      </div>

      <.form_field field={@form[:capacity]} label="Capacity" type="number" class={@capacity_class} />

      <!-- Public Search Settings -->
      <div class={@public_settings_class}>
        <h3 class="text-lg font-medium text-green-900">Public Search Settings</h3>
        <p class="text-sm text-green-700">Control how this item appears in global search for unregistered users.</p>

        <.toggle field={@form[:is_public_searchable]} label="Make this item discoverable in global search" />

        <.form_field
          field={@form[:public_description]}
          label="Public Description"
          type="textarea"
          placeholder="Enter a public-facing description for search results (optional)"
          helper_text="This description will be shown to unregistered users in search results. Leave empty to use the main description."
        />
      </div>

      <div class={@position_class}>
        <h3>Position</h3>
        <.form_field field={@form[:grid_row]} label="Row" type="number" />
        <.form_field field={@form[:grid_column]} label="Column" type="number" />
      </div>

      <div class={@actions_class}>
        <.button type="submit" loading={@loading}>
          <%= if @editing, do: "Update Item", else: "Create Item" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build form classes
  @spec build_form_class() :: String.t()
  defp build_form_class, do: ""

  # Helper function to build basic info classes
  @spec build_basic_info_class() :: String.t()
  defp build_basic_info_class, do: "space-y-4"

  # Helper function to build section type classes
  @spec build_section_type_class(list(), list()) :: String.t()
  defp build_section_type_class(sections, item_types) do
    if sections != [] && item_types != [], do: "grid grid-cols-1 md:grid-cols-2 gap-4", else: "hidden"
  end

  # Helper function to build availability classes
  @spec build_availability_class() :: String.t()
  defp build_availability_class, do: "grid grid-cols-1 md:grid-cols-2 gap-4"

  # Helper function to build capacity classes
  @spec build_capacity_class() :: String.t()
  defp build_capacity_class, do: ""

  # Helper function to build public settings classes
  @spec build_public_settings_class() :: String.t()
  defp build_public_settings_class, do: "space-y-4 p-4 bg-green-50 rounded-lg border border-green-200"

  # Helper function to build position classes
  @spec build_position_class() :: String.t()
  defp build_position_class, do: "space-y-4"

  # Helper function to build actions classes
  @spec build_actions_class() :: String.t()
  defp build_actions_class, do: "flex gap-3"
end
