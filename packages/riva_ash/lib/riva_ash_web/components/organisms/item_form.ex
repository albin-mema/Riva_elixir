defmodule RivaAshWeb.Components.Organisms.ItemForm do
  @moduledoc """
  Item creation and edit form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Toggle
  import RivaAshWeb.Components.Atoms.Select

  @doc """
  Renders an item form with positioning options.
  """
  attr :form, :map, required: true
  attr :sections, :list, default: []
  attr :item_types, :list, default: []
  attr :editing, :boolean, default: false
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def item_form(assigns) do
    ~H"""
    <!-- Item form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.form_field field={@form[:name]} label="Item Name" required />
      <.form_field field={@form[:description]} label="Description" type="textarea" />
      <.form_field field={@form[:section_id]} label="Section" type="select" options={@sections} />
      <.form_field field={@form[:item_type_id]} label="Item Type" type="select" options={@item_types} />
      
      <.toggle field={@form[:is_active]} label="Active" />
      <.toggle field={@form[:is_always_available]} label="Always Available" />
      
      <.form_field field={@form[:capacity]} label="Capacity" type="number" />
      
      <div>
        <h3>Position</h3>
        <.form_field field={@form[:grid_row]} label="Row" type="number" />
        <.form_field field={@form[:grid_column]} label="Column" type="number" />
      </div>
      
      <div>
        <.button type="submit" loading={@loading}>
          <%= if @editing, do: "Update Item", else: "Create Item" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
