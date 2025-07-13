defmodule RivaAshWeb.Components.Forms.ItemTypeForm do
  @moduledoc """
  Item type configuration form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Toggle

  @doc """
  Renders an item type form.
  """
  attr :form, :map, required: true
  attr :editing, :boolean, default: false
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def item_type_form(assigns) do
    ~H"""
    <!-- Item type form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.form_field field={@form[:name]} label="Item Type Name" required />
      <.form_field field={@form[:description]} label="Description" type="textarea" />
      
      <div>
        <h3>Default Properties</h3>
        <.form_field field={@form[:default_capacity]} label="Default Capacity" type="number" />
        <.form_field field={@form[:default_duration]} label="Default Duration (minutes)" type="number" />
        
        <.toggle field={@form[:requires_approval]} label="Requires Approval" />
        <.toggle field={@form[:allows_recurring]} label="Allows Recurring Reservations" />
      </div>
      
      <div>
        <h3>Pricing</h3>
        <.form_field field={@form[:base_price]} label="Base Price" type="number" step="0.01" />
        <.form_field field={@form[:price_unit]} label="Price Unit" type="select" options={[
          {"Per Hour", "hour"},
          {"Per Day", "day"},
          {"Per Week", "week"},
          {"Fixed", "fixed"}
        ]} />
      </div>
      
      <div>
        <.button type="submit" loading={@loading}>
          <%= if @editing, do: "Update Item Type", else: "Create Item Type" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
