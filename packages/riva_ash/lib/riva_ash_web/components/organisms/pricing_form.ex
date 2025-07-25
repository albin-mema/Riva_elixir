defmodule RivaAshWeb.Components.Organisms.PricingForm do
  @moduledoc """
  Pricing configuration form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Select

  @doc """
  Renders a pricing configuration form.
  """
  attr :form, :map, required: true
  attr :items, :list, default: []
  attr :editing, :boolean, default: false
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def pricing_form(assigns) do
    ~H"""
    <!-- Pricing form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.form_field field={@form[:item_id]} label="Item" type="select" options={Enum.map(@items, &{&1.name, &1.id})} />
      <.form_field field={@form[:price_per_day]} label="Price per Day" type="number" step="0.01" />
      <.form_field field={@form[:currency]} label="Currency" value="USD" readonly />

      <.form_field field={@form[:effective_from]} label="Effective From" type="date" />
      <.form_field field={@form[:effective_until]} label="Effective Until" type="date" />

      <.form_field field={@form[:notes]} label="Notes" type="textarea" />

      <div>
        <.button type="submit" variant="primary" loading={@loading}>
          <%= if @editing, do: "Update Pricing", else: "Create Pricing" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
