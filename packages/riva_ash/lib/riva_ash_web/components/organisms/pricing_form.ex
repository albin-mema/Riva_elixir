defmodule RivaAshWeb.Components.Organisms.PricingForm do
  @moduledoc """
  Pricing configuration form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a pricing configuration form.
  """
  attr(:form, :map, required: true)
  attr(:items, :list, default: [])
  attr(:editing, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def pricing_form(assigns) do
    ~H"""
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.select_field field={@form[:item_id]} label="Item" options={Enum.map(@items, &{&1.name, &1.id})} />
      <.form_field field={@form[:price_per_day]} label="Price per Day" type="number" />
      <.form_field field={@form[:currency]} label="Currency" readonly />

      <.form_field field={@form[:effective_from]} label="Effective From" type="date" />
      <.form_field field={@form[:effective_until]} label="Effective Until" type="date" />

      <.textarea_field field={@form[:notes]} label="Notes" />

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
