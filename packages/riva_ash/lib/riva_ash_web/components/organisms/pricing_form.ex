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

  @spec pricing_form(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def pricing_form(assigns) do
    # Render pricing form using functional composition
    assigns
    |> Map.put_new(:form_class, build_form_class(assigns.class, assigns.variant))
    |> Map.put_new(:item_field_class, build_item_field_class(assigns.items))
    |> Map.put_new(:price_field_class, build_price_field_class(assigns.form))
    |> Map.put_new(:currency_field_class, build_currency_field_class(assigns.form))
    |> Map.put_new(:date_fields_class, build_date_fields_class(assigns.form))
    |> Map.put_new(:notes_field_class, build_notes_field_class(assigns.form))
    |> Map.put_new(:actions_class, build_actions_class(assigns.loading))
    |> render_pricing_form_component()
  end

  # Private helper for pricing form rendering
  @spec render_pricing_form_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_pricing_form_component(assigns) do
    ~H"""
    <form class={@form_class} phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.select_field
        field={@form[:item_id]}
        label="Item"
        options={Enum.map(@items, &{&1.name, &1.id})}
        class={@item_field_class}
      />
      <.form_field
        field={@form[:price_per_day]}
        label="Price per Day"
        type="number"
        class={@price_field_class}
      />
      <.form_field
        field={@form[:currency]}
        label="Currency"
        readonly
        class={@currency_field_class}
      />

      <div class={@date_fields_class}>
        <.form_field
          field={@form[:effective_from]}
          label="Effective From"
          type="date"
        />
        <.form_field
          field={@form[:effective_until]}
          label="Effective Until"
          type="date"
        />
      </div>

      <.textarea_field
        field={@form[:notes]}
        label="Notes"
        class={@notes_field_class}
      />

      <div class={@actions_class}>
        <.button type="submit" variant="primary" loading={@loading}>
          <%= if @editing, do: "Update Pricing", else: "Create Pricing" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end

  # Helper function to build form classes
  @spec build_form_class(String.t(), String.t()) :: String.t()
  defp build_form_class(class, variant) do
    base =
      case variant do
        "compact" -> "space-y-3"
        "card" -> "bg-card rounded-lg p-6 shadow-sm space-y-4"
        _ -> "space-y-4"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build item field classes
  @spec build_item_field_class(list()) :: String.t()
  defp build_item_field_class(items) do
    if items != [], do: "mb-4", else: "hidden"
  end

  # Helper function to build price field classes
  @spec build_price_field_class(map()) :: String.t()
  defp build_price_field_class(form) do
    if form[:price_per_day], do: "mb-4", else: "hidden"
  end

  # Helper function to build currency field classes
  @spec build_currency_field_class(map()) :: String.t()
  defp build_currency_field_class(form) do
    if form[:currency], do: "mb-4", else: "hidden"
  end

  # Helper function to build date fields container classes
  @spec build_date_fields_class(map()) :: String.t()
  defp build_date_fields_class(form) do
    if form[:effective_from] or form[:effective_until], do: "grid grid-cols-1 gap-4 mb-4 md:grid-cols-2", else: "hidden"
  end

  # Helper function to build notes field classes
  @spec build_notes_field_class(map()) :: String.t()
  defp build_notes_field_class(form) do
    if form[:notes], do: "mb-6", else: "hidden"
  end

  # Helper function to build actions classes
  @spec build_actions_class(boolean()) :: String.t()
  defp build_actions_class(loading) do
    "flex gap-3"
  end
end
