defmodule RivaAshWeb.Components.UIWrapped.Card do
  @moduledoc """
  App-level Card wrapper around SaladUI.Card.

  Minimal, stable API that enforces accessibility and design tokens while allowing
  advanced usage via global attributes (:rest). Prefer this wrapper over direct
  SaladUI usage in application code while we evaluate and migrate existing UI.
  """
  use Phoenix.Component

  @doc """
  Renders a SaladUI-backed card with app-level props.
  """
  attr :variant, :string,
    default: "default",
    values: ~w(default outlined elevated compact),
    doc: "Visual variant"

  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_variant, fn -> map_variant(assigns.variant) end)

    ~H"""
    <SaladUI.Card.card
      variant={@_salad_variant}
      class={[
        "rounded-lg border bg-card text-card-foreground shadow-sm",
        @class
      ]}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </SaladUI.Card.card>
    """
  end

  # Map our stable API to SaladUI expected props
  defp map_variant("default"), do: "default"
  defp map_variant("outlined"), do: "outline"
  defp map_variant("elevated"), do: "default"
  defp map_variant("compact"), do: "default"
  defp map_variant(_), do: "default"
end
