defmodule RivaAshWeb.Components.UIWrapped.Text do
  @moduledoc """
  App-level Text wrapper around SaladUI.Text.
  
  Provides typography components with consistent styling and accessibility.
  """
  use Phoenix.Component

  @doc """
  Renders text with various variants and colors.
  """
  attr :variant, :string,
    default: "default",
    values: ~w(default lead small muted label),
    doc: "Text variant for typography"

  attr :color, :string,
    default: "default",
    values: ~w(default primary secondary destructive muted),
    doc: "Text color variant"

  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def text(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_variant, fn -> map_variant(assigns.variant) end)
      |> assign_new(:_salad_color, fn -> map_color(assigns.color) end)

    ~H"""
    <span
      class={[
        case @variant do
          "lead" -> "text-xl font-semibold"
          "small" -> "text-sm"
          "muted" -> "text-muted-foreground"
          "label" -> "text-sm font-medium"
          _ -> "text-base"
        end,
        case @color do
          "primary" -> "text-primary"
          "secondary" -> "text-secondary"
          "destructive" -> "text-destructive"
          "muted" -> "text-muted-foreground"
          _ -> "text-foreground"
        end,
        @class
      ]}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </span>
    """
  end

  # Map our stable API to SaladUI expected props
  defp map_variant("default"), do: "default"
  defp map_variant(v) when v in ["lead", "small", "muted", "label"], do: v
  defp map_variant(_), do: "default"

  defp map_color("default"), do: "default"
  defp map_color(v) when v in ["primary", "secondary", "destructive", "muted"], do: v
  defp map_color(_), do: "default"
end