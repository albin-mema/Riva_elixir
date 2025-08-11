defmodule RivaAshWeb.Components.UIWrapped.Link do
  @moduledoc """
  App-level Link wrapper around SaladUI.Link.
  
  Provides consistent link styling and behavior.
  """
  use Phoenix.Component

  @doc """
  Renders a styled link component.
  """
  attr :to, :string, required: true, doc: "Link destination path"
  attr :variant, :string,
    default: "default",
    values: ~w(default primary secondary underline),
    doc: "Link visual variant"

  attr :size, :string,
    default: "default",
    values: ~w(default sm),
    doc: "Link size"

  attr :disabled, :boolean, default: false, doc: "Whether the link is disabled"
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def link(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_variant, fn -> map_variant(assigns.variant) end)
      |> assign_new(:_salad_size, fn -> size_to_salad(assigns.size) end)

    ~H"""
    <SaladUI.Link.link
      to={@to}
      variant={@_salad_variant}
      size={@_salad_size}
      disabled={@disabled}
      class={[
        case @variant do
          "primary" -> "text-primary hover:text-primary/80"
          "secondary" -> "text-muted-foreground hover:text-foreground"
          "underline" -> "underline underline-offset-4 hover:text-primary"
          _ -> "text-primary hover:text-primary/80"
        end,
        case @size do
          "sm" -> "text-sm"
          _ -> "text-base"
        end,
        @disabled && "pointer-events-none opacity-50",
        @class
      ]}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </SaladUI.Link.link>
    """
  end

  # Map our stable API to SaladUI expected props
  defp map_variant("default"), do: "default"
  defp map_variant(v) when v in ["primary", "secondary", "underline"], do: v
  defp map_variant(_), do: "default"

  defp size_to_salad("default"), do: "default"
  defp size_to_salad(s) when s in ["sm"], do: s
  defp size_to_salad(_), do: "default"
end