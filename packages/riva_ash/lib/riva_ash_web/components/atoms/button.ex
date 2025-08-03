defmodule RivaAshWeb.Components.Atoms.Button do
  use Phoenix.Component

  @doc """
  Renders a button component.
  """
  attr(:type, :string, default: "button")
  attr(:variant, :string, default: "primary")
  attr(:size, :string, default: "md")
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:icon_left, :string)
  attr(:icon_right, :string)
  attr(:disabled, :boolean, default: false)
  attr(:rest, :global, include: ~w(phx-click phx-disable-with phx-value phx-value-id))

  slot(:inner_block, required: true)

  def button(assigns) do
    ~H"""
    <button
      type={@type}
      class={[
        "inline-flex items-center justify-center rounded-lg font-medium",
        "transition-colors duration-200 focus:outline-none focus:ring-2",
        "focus:ring-ring/30 focus:ring-offset-2 ring-offset-background",
        "disabled:opacity-50 disabled:cursor-not-allowed",
        size_classes(@size),
        variant_classes(@variant),
        @class
      ]}
      disabled={@disabled}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </button>
    """
  end

  defp size_classes(size) do
    case size do
      "sm" -> "text-sm px-3 py-1.5"
      "md" -> "text-base px-4 py-2"
      "lg" -> "text-lg px-6 py-3"
      _ -> "text-base px-4 py-2"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "primary" ->
        "bg-primary text-primary-foreground hover:bg-primary/90 shadow-sm"
      "secondary" ->
        "bg-secondary text-secondary-foreground hover:bg-secondary/80 border border-secondary-border"
      "outline" ->
        "border-2 border-input bg-transparent hover:bg-accent/20 text-foreground"
      "ghost" ->
        "hover:bg-accent/20 text-foreground/80 hover:text-foreground"
      "link" ->
        "text-primary underline-offset-4 hover:underline decoration-2"
      _ -> "bg-primary text-primary-foreground"
    end
  end
end
