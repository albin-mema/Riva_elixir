defmodule RivaAshWeb.Components.Atoms.Button do
  @moduledoc """
  Button component with encapsulated styling.
  Provides consistent button styles across the application without exposing Tailwind classes.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders a button with consistent styling.

  ## Examples

      <.button>Click me</.button>
      <.button variant="primary" size="lg">Submit</.button>
      <.button variant="destructive" disabled={true}>Delete</.button>
      <.button variant="ghost" icon_left={:plus}>Add Item</.button>
  """
  attr :type, :string, default: "button", values: ~w(button submit reset)
  attr :variant, :string, default: "primary", values: ~w(primary secondary destructive outline ghost link)
  attr :size, :string, default: "md", values: ~w(xs sm md lg xl)
  attr :disabled, :boolean, default: false
  attr :loading, :boolean, default: false
  attr :full_width, :boolean, default: false
  attr :icon_left, :atom, default: nil
  attr :icon_right, :atom, default: nil
  attr :class, :string, default: ""
  attr :rest, :global, include: ~w(phx-click phx-value-id data-confirm)

  slot :inner_block, required: true

  def button(assigns) do
    assigns = assign(assigns, :button_class, button_class(assigns))

    ~H"""
    <button
      type={@type}
      class={@button_class}
      disabled={@disabled || @loading}
      {@rest}
    >
      <%= if @loading do %>
        <span class="animate-spin inline-block">
          <svg class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
          </svg>
        </span>
      <% else %>
        <%= if @icon_left do %>
          <.icon name={@icon_left} size={icon_size(@size)} />
        <% end %>
      <% end %>

      <span><%= render_slot(@inner_block) %></span>

      <%= if @icon_right && !@loading do %>
        <.icon name={@icon_right} size={icon_size(@size)} />
      <% end %>
    </button>
    """
  end

  defp button_class(assigns) do
    base = base_classes()
    size = size_classes(assigns.size)
    variant = variant_classes(assigns.variant)
    state = state_classes(assigns)
    width = if assigns.full_width, do: "w-full", else: ""

    Enum.join([base, size, variant, state, width, assigns.class], " ")
  end

  defp base_classes do
    "inline-flex items-center justify-center gap-2 font-medium transition-all focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50 rounded-md"
  end

  defp size_classes(size) do
    case size do
      "xs" -> "h-7 px-2 text-xs"
      "sm" -> "h-8 px-3 text-sm"
      "md" -> "h-9 px-4 text-sm"
      "lg" -> "h-10 px-6"
      "xl" -> "h-12 px-8 text-lg"
      _ -> "h-9 px-4 text-sm"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "primary" ->
        "bg-primary text-primary-foreground hover:bg-primary/90 focus-visible:ring-primary"

      "secondary" ->
        "bg-secondary text-secondary-foreground hover:bg-secondary/80 focus-visible:ring-secondary"

      "destructive" ->
        "bg-destructive text-destructive-foreground hover:bg-destructive/90 focus-visible:ring-destructive"

      "outline" ->
        "border border-input bg-background hover:bg-accent hover:text-accent-foreground focus-visible:ring-accent"

      "ghost" ->
        "hover:bg-accent hover:text-accent-foreground focus-visible:ring-accent"

      "link" ->
        "text-primary underline-offset-4 hover:underline focus-visible:ring-primary"

      _ ->
        "bg-primary text-primary-foreground hover:bg-primary/90 focus-visible:ring-primary"
    end
  end

  defp state_classes(assigns) do
    cond do
      assigns.loading -> "cursor-wait"
      assigns.disabled -> "cursor-not-allowed"
      true -> "cursor-pointer"
    end
  end

  defp icon_size(button_size) do
    case button_size do
      "xs" -> "xs"
      "sm" -> "sm"
      "md" -> "sm"
      "lg" -> "md"
      "xl" -> "lg"
      _ -> "sm"
    end
  end
end
