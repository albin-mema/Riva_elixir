defmodule RivaAshWeb.Components.Atoms.Badge do
  @moduledoc """
  Badge component for displaying labels, statuses, and small pieces of information.
  Encapsulates all styling without exposing Tailwind classes.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders a badge with consistent styling.

  ## Examples

      <.badge>New</.badge>
      <.badge variant="success">Active</.badge>
      <.badge variant="destructive" size="lg">Critical</.badge>
      <.badge variant="outline" icon={:check}>Verified</.badge>
  """
  attr :variant, :string, default: "default", values: ~w(default secondary success warning destructive outline)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :icon, :atom, default: nil
  attr :icon_position, :string, default: "left", values: ~w(left right)
  attr :pill, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def badge(assigns) do
    assigns = assign(assigns, :badge_class, badge_class(assigns))

    ~H"""
    <span class={@badge_class} {@rest}>
      <%= if @icon && @icon_position == "left" do %>
        <.icon name={@icon} size={icon_size(@size)} />
      <% end %>

      <span><%= render_slot(@inner_block) %></span>

      <%= if @icon && @icon_position == "right" do %>
        <.icon name={@icon} size={icon_size(@size)} />
      <% end %>
    </span>
    """
  end

  defp badge_class(assigns) do
    base = base_classes(assigns.pill)
    size = size_classes(assigns.size)
    variant = variant_classes(assigns.variant)

    Enum.join([base, size, variant, assigns.class], " ")
  end

  defp base_classes(pill) do
    base = "inline-flex items-center gap-1 font-medium transition-colors focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"

    if pill do
      "#{base} rounded-full"
    else
      "#{base} rounded-md"
    end
  end

  defp size_classes(size) do
    case size do
      "sm" -> "px-2 py-0.5 text-xs"
      "md" -> "px-2.5 py-0.5 text-xs"
      "lg" -> "px-3 py-1 text-sm"
      _ -> "px-2.5 py-0.5 text-xs"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "default" ->
        "bg-primary text-primary-foreground hover:bg-primary/80"

      "secondary" ->
        "bg-secondary text-secondary-foreground hover:bg-secondary/80"

      "success" ->
        "bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-100 hover:bg-green-200 dark:hover:bg-green-800"

      "warning" ->
        "bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-100 hover:bg-yellow-200 dark:hover:bg-yellow-800"

      "destructive" ->
        "bg-destructive text-destructive-foreground hover:bg-destructive/80"

      "outline" ->
        "border border-input bg-background hover:bg-accent hover:text-accent-foreground"

      _ ->
        "bg-primary text-primary-foreground hover:bg-primary/80"
    end
  end

  defp icon_size(badge_size) do
    case badge_size do
      "sm" -> "xs"
      "md" -> "xs"
      "lg" -> "sm"
      _ -> "xs"
    end
  end
end
