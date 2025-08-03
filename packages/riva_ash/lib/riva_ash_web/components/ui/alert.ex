defmodule RivaAshWeb.Components.UI.Alert do
  @moduledoc """
  Implements an alert component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders an alert component using the design system.
  """
  attr :variant, :string, default: "default", values: ~w(default destructive success warning)
  attr :title, :string, default: nil
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def alert(assigns) do
    assigns = assign(assigns, :alert_class, alert_class(assigns))

    ~H"""
    <div class={@alert_class} {@rest}>
      <%= if @title do %>
        <h5 class="mb-1 font-medium leading-none tracking-tight">
          <%= @title %>
        </h5>
      <% end %>
      <div class="text-sm [&_p]:leading-relaxed">
        <%= render_slot(@inner_block) %>
      </div>
    </div>
    """
  end

  defp alert_class(assigns) do
    base = "relative w-full rounded-lg border p-4 [&>svg]:absolute [&>svg]:text-foreground [&>svg]:left-4 [&>svg]:top-4 [&>svg+div]:translate-y-[-3px] [&:has(svg)]:pl-11"

    variant = variant_classes(assigns.variant)

    Enum.join([base, variant, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> "bg-background text-foreground"
      "destructive" -> "border-destructive/50 text-destructive dark:border-destructive [&>svg]:text-destructive"
      "success" -> "border-[var(--chart-5)]/50 text-[var(--chart-5)] dark:border-[var(--chart-5)] [&>svg]:text-[var(--chart-5)]"
      "warning" -> "border-amber-500/50 text-amber-500 dark:border-amber-500 [&>svg]:text-amber-500"
      _ -> "bg-background text-foreground"
    end
  end
end
