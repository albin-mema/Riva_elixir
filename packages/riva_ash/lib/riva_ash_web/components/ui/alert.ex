alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.LiveView.Rendered, as: Rendered

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

  @spec alert(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def alert(assigns) do
    # Render alert using functional composition
    assigns
    |> Map.put_new(:alert_class, alert_class(assigns))
    |> Map.put_new(:title_class, build_title_class(assigns.title))
    |> Map.put_new(:content_class, build_content_class(assigns.title))
    |> render_alert_component()
  end

  # Private helper for alert rendering
  @spec render_alert_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_alert_component(assigns) do
    ~H"""
    <div class={@alert_class} {@rest}>
      <%= if @title do %>
        <h5 class={@title_class}>
          <%= @title %>
        </h5>
      <% end %>
      <div class={@content_class}>
        <%= render_slot(@inner_block) %>
      </div>
    </div>
    """
  end

  # Helper function to build title classes
  @spec build_title_class(String.t() | nil) :: String.t()
  defp build_title_class(title) do
    if is_nil(title) or title == "" do
      "hidden"
    else
      "mb-1 font-medium leading-none tracking-tight"
    end
  end

  # Helper function to build content classes
  @spec build_content_class(String.t() | nil) :: String.t()
  defp build_content_class(title) do
    if title, do: "text-sm [&_p]:leading-relaxed", else: "text-sm [&_p]:leading-relaxed"
  end

  defp alert_class(assigns) do
    base =
      "relative w-full rounded-lg border p-4 [&>svg]:absolute [&>svg]:text-foreground [&>svg]:left-4 [&>svg]:top-4 [&>svg+div]:translate-y-[-3px] [&:has(svg)]:pl-11"

    variant = variant_classes(assigns.variant)

    Enum.join([base, variant, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" ->
        "bg-background text-foreground"

      "destructive" ->
        "border-destructive/50 text-destructive dark:border-destructive [&>svg]:text-destructive"

      "success" ->
        "border-[var(--chart-5)]/50 text-[var(--chart-5)] dark:border-[var(--chart-5)] [&>svg]:text-[var(--chart-5)]"

      "warning" ->
        "border-amber-500/50 text-amber-500 dark:border-amber-500 [&>svg]:text-amber-500"

      _unmatchedunmatched ->
        "bg-background text-foreground"
    end
  end
end
