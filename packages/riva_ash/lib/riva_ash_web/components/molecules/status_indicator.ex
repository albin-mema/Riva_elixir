defmodule RivaAshWeb.Components.Molecules.StatusIndicator do
  @moduledoc """
  Status indicator component with colors and icons.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Badge, as: UIBadge
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon

  @doc """
  Renders a status indicator with appropriate styling.
  """
  attr(:status, :string, required: true)
  attr(:label, :string, default: nil)
  attr(:show_icon, :boolean, default: true)
  attr(:show_pulse, :boolean, default: false)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "auto", values: ~w(auto success warning error info))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def status_indicator(assigns) do
    assigns = assign(assigns, :computed_variant, compute_variant(assigns.status, assigns.variant))
    assigns = assign(assigns, :status_icon, status_icon(assigns.status))

    ~H"""
    <div class={["inline-flex items-center gap-1.5", @class]} {@rest}>
      <%= if @show_pulse do %>
        <div class={["relative flex h-2 w-2", pulse_color(@computed_variant)]}>
          <span class="animate-ping absolute inline-flex h-full w-full rounded-full opacity-75"></span>
          <span class="relative inline-flex rounded-full h-2 w-2"></span>
        </div>
      <% end %>

      <UIBadge.badge variant={@computed_variant} size={@size}>
        <%= if @show_icon && !@show_pulse do %>
          <UIIcon.icon name={@status_icon} size="xs" />
        <% end %>
        <%= @label || String.capitalize(@status) %>
      </UIBadge.badge>
    </div>
    """
  end

  defp compute_variant(status, "auto") do
    case String.downcase(status) do
      s when s in ~w(active online success completed approved) -> "success"
      s when s in ~w(pending warning processing) -> "warning"
      s when s in ~w(error failed rejected offline) -> "destructive"
      s when s in ~w(info draft) -> "secondary"
      _ -> "default"
    end
  end

  defp compute_variant(_status, variant), do: variant

  defp status_icon(status) do
    case String.downcase(status) do
      s when s in ~w(active online success completed approved) -> :check
      s when s in ~w(pending warning processing) -> :clock
      s when s in ~w(error failed rejected offline) -> :x_mark
      s when s in ~w(info draft) -> :information_circle
      _ -> :circle
    end
  end

  defp pulse_color(variant) do
    case variant do
      "success" -> "text-green-400"
      "warning" -> "text-yellow-400"
      "destructive" -> "text-red-400"
      "secondary" -> "text-blue-400"
      _ -> "text-gray-400"
    end
  end
end
