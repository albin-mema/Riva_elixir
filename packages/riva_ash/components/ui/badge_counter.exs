defmodule RivaAsh.Components.UI.BadgeCounter do
  @moduledoc """
  BadgeCounter atom displaying numeric values with overflow handling ("99+") and optional removable state.
  Implements standardized prop structure with semantic variants and token-based styling.
  """

  use Phoenix.Component

  @variants [:default, :info, :success, :warning, :danger]

  attr :value, :integer, required: true
  attr :max, :integer, default: 99
  attr :variant, :atom, default: :default, values: @variants
  attr :removable, :boolean, default: false
  attr :on_remove, :string

  def badge_counter(assigns) do
    display_value = if @value > @max, do: "#{@max}+", else: @value

    ~H"""
    <span class={"inline-flex items-center rounded-sm px-2 py-1 text-xs font-medium #{variant_classes(@variant)}"}
         role="status"
         aria-live="polite">
      <%= display_value %>
      <%= if @removable do %>
        <.icon_button
          icon="close"
          phx_click={@on_remove}
          class="ml-1 w-4 h-4"
          aria_label="Remove badge"
        />
      <% end %>
    </span>
    """
  end

  defp variant_classes(:default), do: "bg-secondary text-secondary-foreground"
  defp variant_classes(:info), do: "bg-info text-info-foreground"
  defp variant_classes(:success), do: "bg-success text-success-foreground"
  defp variant_classes(:warning), do: "bg-warning text-warning-foreground"
  defp variant_classes(:danger), do: "bg-destructive text-destructive-foreground"
end
