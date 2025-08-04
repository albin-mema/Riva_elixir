defmodule RivaAshWeb.Components.Atoms.Badge do
  @moduledoc """
  Deprecated wrapper around the canonical design-system badge.

  Use RivaAshWeb.Components.UI.Badge.badge/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.

  Note: Icon functionality is not supported in the canonical UI.Badge and will be ignored.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Badge, as: UIBadge

  @doc """
  Renders a badge component.

  Backwards-compatible API: maps legacy size to UI.Badge API.
  Note: icon, icon_position, and pill attributes are ignored as they're not supported in UI.Badge.
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
    assigns =
      assigns
      |> Phoenix.Component.assign(:ui_size, map_legacy_size(assigns[:size]))

    ~H"""
    <UIBadge.badge
      variant={@variant}
      size={@ui_size}
      class={@class}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </UIBadge.badge>
    """
  end

  # Map legacy atom sizes to UI sizes
  defp map_legacy_size("sm"), do: "sm"
  defp map_legacy_size("md"), do: "default"
  defp map_legacy_size("lg"), do: "lg"
  defp map_legacy_size(_), do: "default"
end
