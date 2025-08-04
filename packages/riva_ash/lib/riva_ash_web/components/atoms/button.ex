defmodule RivaAshWeb.Components.Atoms.Button do
  @moduledoc """
  Deprecated wrapper around the canonical design-system button.

  Use RivaAshWeb.Components.UI.Button.button/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Button, as: UIButton

  @doc """
  Renders a button component.

  Backwards-compatible API: maps legacy variant/size to UI.Button API.
  """
  attr :type, :string, default: "button"
  attr :variant, :string, default: "primary"
  attr :size, :string, default: "md"
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :rest, :global, include: ~w(phx-click phx-disable-with phx-value phx-value-id)
  slot :inner_block, required: true

  def button(assigns) do
    assigns =
      assigns
      |> Phoenix.Component.assign(:ui_variant, map_variant(assigns[:variant]))
      |> Phoenix.Component.assign(:ui_size, legacy_size(assigns[:size]))

    ~H"""
    <UIButton.button
      type={@type}
      variant={@ui_variant}
      size={@ui_size}
      loading={@loading}
      class={@class}
      disabled={@disabled}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </UIButton.button>
    """
  end

  # Map legacy atoms variants to UI variants
  defp map_variant("primary"), do: "default"
  defp map_variant("secondary"), do: "secondary"
  defp map_variant("outline"), do: "outline"
  defp map_variant("ghost"), do: "ghost"
  defp map_variant("link"), do: "link"
  defp map_variant(_), do: "default"

  # Map legacy sizes to UI sizes
  defp legacy_size("sm"), do: "sm"
  defp legacy_size("md"), do: "default"
  defp legacy_size("lg"), do: "lg"
  defp legacy_size(_), do: "default"
end
