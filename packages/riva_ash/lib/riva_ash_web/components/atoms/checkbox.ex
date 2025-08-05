defmodule RivaAshWeb.Components.Atoms.Checkbox do
  @moduledoc """
  Deprecated wrapper around the canonical design-system checkbox.

  Use RivaAshWeb.Components.UI.Checkbox.checkbox/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Checkbox, as: UICheckbox

  @doc """
  Renders a checkbox component.

  Backwards-compatible API: maps legacy size to UI.Checkbox API.
  """
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :checked, :boolean, default: false
  attr :value, :string, default: "true"
  attr :label, :string, default: nil
  attr :description, :string, default: nil
  attr :disabled, :boolean, default: false
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :class, :string, default: ""
  attr :rest, :global

  def checkbox(assigns) when is_map(assigns) do
    assigns
    |> assign_ui_size()
    |> render_checkbox()
  end

  defp assign_ui_size(assigns) when is_map(assigns) do
    ui_size = assigns |> Map.get(:size, "md") |> map_legacy_size()
    Phoenix.Component.assign(assigns, :ui_size, ui_size)
  end

  defp render_checkbox(assigns) do
    ~H"""
    <UICheckbox.checkbox
      field={@field}
      checked={@checked}
      value={@value}
      label={@label}
      description={@description}
      disabled={@disabled}
      variant={@variant}
      size={@ui_size}
      class={@class}
      {@rest}
    />
    """
  end

  # Map legacy atom sizes to UI sizes using pattern matching
  defp map_legacy_size("sm"), do: "sm"
  defp map_legacy_size("md"), do: "default"
  defp map_legacy_size("lg"), do: "lg"
  defp map_legacy_size(_size), do: "default"
end
