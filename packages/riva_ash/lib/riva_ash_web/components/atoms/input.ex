defmodule RivaAshWeb.Components.Atoms.Input do
  @moduledoc """
  Deprecated wrapper around the canonical design-system input.

  Use RivaAshWeb.Components.UI.Input.input/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Input, as: UIInput

  @doc """
  Renders an input component.

  Backwards-compatible API: maps legacy size to UI.Input API.
  """
  attr :type, :string, default: "text"
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :value, :string, default: nil
  attr :placeholder, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :readonly, :boolean, default: false
  attr :required, :boolean, default: false
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :class, :string, default: ""
  attr :rest, :global

  def input(assigns) do
    assigns =
      assigns
      |> Phoenix.Component.assign(:ui_size, map_legacy_size(assigns[:size]))

    ~H"""
    <UIInput.input
      type={@type}
      field={@field}
      value={@value}
      placeholder={@placeholder}
      disabled={@disabled}
      readonly={@readonly}
      required={@required}
      variant={@variant}
      size={@ui_size}
      class={@class}
      {@rest}
    />
    """
  end

  # Map legacy atom sizes to UI sizes
  defp map_legacy_size("sm"), do: "sm"
  defp map_legacy_size("md"), do: "default"
  defp map_legacy_size("lg"), do: "lg"
  defp map_legacy_size(_), do: "default"
end
