
defmodule RivaAshWeb.Components.Atoms.TextInput do
  @moduledoc """
  Deprecated wrapper around the canonical design-system input.

  Use RivaAshWeb.Components.UI.Input.input/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Input, as: UIInput

  @doc """
  Renders a text input component.

  Backwards-compatible API: delegates to UI.Input with legacy attribute mapping.
  """
  attr :type, :string, default: "text"
  attr :name, :string, default: nil
  attr :id, :string, default: nil
  attr :value, :string, default: nil
  attr :placeholder, :string, default: ""
  attr :class, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :phx_debounce, :string, default: nil
  attr :phx_change, :string, default: nil
  attr :phx_keydown, :string, default: nil
  attr :rest, :global

  def text_input(assigns) when is_map(assigns) do
    assigns
    |> build_rest_attributes()
    |> assign_rest_attrs()
    |> render_text_input()
  end

  defp build_rest_attributes(assigns) when is_map(assigns) do
    _rest_attrs =
      []
      |> add_attribute("name", assigns[:name])
      |> add_attribute("id", assigns[:id] || assigns[:name])
      |> add_attribute("phx-debounce", assigns[:phx_debounce])
      |> add_attribute("phx-change", assigns[:phx_change])
      |> add_attribute("phx-keydown", assigns[:phx_keydown])
      |> Enum.into(%{})
      |> Map.merge(assigns[:rest] || %{})
  end

  defp assign_rest_attrs(assigns) when is_map(assigns) do
    Phoenix.Component.assign(assigns, :computed_rest, build_rest_attributes(assigns))
  end

  defp render_text_input(assigns) do
    ~H"""
    <UIInput.input
      type={@type}
      value={@value}
      placeholder={@placeholder}
      disabled={@disabled}
      class={@class}
      {@computed_rest}
    />
    """
  end

  # Pattern matching for attribute building
  defp add_attribute(attrs, _key, nil), do: attrs
  defp add_attribute(attrs, key, value) when is_binary(key) and not is_nil(value), do: [{key, value} | attrs]
end
