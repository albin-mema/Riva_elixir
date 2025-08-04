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

  def text_input(assigns) do
    # Build rest attributes from legacy Phoenix event attributes
    rest_attrs =
      []
      |> maybe_add_attr("name", assigns[:name])
      |> maybe_add_attr("id", assigns[:id] || assigns[:name])
      |> maybe_add_attr("phx-debounce", assigns[:phx_debounce])
      |> maybe_add_attr("phx-change", assigns[:phx_change])
      |> maybe_add_attr("phx-keydown", assigns[:phx_keydown])
      |> Enum.into(%{})
      |> Map.merge(assigns[:rest] || %{})

    assigns = Phoenix.Component.assign(assigns, :computed_rest, rest_attrs)

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

  defp maybe_add_attr(attrs, _key, nil), do: attrs
  defp maybe_add_attr(attrs, key, value), do: [{key, value} | attrs]
end
