alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Components.UI.Select, as: Select
alias RivaAshWeb.Components.UI, as: UI
alias UI.Select, as: Select
alias Phoenix.HTML, as: HTML
alias Phoenix.Component, as: Component

defmodule RivaAshWeb.Components.Atoms.Select do
  @moduledoc """
  Deprecated wrapper around the canonical design-system select.

  Use RivaAshWeb.Components.UI.Select.select/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Select, as: UISelect

  @doc """
  Renders a select component.

  Backwards-compatible API: maps legacy size to UI.Select API.
  Note: searchable attribute is ignored as it's not supported in the canonical UI.Select.
  """
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :options, :list, default: []
  attr :prompt, :string, default: nil
  attr :multiple, :boolean, default: false
  attr :searchable, :boolean, default: false
  attr :disabled, :boolean, default: false
  attr :required, :boolean, default: false
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :class, :string, default: ""
  attr :rest, :global

  def select(assigns) when is_map(assigns) do
    assigns
    |> assign_ui_size()
    |> render_select()
  end

  defp assign_ui_size(assigns) when is_map(assigns) do
    ui_size = assigns |> Map.get(:size, "md") |> map_legacy_size()
    Phoenix.Component.assign(assigns, :ui_size, ui_size)
  end

  defp render_select(assigns) do
    ~H"""
    <UISelect.select
      field={@field}
      options={@options}
      prompt={@prompt}
      multiple={@multiple}
      disabled={@disabled}
      required={@required}
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
  defp map_legacy_size(_), do: "default"
end
