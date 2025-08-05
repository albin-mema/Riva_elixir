defmodule RivaAshWeb.Components.Atoms.Toggle do
  @moduledoc """
  Toggle/switch component for boolean values.
  """
  use Phoenix.Component

  @doc """
  Renders a toggle switch.
  """
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:checked, :boolean, default: false)
  attr(:value, :string, default: "true")
  attr(:label, :string, default: nil)
  attr(:description, :string, default: nil)
  attr(:disabled, :boolean, default: false)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default success warning destructive))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def toggle(assigns) when is_map(assigns) do
    assigns
    |> validate_toggle_attributes()
    |> render_toggle()
  end

  defp validate_toggle_attributes(assigns) when is_map(assigns) do
    case {Map.get(assigns, :value), Map.get(assigns, :checked)} do
      {nil, _} -> raise ArgumentError, "value is required for toggle component"
      {_, _} -> assigns
    end
  end

  defp render_toggle(assigns) do
    ~H"""
    <!-- Toggle implementation will go here -->
    <div>
      <input type="checkbox" {@rest} />
      <label :if={@label}><%= @label %></label>
    </div>
    """
  end
end
