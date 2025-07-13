defmodule RivaAshWeb.Components.Atoms.Checkbox do
  @moduledoc """
  Checkbox component with labels and validation states.
  """
  use Phoenix.Component

  @doc """
  Renders a checkbox input.
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

  def checkbox(assigns) do
    ~H"""
    <!-- Checkbox implementation will go here -->
    <div>
      <input type="checkbox" {@rest} />
      <label :if={@label}><%= @label %></label>
    </div>
    """
  end
end
