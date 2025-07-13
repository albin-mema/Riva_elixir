defmodule RivaAshWeb.Components.Atoms.Select do
  @moduledoc """
  Select dropdown component with search functionality and consistent styling.
  """
  use Phoenix.Component

  @doc """
  Renders a select dropdown.
  """
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :options, :list, default: []
  attr :prompt, :string, default: nil
  attr :multiple, :boolean, default: false
  attr :searchable, :boolean, default: false
  attr :disabled, :boolean, default: false
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :class, :string, default: ""
  attr :rest, :global

  def select(assigns) do
    ~H"""
    <!-- Select implementation will go here -->
    <select {@rest}>
      <option :if={@prompt} value=""><%= @prompt %></option>
      <option :for={{label, value} <- @options} value={value}><%= label %></option>
    </select>
    """
  end
end
