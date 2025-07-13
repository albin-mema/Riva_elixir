defmodule RivaAshWeb.Components.Atoms.Input do
  @moduledoc """
  Standardized input component with validation states and consistent styling.
  """
  use Phoenix.Component

  @doc """
  Renders a standardized input field.
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
    ~H"""
    <!-- Input implementation will go here -->
    <input {@rest} />
    """
  end
end
