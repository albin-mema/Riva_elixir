defmodule RivaAshWeb.Components.Atoms.AllAtoms do
  @moduledoc """
  This module serves as a central point for importing all atomic components.
  """
  use Phoenix.Component

  # Import individual atom components here
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.TextInput

  # Re-export components for convenience
  def button(assigns), do: Button.button(assigns)
  def text_input(assigns), do: TextInput.text_input(assigns)
end
