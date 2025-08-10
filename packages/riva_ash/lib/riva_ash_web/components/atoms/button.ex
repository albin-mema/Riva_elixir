alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.HTML, as: HTML

defmodule RivaAshWeb.Components.Atoms.Button do
  @deprecated "Use RivaAshWeb.Components.UI.Button instead"
  alias RivaAshWeb.Components.UI.Button, as: UIButton

  def button(assigns) do
    UIButton.button(assigns)
  end
end
