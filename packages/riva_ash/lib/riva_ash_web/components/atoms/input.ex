alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.HTML, as: HTML

defmodule RivaAshWeb.Components.Atoms.Input do
  @deprecated "Use RivaAshWeb.Components.UI.Input instead"
  alias RivaAshWeb.Components.UI.Input, as: UIInput

  def input(assigns) do
    UIInput.input(assigns)
  end
end
