alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.HTML, as: HTML

defmodule RivaAshWeb.Components.Atoms.Toggle do
  @deprecated "Use RivaAshWeb.Components.UI.Toggle instead"
  alias RivaAshWeb.Components.UI.Toggle, as: UIToggle

  def toggle(assigns) do
    UIToggle.toggle(assigns)
  end
end
