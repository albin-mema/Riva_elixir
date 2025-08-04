defmodule RivaAshWeb.Components.Atoms.AllAtoms do
  @moduledoc """
  This module serves as a central point for importing all atomic components.
  """
  use Phoenix.Component

  # Import individual atom components here
  # Note: Leave imports only when this aggregator is actually re-exporting usages in this module.
  # Since this module doesn't reference the components directly, remove unused imports to avoid warnings.
  # Downstream code should import the specific components or the canonical UI.* components directly.

  # Components are available through imports above
end
