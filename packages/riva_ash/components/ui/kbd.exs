defmodule RivaAsh.Components.UI.Kbd do
  @moduledoc """
  A keyboard key component that displays keys with consistent styling.
  """

  use Surface.Component

  import RivaAsh.Components.UI.Atoms, only: [standard_assigns: 2]

  @doc """
  Renders a keyboard key with the specified variant and text.
  """
  def render(assigns) do
    assigns = standard_assigns(assigns, variant: :default)

    ~H"""
    <kbd
      class={[
        "font-mono rounded-xs bg-card text-muted-foreground inline-flex",
        size_classes(@variant),
        assigns[:class]
      ]}
      {@rest}
      {@aria_attributes}
      {@interactive && focus_styles()}
    >
      <%= @text %>
    </kbd>
    """
  end

  @doc """
  Generates size-specific classes based on the variant.
  """
  defp size_classes(:small), do: "px-1 py-0.5 text-xs"
  defp size_classes(_), do: "px-2 py-1 text-sm"

  @doc """
  Applies focus styles when the component is interactive.
  """
  defp focus_styles do
    "focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"
  end

  @doc """
  Generates ARIA attributes for accessibility compliance.
  """
  defp aria_attributes(assigns) do
    Atoms.aria_attributes(assigns)
  end
end
