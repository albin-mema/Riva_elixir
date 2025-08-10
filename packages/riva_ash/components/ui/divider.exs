defmodule RivaAsh.Components.UI.Divider do
  @moduledoc """
  A divider component for separating content with consistent styling and orientation options.
  Implements proper ARIA attributes, design token usage, and accessibility features per atomic design specifications.
  """

  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms

  @doc """
  Renders a divider with specified orientation, spacing, and focusability.

  ## Examples

      <.divider orientation={:horizontal} spacing={:space-4} />
      <.divider orientation={:vertical} spacing={:space-2} />
      <.divider focusable={true} />
  """
  def divider(assigns) do
    assigns =
      assigns
      |> assign_new(:orientation, fn -> validate_orientation(assigns[:orientation] || :horizontal) end)
      |> assign_new(:spacing, fn -> validate_spacing(assigns[:spacing] || :space-4) end)
      |> assign_new(:focusable, fn -> !!assigns[:focusable] end)

    ~H"""
    <div
      role="separator"
      aria-orientation={to_string(assigns.orientation)}
      class={classes(assigns)}
      {@rest}
    />
    """
  end

  @doc """
  Validates orientation values to ensure only horizontal/vertical are used.
  """
  defp validate_orientation(orientation) when orientation in [:horizontal, :vertical], do: orientation
  defp validate_orientation(_), do: :horizontal

  @doc """
  Validates spacing values against design token scale.
  """
  defp validate_spacing(spacing) when spacing in [:space-0, :space-1, :space-2, :space-3, :space-4, :space-5, :space-6], do: spacing
  defp validate_spacing(_), do: :space-4

  @doc """
  Generates combined classes for the divider.
  """
  defp classes(assigns) do
    base =
      orientation_classes(assigns.orientation) <>
        " " <>
        spacing_classes(assigns.orientation, assigns.spacing)

    if assigns.focusable do
      base <> " " <> Atoms.focus_styles()
    else
      base
    end
  end

  @doc """
  Generates orientation-specific border classes using design tokens.
  """
  defp orientation_classes(:horizontal), do: "border-t border border-muted-foreground w-full"
  defp orientation_classes(:vertical), do: "border-l border border-muted-foreground h-full"

  @doc """
  Generates spacing classes based on orientation and design token values.
  """
  defp spacing_classes(:horizontal, spacing) do
    case spacing do
      :space-0 -> "my-0"
      :space-1 -> "my-1"
      :space-2 -> "my-2"
      :space-3 -> "my-3"
      :space-4 -> "my-4"
      :space-5 -> "my-5"
      :space-6 -> "my-6"
    end
  end

  defp spacing_classes(:vertical, spacing) do
    case spacing do
      :space-0 -> "mx-0"
      :space-1 -> "mx-1"
      :space-2 -> "mx-2"
      :space-3 -> "mx-3"
      :space-4 -> "mx-4"
      :space-5 -> "mx-5"
      :space-6 -> "mx-6"
    end
  end
end
