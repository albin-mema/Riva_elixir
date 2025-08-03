defmodule Storybook.Atoms.Button do
  use PhoenixStorybook.Story, :component

  def component, do: &RivaAshWeb.Components.Atoms.Button.button/1

  def template do
    """
    <div class="sb-p-4">
      <.button variant={@variant} {@rest}>
        <%= @label %>
      </.button>
    </div>
    """
  end

  def variations do
    [
      %Variation{
        id: :primary,
        attributes: %{
          label: "Primary Button",
          variant: :primary
        }
      },
      %Variation{
        id: :secondary,
        attributes: %{
          label: "Secondary Button",
          variant: :secondary
        }
      },
      %Variation{
        id: :disabled,
        attributes: %{
          label: "Disabled Button",
          variant: :primary,
          disabled: true
        }
      }
    ]
  end
end
