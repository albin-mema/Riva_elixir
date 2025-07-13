defmodule Storybook.Atoms.Toggle do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Toggle.toggle/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          label: "Default toggle"
        }
      },
      %Variation{
        id: :checked,
        attributes: %{
          label: "Checked toggle",
          checked: true
        }
      },
      %Variation{
        id: :disabled,
        attributes: %{
          label: "Disabled toggle",
          disabled: true
        }
      },
      %Variation{
        id: :disabled_checked,
        attributes: %{
          label: "Disabled checked toggle",
          disabled: true,
          checked: true
        }
      },
      %Variation{
        id: :with_description,
        attributes: %{
          label: "Toggle with description",
          description: "This toggle controls an important setting in your application."
        }
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="space-y-4">
          <.toggle size="sm" label="Small toggle" />
          <.toggle size="md" label="Medium toggle" />
          <.toggle size="lg" label="Large toggle" />
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="space-y-4">
          <.toggle variant="default" label="Default variant" checked={true} />
          <.toggle variant="success" label="Success variant" checked={true} />
          <.toggle variant="warning" label="Warning variant" checked={true} />
          <.toggle variant="destructive" label="Destructive variant" checked={true} />
        </div>
        """
      }
    ]
  end
end
