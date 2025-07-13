defmodule Storybook.Atoms.Checkbox do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Checkbox.checkbox/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          label: "Default checkbox"
        }
      },
      %Variation{
        id: :checked,
        attributes: %{
          label: "Checked checkbox",
          checked: true
        }
      },
      %Variation{
        id: :disabled,
        attributes: %{
          label: "Disabled checkbox",
          disabled: true
        }
      },
      %Variation{
        id: :with_description,
        attributes: %{
          label: "Checkbox with description",
          description: "This is a helpful description for the checkbox option."
        }
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="space-y-4">
          <.checkbox size="sm" label="Small checkbox" />
          <.checkbox size="md" label="Medium checkbox" />
          <.checkbox size="lg" label="Large checkbox" />
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="space-y-4">
          <.checkbox variant="default" label="Default variant" checked={true} />
          <.checkbox variant="error" label="Error variant" checked={true} />
          <.checkbox variant="success" label="Success variant" checked={true} />
        </div>
        """
      }
    ]
  end
end
