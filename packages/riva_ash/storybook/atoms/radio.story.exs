defmodule Storybook.Atoms.Radio do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Radio.radio/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          name: "example",
          value: "option1",
          label: "Option 1"
        }
      },
      %Variation{
        id: :checked,
        attributes: %{
          name: "example",
          value: "option2",
          label: "Selected option",
          checked: true
        }
      },
      %Variation{
        id: :disabled,
        attributes: %{
          name: "example",
          value: "option3",
          label: "Disabled option",
          disabled: true
        }
      },
      %Variation{
        id: :radio_group,
        template: """
        <div class="space-y-3">
          <div class="font-medium">Choose your preference:</div>
          <.radio name="preference" value="option1" label="Option 1" />
          <.radio name="preference" value="option2" label="Option 2" checked={true} />
          <.radio name="preference" value="option3" label="Option 3" />
          <.radio name="preference" value="option4" label="Disabled option" disabled={true} />
        </div>
        """
      },
      %Variation{
        id: :with_description,
        attributes: %{
          name: "example",
          value: "detailed",
          label: "Option with description",
          description: "This option includes additional helpful information."
        }
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="space-y-4">
          <.radio size="sm" name="size" value="small" label="Small radio" />
          <.radio size="md" name="size" value="medium" label="Medium radio" />
          <.radio size="lg" name="size" value="large" label="Large radio" />
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="space-y-4">
          <.radio variant="default" name="variant" value="default" label="Default variant" checked={true} />
          <.radio variant="error" name="variant" value="error" label="Error variant" checked={true} />
          <.radio variant="success" name="variant" value="success" label="Success variant" checked={true} />
        </div>
        """
      }
    ]
  end
end
