defmodule Storybook.UI.Select do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Select.select/1

  def doc do
    """
    # Select

    Displays a list of options for the user to pick from.

    ## Features

    - Multiple variants: default, error, success
    - Multiple sizes: sm, default, lg
    - Disabled state
    - Prompt support
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          prompt: "Select an option",
          options: [{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]
        },
        slots: []
      },
      %Variation{
        id: :error,
        attributes: %{
          variant: "error",
          prompt: "Error select",
          options: [{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]
        },
        slots: []
      },
      %Variation{
        id: :success,
        attributes: %{
          variant: "success",
          prompt: "Success select",
          options: [{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]
        },
        slots: []
      },
      %Variation{
        id: :small,
        attributes: %{
          size: "sm",
          prompt: "Small select",
          options: [{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]
        },
        slots: []
      },
      %Variation{
        id: :large,
        attributes: %{
          size: "lg",
          prompt: "Large select",
          options: [{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]
        },
        slots: []
      },
      %Variation{
        id: :disabled,
        attributes: %{
          disabled: true,
          prompt: "Disabled select",
          options: [{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]
        },
        slots: []
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_label,
        description: "Select with label",
        template: """
        <div class="space-y-2">
          <label class="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
            Select option
          </label>
          <Select prompt="Choose an option" options={[{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]} />
        </div>
        """
      },
      %Example{
        id: :multiple,
        description: "Multiple select",
        template: """
        <div class="space-y-2">
          <label class="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
            Select multiple options
          </label>
          <Select
            multiple={true}
            prompt="Choose options"
            options={[{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]}
          />
        </div>
        """
      }
    ]
  end
end
