defmodule Storybook.UI.Input do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Input.input/1

  def doc do
    """
    # Input

    Displays a form input field.

    ## Features

    - Multiple variants: default, error, success
    - Multiple sizes: sm, default, lg
    - Disabled and readonly states
    - Form field integration
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{placeholder: "Default input"},
        slots: []
      },
      %Variation{
        id: :error,
        attributes: %{variant: "error", placeholder: "Error input"},
        slots: []
      },
      %Variation{
        id: :success,
        attributes: %{variant: "success", placeholder: "Success input"},
        slots: []
      },
      %Variation{
        id: :small,
        attributes: %{size: "sm", placeholder: "Small input"},
        slots: []
      },
      %Variation{
        id: :large,
        attributes: %{size: "lg", placeholder: "Large input"},
        slots: []
      },
      %Variation{
        id: :disabled,
        attributes: %{disabled: true, placeholder: "Disabled input"},
        slots: []
      },
      %Variation{
        id: :readonly,
        attributes: %{readonly: true, value: "Readonly input"},
        slots: []
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_label,
        description: "Input with label",
        template: """
        <div class="space-y-2">
          <label class="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
            Email
          </label>
          <Input type="email" placeholder="Enter your email" />
        </div>
        """
      },
      %Example{
        id: :with_validation,
        description: "Input with validation states",
        template: """
        <div class="space-y-4">
          <div class="space-y-2">
            <label class="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
              Valid input
            </label>
            <Input variant="success" placeholder="Valid input" />
            <p class="text-sm text-[var(--chart-5)]">This is a valid input</p>
          </div>
          <div class="space-y-2">
            <label class="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
              Invalid input
            </label>
            <Input variant="error" placeholder="Invalid input" />
            <p class="text-sm text-destructive">This is an invalid input</p>
          </div>
        </div>
        """
      }
    ]
  end
end
