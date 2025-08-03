defmodule Storybook.UI.Textarea do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Textarea.textarea/1

  def doc do
    """
    # Textarea

    Displays a multi-line text input field.

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
        attributes: %{placeholder: "Default textarea"},
        slots: []
      },
      %Variation{
        id: :error,
        attributes: %{variant: "error", placeholder: "Error textarea"},
        slots: []
      },
      %Variation{
        id: :success,
        attributes: %{variant: "success", placeholder: "Success textarea"},
        slots: []
      },
      %Variation{
        id: :small,
        attributes: %{size: "sm", placeholder: "Small textarea"},
        slots: []
      },
      %Variation{
        id: :large,
        attributes: %{size: "lg", placeholder: "Large textarea"},
        slots: []
      },
      %Variation{
        id: :disabled,
        attributes: %{disabled: true, placeholder: "Disabled textarea"},
        slots: []
      },
      %Variation{
        id: :readonly,
        attributes: %{readonly: true, value: "Readonly textarea"},
        slots: []
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_label,
        description: "Textarea with label",
        template: """
        <div class="space-y-2">
          <label class="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
            Description
          </label>
          <Textarea placeholder="Enter a description" rows="4" />
        </div>
        """
      },
      %Example{
        id: :with_validation,
        description: "Textarea with validation states",
        template: """
        <div class="space-y-4">
          <div class="space-y-2">
            <label class="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
              Valid textarea
            </label>
            <Textarea variant="success" placeholder="Valid textarea" rows="3" />
            <p class="text-sm text-[var(--chart-5)]">This is a valid textarea</p>
          </div>
          <div class="space-y-2">
            <label class="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
              Invalid textarea
            </label>
            <Textarea variant="error" placeholder="Invalid textarea" rows="3" />
            <p class="text-sm text-destructive">This is an invalid textarea</p>
          </div>
        </div>
        """
      }
    ]
  end
end
