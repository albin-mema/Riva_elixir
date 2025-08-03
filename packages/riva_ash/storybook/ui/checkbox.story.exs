defmodule Storybook.UI.Checkbox do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Checkbox.checkbox/1

  def doc do
    """
    # Checkbox

    A control that allows the user to toggle between checked and not checked.

    ## Features

    - Multiple variants: default, error, success
    - Multiple sizes: sm, default, lg
    - Disabled state
    - Label and description support
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{label: "Default checkbox"},
        slots: []
      },
      %Variation{
        id: :error,
        attributes: %{variant: "error", label: "Error checkbox"},
        slots: []
      },
      %Variation{
        id: :success,
        attributes: %{variant: "success", label: "Success checkbox"},
        slots: []
      },
      %Variation{
        id: :small,
        attributes: %{size: "sm", label: "Small checkbox"},
        slots: []
      },
      %Variation{
        id: :large,
        attributes: %{size: "lg", label: "Large checkbox"},
        slots: []
      },
      %Variation{
        id: :disabled,
        attributes: %{disabled: true, label: "Disabled checkbox"},
        slots: []
      },
      %Variation{
        id: :checked,
        attributes: %{checked: true, label: "Checked checkbox"},
        slots: []
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_description,
        description: "Checkbox with description",
        template: """
        <Checkbox
          label="Accept terms and conditions"
          description="You agree to our Terms of Service and Privacy Policy."
        />
        """
      },
      %Example{
        id: :group,
        description: "Checkbox group",
        template: """
        <div class="space-y-2">
          <Checkbox label="Option 1" />
          <Checkbox label="Option 2" />
          <Checkbox label="Option 3" />
        </div>
        """
      }
    ]
  end
end
