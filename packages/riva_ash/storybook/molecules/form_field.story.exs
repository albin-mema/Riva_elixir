defmodule Storybook.Molecules.FormField do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.FormField.form_field/1

  def doc do
    """
    # FormField

    FormField component that combines label, input, and error messages.

    ## Features

    - Complete form field experience
    - Label, input, helper text, and error messages
    - Different input types (text, email, password, etc.)
    - Icon support (left or right position)
    - Required and disabled states
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          field: %{name: "username", id: "username", value: "", errors: []},
          label: "Username",
          placeholder: "Enter your username"
        }
      },
      %Variation{
        id: :with_icon,
        attributes: %{
          field: %{name: "email", id: "email", value: "", errors: []},
          label: "Email Address",
          type: "email",
          placeholder: "user@example.com",
          icon: :envelope,
          icon_position: "left"
        }
      },
      %Variation{
        id: :with_helper_text,
        attributes: %{
          field: %{name: "password", id: "password", value: "", errors: []},
          label: "Password",
          type: "password",
          placeholder: "Enter your password",
          helper_text: "Password must be at least 8 characters long"
        }
      },
      %Variation{
        id: :required_field,
        attributes: %{
          field: %{name: "name", id: "name", value: "", errors: []},
          label: "Full Name",
          placeholder: "Enter your full name",
          required: true
        }
      },
      %Variation{
        id: :with_errors,
        attributes: %{
          field: %{name: "phone", id: "phone", value: "invalid", errors: ["is invalid"]},
          label: "Phone Number",
          type: "tel",
          placeholder: "Enter your phone number"
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :disabled_field,
        description: "Disabled form field",
        template: """
        <.form_field
          field={%{name: "disabled_field", id: "disabled_field", value: "Disabled value", errors: []}}
          label="Disabled Field"
          placeholder="This field is disabled"
          disabled={true}
        />
        """
      },
      %Example{
        id: :with_custom_styling,
        description: "Form field with custom CSS classes",
        template: """
        <.form_field
          field={%{name: "custom_field", id: "custom_field", value: "", errors: []}}
          label="Custom Styled Field"
          placeholder="This field has custom styling"
          class="bg-blue-50 border-blue-200"
        />
        """
      }
    ]
  end
end
