defmodule Storybook.Organisms.ClientForm do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.ClientForm.client_form/1

  def variations do
    [
      %Variation{
        id: :registration_form,
        attributes: %{
          form: %{
            id: "client-form",
            source: %{
              first_name: "",
              last_name: "",
              email: "",
              phone: "",
              is_registered: false,
              password: ""
            },
            errors: []
          },
          editing: false,
          show_registration_fields: true,
          on_submit: "save_client",
          on_change: "validate_client",
          on_cancel: "cancel_form",
          loading: false
        }
      },
      %Variation{
        id: :edit_form,
        attributes: %{
          form: %{
            id: "client-form",
            source: %{
              first_name: "John",
              last_name: "Doe",
              email: "john.doe@example.com",
              phone: "+1 (555) 123-4567",
              is_registered: true,
              password: ""
            },
            errors: []
          },
          editing: true,
          show_registration_fields: true,
          on_submit: "save_client",
          on_change: "validate_client",
          on_cancel: "cancel_form",
          loading: false
        }
      },
      %Variation{
        id: :loading_form,
        attributes: %{
          form: %{
            id: "client-form",
            source: %{
              first_name: "John",
              last_name: "Doe",
              email: "john.doe@example.com",
              phone: "+1 (555) 123-4567",
              is_registered: true,
              password: ""
            },
            errors: []
          },
          editing: true,
          show_registration_fields: true,
          on_submit: "save_client",
          on_change: "validate_client",
          on_cancel: "cancel_form",
          loading: true
        }
      }
    ]
  end
end
