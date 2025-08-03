defmodule Storybook.Organisms.BusinessForm do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.BusinessForm.business_form/1

  def variations do
    [
      %Variation{
        id: :create_form,
        attributes: %{
          form: %{
            id: "business-form",
            source: %{
              name: "",
              description: "",
              is_public_searchable: false,
              public_description: "",
              city: "",
              country: "",
              address: ""
            },
            errors: []
          },
          editing: false,
          loading: false,
          on_submit: "save_business",
          on_change: "validate_business",
          on_cancel: "cancel_form"
        }
      },
      %Variation{
        id: :edit_form,
        attributes: %{
          form: %{
            id: "business-form",
            source: %{
              name: "Sunny Beach Resort",
              description: "A beautiful beachfront resort with stunning ocean views and world-class amenities.",
              is_public_searchable: true,
              public_description: "Luxury beach resort with ocean views",
              city: "Miami Beach",
              country: "USA",
              address: "123 Ocean Drive, Miami Beach, FL 33139"
            },
            errors: []
          },
          editing: true,
          loading: false,
          on_submit: "save_business",
          on_change: "validate_business",
          on_cancel: "cancel_form"
        }
      },
      %Variation{
        id: :loading_form,
        attributes: %{
          form: %{
            id: "business-form",
            source: %{
              name: "Sunny Beach Resort",
              description: "A beautiful beachfront resort with stunning ocean views and world-class amenities.",
              is_public_searchable: true,
              public_description: "Luxury beach resort with ocean views",
              city: "Miami Beach",
              country: "USA",
              address: "123 Ocean Drive, Miami Beach, FL 33139"
            },
            errors: []
          },
          editing: true,
          loading: true,
          on_submit: "save_business",
          on_change: "validate_business",
          on_cancel: "cancel_form"
        }
      }
    ]
  end
end
