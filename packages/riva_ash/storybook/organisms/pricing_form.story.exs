defmodule Storybook.Organisms.PricingForm do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.PricingForm.pricing_form/1

  def variations do
    [
      %Variation{
        id: :create_form,
        attributes: %{
          form: %{
            id: "pricing-form",
            source: %{
              item_id: "",
              price_per_day: "",
              currency: "USD",
              effective_from: "",
              effective_until: "",
              notes: ""
            },
            errors: []
          },
          items: [
            %{id: "1", name: "Beach Umbrella"},
            %{id: "2", name: "Sunbed"},
            %{id: "3", name: "Cabin"}
          ],
          editing: false,
          on_submit: "save_pricing",
          on_change: "validate_pricing",
          on_cancel: "cancel_form",
          loading: false
        }
      },
      %Variation{
        id: :edit_form,
        attributes: %{
          form: %{
            id: "pricing-form",
            source: %{
              item_id: "1",
              price_per_day: "25.00",
              currency: "USD",
              effective_from: "2024-06-01",
              effective_until: "2024-08-31",
              notes: "Summer pricing"
            },
            errors: []
          },
          items: [
            %{id: "1", name: "Beach Umbrella"},
            %{id: "2", name: "Sunbed"},
            %{id: "3", name: "Cabin"}
          ],
          editing: true,
          on_submit: "save_pricing",
          on_change: "validate_pricing",
          on_cancel: "cancel_form",
          loading: false
        }
      },
      %Variation{
        id: :loading_form,
        attributes: %{
          form: %{
            id: "pricing-form",
            source: %{
              item_id: "1",
              price_per_day: "25.00",
              currency: "USD",
              effective_from: "2024-06-01",
              effective_until: "2024-08-31",
              notes: "Summer pricing"
            },
            errors: []
          },
          items: [
            %{id: "1", name: "Beach Umbrella"},
            %{id: "2", name: "Sunbed"},
            %{id: "3", name: "Cabin"}
          ],
          editing: true,
          on_submit: "save_pricing",
          on_change: "validate_pricing",
          on_cancel: "cancel_form",
          loading: true
        }
      }
    ]
  end
end
