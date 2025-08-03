defmodule Storybook.Organisms.ItemForm do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.ItemForm.item_form/1

  def variations do
    [
      %Variation{
        id: :create_form,
        attributes: %{
          form: %{
            id: "item-form",
            source: %{
              name: "",
              description: "",
              section_id: "",
              item_type_id: "",
              is_active: true,
              is_always_available: false,
              capacity: "",
              is_public_searchable: false,
              public_description: "",
              grid_row: "",
              grid_column: ""
            },
            errors: []
          },
          sections: [
            {"Beach Area", "1"},
            {"Pool Area", "2"},
            {"Restaurant", "3"}
          ],
          item_types: [
            {"Umbrella", "1"},
            {"Sunbed", "2"},
            {"Cabin", "3"}
          ],
          editing: false,
          on_submit: "save_item",
          on_change: "validate_item",
          on_cancel: "cancel_form",
          loading: false
        }
      },
      %Variation{
        id: :edit_form,
        attributes: %{
          form: %{
            id: "item-form",
            source: %{
              name: "Premium Beach Umbrella",
              description: "Large umbrella with UV protection",
              section_id: "1",
              item_type_id: "1",
              is_active: true,
              is_always_available: false,
              capacity: "4",
              is_public_searchable: true,
              public_description: "Premium beach umbrella with UV protection",
              grid_row: "2",
              grid_column: "5"
            },
            errors: []
          },
          sections: [
            {"Beach Area", "1"},
            {"Pool Area", "2"},
            {"Restaurant", "3"}
          ],
          item_types: [
            {"Umbrella", "1"},
            {"Sunbed", "2"},
            {"Cabin", "3"}
          ],
          editing: true,
          on_submit: "save_item",
          on_change: "validate_item",
          on_cancel: "cancel_form",
          loading: false
        }
      },
      %Variation{
        id: :loading_form,
        attributes: %{
          form: %{
            id: "item-form",
            source: %{
              name: "Premium Beach Umbrella",
              description: "Large umbrella with UV protection",
              section_id: "1",
              item_type_id: "1",
              is_active: true,
              is_always_available: false,
              capacity: "4",
              is_public_searchable: true,
              public_description: "Premium beach umbrella with UV protection",
              grid_row: "2",
              grid_column: "5"
            },
            errors: []
          },
          sections: [
            {"Beach Area", "1"},
            {"Pool Area", "2"},
            {"Restaurant", "3"}
          ],
          item_types: [
            {"Umbrella", "1"},
            {"Sunbed", "2"},
            {"Cabin", "3"}
          ],
          editing: true,
          on_submit: "save_item",
          on_change: "validate_item",
          on_cancel: "cancel_form",
          loading: true
        }
      }
    ]
  end
end
