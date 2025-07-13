defmodule Storybook.Atoms.DatePicker do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.DatePicker.date_picker/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          placeholder: "Select a date"
        }
      },
      %Variation{
        id: :with_value,
        attributes: %{
          value: "2024-01-15",
          placeholder: "Select a date"
        }
      },
      %Variation{
        id: :disabled,
        attributes: %{
          placeholder: "Select a date",
          disabled: true
        }
      },
      %Variation{
        id: :readonly,
        attributes: %{
          value: "2024-01-15",
          readonly: true
        }
      },
      %Variation{
        id: :required,
        attributes: %{
          placeholder: "Select a date",
          required: true
        }
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="space-y-4">
          <.date_picker size="sm" placeholder="Small date picker" />
          <.date_picker size="md" placeholder="Medium date picker" />
          <.date_picker size="lg" placeholder="Large date picker" />
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="space-y-4">
          <.date_picker variant="default" placeholder="Default variant" />
          <.date_picker variant="error" placeholder="Error variant" />
          <.date_picker variant="success" value="2024-01-15" />
        </div>
        """
      }
    ]
  end
end
