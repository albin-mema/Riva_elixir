defmodule Storybook.Atoms.TimePicker do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.TimePicker.time_picker/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          placeholder: "Select time"
        }
      },
      %Variation{
        id: :with_value,
        attributes: %{
          value: "14:30",
          placeholder: "Select time"
        }
      },
      %Variation{
        id: :disabled,
        attributes: %{
          placeholder: "Select time",
          disabled: true
        }
      },
      %Variation{
        id: :readonly,
        attributes: %{
          value: "09:15",
          readonly: true
        }
      },
      %Variation{
        id: :required,
        attributes: %{
          placeholder: "Select time",
          required: true
        }
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="space-y-4">
          <.time_picker size="sm" placeholder="Small time picker" />
          <.time_picker size="md" placeholder="Medium time picker" />
          <.time_picker size="lg" placeholder="Large time picker" />
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="space-y-4">
          <.time_picker variant="default" placeholder="Default variant" />
          <.time_picker variant="error" placeholder="Error variant" />
          <.time_picker variant="success" value="10:00" />
        </div>
        """
      }
    ]
  end
end
