defmodule Storybook.Atoms.Input do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Input.input/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          placeholder: "Enter text..."
        }
      },
      %Variation{
        id: :with_value,
        attributes: %{
          value: "Sample text",
          placeholder: "Enter text..."
        }
      },
      %Variation{
        id: :input_types,
        template: """
        <div class="space-y-4">
          <.input type="text" placeholder="Text input" />
          <.input type="email" placeholder="Email input" />
          <.input type="password" placeholder="Password input" />
          <.input type="number" placeholder="Number input" />
          <.input type="tel" placeholder="Phone input" />
          <.input type="url" placeholder="URL input" />
          <.input type="search" placeholder="Search input" />
        </div>
        """
      },
      %Variation{
        id: :states,
        template: """
        <div class="space-y-4">
          <.input placeholder="Default state" />
          <.input placeholder="Disabled state" disabled={true} />
          <.input placeholder="Readonly state" readonly={true} />
          <.input placeholder="Required field" required={true} />
        </div>
        """
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="space-y-4">
          <.input size="sm" placeholder="Small input" />
          <.input size="md" placeholder="Medium input" />
          <.input size="lg" placeholder="Large input" />
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="space-y-4">
          <.input variant="default" placeholder="Default variant" />
          <.input variant="error" placeholder="Error variant" />
          <.input variant="success" value="Valid input" />
        </div>
        """
      }
    ]
  end
end
