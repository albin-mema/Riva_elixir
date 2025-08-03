defmodule Storybook.Atoms.TextInput do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.TextInput.text_input/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          name: "username",
          placeholder: "Enter your username"
        }
      },
      %Variation{
        id: :with_value,
        attributes: %{
          name: "name",
          value: "John Doe"
        }
      },
      %Variation{
        id: :input_types,
        template: """
        <div class="space-y-4">
          <.text_input type="text" name="text" placeholder="Text input" />
          <.text_input type="email" name="email" placeholder="Email input" />
          <.text_input type="password" name="password" placeholder="Password input" />
        </div>
        """
      },
      %Variation{
        id: :states,
        template: """
        <div class="space-y-4">
          <.text_input name="default" placeholder="Default state" />
          <.text_input name="disabled" placeholder="Disabled state" disabled={true} />
        </div>
        """
      },
      %Variation{
        id: :phoenix_events,
        template: """
        <div class="space-y-4">
          <.text_input name="search" placeholder="Search..." phx-change="search" phx-debounce="300" />
          <.text_input name="keydown" placeholder="Press any key..." phx-keydown="handle_key" />
        </div>
        """
      },
      %Variation{
        id: :custom_styling,
        template: """
        <div class="space-y-4">
          <.text_input name="custom1" placeholder="Full width input" class="w-full" />
          <.text_input name="custom2" placeholder="Large input" class="text-lg px-4 py-3" />
          <.text_input name="custom3" placeholder="Rounded input" class="rounded-full" />
        </div>
        """
      }
    ]
  end
end
