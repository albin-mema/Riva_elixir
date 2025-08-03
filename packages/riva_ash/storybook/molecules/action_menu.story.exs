defmodule Storybook.Molecules.ActionMenu do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.ActionMenu.action_menu/1

  def doc do
    """
    # ActionMenu

    Dropdown action menu component.

    ## Features

    - Dropdown menu with customizable trigger
    - Configurable position (bottom-left, bottom-right, top-left, top-right)
    - Different sizes (sm, md, lg)
    - Custom CSS classes
    - Icon support in trigger button
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          actions: [
            %{label: "Edit", action: "edit"},
            %{label: "Delete", action: "delete"},
            %{label: "View Details", action: "view"}
          ]
        }
      },
      %Variation{
        id: :with_custom_trigger,
        attributes: %{
          trigger_label: "More Options",
          trigger_icon: :cog,
          actions: [
            %{label: "Settings", action: "settings"},
            %{label: "Preferences", action: "preferences"},
            %{label: "Help", action: "help"}
          ]
        }
      },
      %Variation{
        id: :different_positions,
        template: """
        <div class="flex space-x-4">
          <.action_menu
            actions={[%{label: "Top Left", action: "top-left"}]}
            position="top-left"
            trigger_label="Top Left"
          />
          <.action_menu
            actions={[%{label: "Top Right", action: "top-right"}]}
            position="top-right"
            trigger_label="Top Right"
          />
          <.action_menu
            actions={[%{label: "Bottom Left", action: "bottom-left"}]}
            position="bottom-left"
            trigger_label="Bottom Left"
          />
          <.action_menu
            actions={[%{label: "Bottom Right", action: "bottom-right"}]}
            position="bottom-right"
            trigger_label="Bottom Right"
          />
        </div>
        """
      },
      %Variation{
        id: :different_sizes,
        template: """
        <div class="flex items-center space-x-4">
          <.action_menu
            actions={[%{label: "Small", action: "small"}]}
            size="sm"
            trigger_label="Small"
          />
          <.action_menu
            actions={[%{label: "Medium", action: "medium"}]}
            size="md"
            trigger_label="Medium"
          />
          <.action_menu
            actions={[%{label: "Large", action: "large"}]}
            size="lg"
            trigger_label="Large"
          />
        </div>
        """
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Action menu with custom CSS classes",
        template: """
        <.action_menu
          actions={[%{label: "Custom Style", action: "custom"}]}
          class="bg-blue-100 border-blue-300"
          trigger_label="Custom Menu"
        />
        """
      }
    ]
  end
end
