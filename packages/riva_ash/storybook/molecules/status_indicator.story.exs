defmodule Storybook.Molecules.StatusIndicator do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.StatusIndicator.status_indicator/1

  def doc do
    """
    # StatusIndicator

    Status indicator component with colors and icons.

    ## Features

    - Visual status indicator with appropriate styling
    - Configurable status label
    - Icon support
    - Pulse animation option
    - Different sizes (sm, md, lg)
    - Different variants (auto, success, warning, error, info)
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          status: "active",
          label: "Active"
        }
      },
      %Variation{
        id: :without_icon,
        attributes: %{
          status: "pending",
          label: "Pending",
          show_icon: false
        }
      },
      %Variation{
        id: :with_pulse,
        attributes: %{
          status: "online",
          label: "Online",
          show_pulse: true
        }
      },
      %Variation{
        id: :success,
        attributes: %{
          status: "completed",
          label: "Completed",
          variant: "success"
        }
      },
      %Variation{
        id: :warning,
        attributes: %{
          status: "warning",
          label: "Warning",
          variant: "warning"
        }
      },
      %Variation{
        id: :error,
        attributes: %{
          status: "error",
          label: "Error",
          variant: "error"
        }
      },
      %Variation{
        id: :info,
        attributes: %{
          status: "info",
          label: "Info",
          variant: "info"
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :different_sizes,
        description: "Status indicators in different sizes",
        template: """
        <div class="flex items-center space-x-4">
          <.status_indicator
            status="small"
            label="Small"
            size="sm"
          />
          <.status_indicator
            status="medium"
            label="Medium"
            size="md"
          />
          <.status_indicator
            status="large"
            label="Large"
            size="lg"
          />
        </div>
        """
      },
      %Example{
        id: :with_custom_styling,
        description: "Status indicator with custom CSS classes",
        template: """
        <.status_indicator
          status="custom"
          label="Custom Style"
          class="bg-purple-100 text-purple-800"
        />
        """
      }
    ]
  end
end
