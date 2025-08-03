defmodule Storybook.Molecules.ProgressBar do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.ProgressBar.progress_bar/1

  def doc do
    """
    # ProgressBar

    Progress bar component for indicating completion status.

    ## Features

    - Visual progress indicator
    - Configurable value and maximum
    - Optional label and percentage display
    - Different sizes (sm, md, lg)
    - Different variants (default, success, warning, error)
    - Animated option
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          value: 50,
          max: 100
        }
      },
      %Variation{
        id: :with_label,
        attributes: %{
          value: 75,
          max: 100,
          label: "Processing..."
        }
      },
      %Variation{
        id: :with_percentage,
        attributes: %{
          value: 30,
          max: 100,
          show_percentage: true
        }
      },
      %Variation{
        id: :success,
        attributes: %{
          value: 100,
          max: 100,
          label: "Complete",
          variant: "success"
        }
      },
      %Variation{
        id: :warning,
        attributes: %{
          value: 60,
          max: 100,
          label: "Warning",
          variant: "warning"
        }
      },
      %Variation{
        id: :error,
        attributes: %{
          value: 40,
          max: 100,
          label: "Error",
          variant: "error"
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :different_sizes,
        description: "Progress bars in different sizes",
        template: """
        <div class="space-y-4">
          <.progress_bar
            value={30}
            max={100}
            label="Small"
            size="sm"
          />
          <.progress_bar
            value={50}
            max={100}
            label="Medium"
            size="md"
          />
          <.progress_bar
            value={70}
            max={100}
            label="Large"
            size="lg"
          />
        </div>
        """
      },
      %Example{
        id: :animated,
        description: "Animated progress bar",
        template: """
        <.progress_bar
          value={60}
          max={100}
          label="Animated Progress"
          animated={true}
        />
        """
      },
      %Example{
        id: :with_custom_styling,
        description: "Progress bar with custom CSS classes",
        template: """
        <.progress_bar
          value={80}
          max={100}
          label="Custom Style"
          class="bg-purple-100 border-purple-300"
        />
        """
      }
    ]
  end
end
