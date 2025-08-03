defmodule Storybook.Molecules.NotificationToast do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.NotificationToast.notification_toast/1

  def doc do
    """
    # NotificationToast

    Toast notification component for temporary messages.

    ## Features

    - Temporary notification messages
    - Different types (success, error, warning, info)
    - Configurable duration
    - Dismissible option
    - Different positions (top-left, top-right, bottom-left, bottom-right)
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          message: "This is a notification message.",
          show: true
        }
      },
      %Variation{
        id: :with_title,
        attributes: %{
          title: "Notification Title",
          message: "This is a notification with a title.",
          show: true
        }
      },
      %Variation{
        id: :success,
        attributes: %{
          message: "Operation completed successfully.",
          type: "success",
          show: true
        }
      },
      %Variation{
        id: :error,
        attributes: %{
          message: "An error occurred while processing your request.",
          type: "error",
          show: true
        }
      },
      %Variation{
        id: :warning,
        attributes: %{
          message: "Please review the information before proceeding.",
          type: "warning",
          show: true
        }
      },
      %Variation{
        id: :info,
        attributes: %{
          message: "This is an informational message.",
          type: "info",
          show: true
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :non_dismissible,
        description: "Non-dismissible toast notification",
        template: """
        <.notification_toast
          message="This notification cannot be dismissed."
          show={true}
          dismissible={false}
        />
        """
      },
      %Example{
        id: :different_positions,
        description: "Toast notifications in different positions",
        template: """
        <div>
          <.notification_toast
            message="Top Right Notification"
            show={true}
            position="top-right"
          />
          <.notification_toast
            message="Bottom Left Notification"
            show={true}
            position="bottom-left"
          />
        </div>
        """
      },
      %Example{
        id: :with_custom_styling,
        description: "Toast notification with custom CSS classes",
        template: """
        <.notification_toast
          message="This notification has custom styling."
          show={true}
          class="bg-purple-100 border-purple-300"
        />
        """
      }
    ]
  end
end
