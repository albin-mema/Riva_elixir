defmodule Storybook.Atoms.ChatMessage do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.ChatMessage.chat_message/1

  def doc do
    """
    # Chat Message (Atom)

    A chat message component that displays individual messages with sender information, content, and timestamp.
    Supports different message alignments based on whether it's from the current user or others.

    ## Features

    - **Message alignment**: Own messages align right, others align left
    - **Sender information**: Shows sender name and optional avatar for other users
    - **Timestamps**: Formatted time display
    - **Message types**: Support for different message types (text, system, error)
    - **Responsive design**: Adapts to different screen sizes

    ## Usage

    ```heex
    <.chat_message 
      content="Hello everyone!" 
      sender_name="John Doe"
      timestamp={~N[2024-01-15 10:30:00]}
      is_own_message={false}
    />
    ```
    """
  end

  def variations do
    [
      %Variation{
        id: :other_user_message,
        attributes: %{
          content: "Hello everyone! How's the project going?",
          sender_name: "John Doe",
          timestamp: ~N[2024-01-15 10:30:00],
          is_own_message: false
        }
      },
      %Variation{
        id: :own_message,
        attributes: %{
          content: "Great! Just finished the new feature.",
          sender_name: "You",
          timestamp: ~N[2024-01-15 10:31:00],
          is_own_message: true
        }
      },
      %Variation{
        id: :with_avatar,
        attributes: %{
          content: "Thanks for the update!",
          sender_name: "Alice Smith",
          sender_avatar: "https://images.unsplash.com/photo-1494790108755-2616b612b786?w=32&h=32&fit=crop&crop=face",
          timestamp: ~N[2024-01-15 10:32:00],
          is_own_message: false
        }
      },
      %Variation{
        id: :system_message,
        attributes: %{
          content: "John Doe joined the room",
          sender_name: "System",
          timestamp: ~N[2024-01-15 10:29:00],
          is_own_message: false,
          message_type: "system"
        }
      },
      %Variation{
        id: :error_message,
        attributes: %{
          content: "Failed to send message. Please try again.",
          sender_name: "System",
          timestamp: ~N[2024-01-15 10:33:00],
          is_own_message: false,
          message_type: "error"
        }
      },
      %Variation{
        id: :long_message,
        attributes: %{
          content:
            "This is a much longer message to demonstrate how the component handles text wrapping and maintains readability even with extensive content. The message bubble should expand appropriately while maintaining proper spacing and alignment.",
          sender_name: "Jane Wilson",
          timestamp: ~N[2024-01-15 10:34:00],
          is_own_message: false
        }
      },
      %Variation{
        id: :conversation_flow,
        template: """
        <div class="space-y-2 max-w-md mx-auto p-4 bg-gray-50 rounded-lg">
          <.chat_message 
            content="Hey team, are we still on for the meeting at 3 PM?"
            sender_name="Sarah"
            timestamp={~N[2024-01-15 14:45:00]}
            is_own_message={false}
          />
          <.chat_message 
            content="Yes, I'll be there!"
            sender_name="You"
            timestamp={~N[2024-01-15 14:46:00]}
            is_own_message={true}
          />
          <.chat_message 
            content="Perfect! I'll send the agenda shortly."
            sender_name="Sarah"
            timestamp={~N[2024-01-15 14:47:00]}
            is_own_message={false}
          />
          <.chat_message 
            content="Sarah joined the room"
            sender_name="System"
            timestamp={~N[2024-01-15 14:44:00]}
            is_own_message={false}
            message_type="system"
          />
        </div>
        """
      }
    ]
  end
end
