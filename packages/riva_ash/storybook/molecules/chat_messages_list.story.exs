defmodule Storybook.Molecules.ChatMessagesList do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.ChatMessagesList.chat_messages_list/1

  def doc do
    """
    # Chat Messages List (Molecule)

    A scrollable list of chat messages with auto-scroll functionality and empty states.
    Handles message grouping, spacing, and optimized rendering for chat conversations.

    ## Features

    - **Auto-scroll**: Automatically scrolls to newest messages
    - **Message grouping**: Smart spacing based on sender and time gaps
    - **Empty states**: Customizable empty state when no messages
    - **Loading states**: Shows loading indicator while fetching messages
    - **Performance optimized**: Efficient rendering for large message lists
    - **Responsive design**: Adapts to different container sizes

    ## Usage

    ```heex
    <.chat_messages_list 
      messages={@messages}
      current_user_id={@current_user.id}
      loading={@loading}
    />
    ```
    """
  end

  def variations do
    [
      %Variation{
        id: :empty_state,
        attributes: %{
          messages: [],
          current_user_id: "user-123",
          loading: false
        }
      },
      %Variation{
        id: :loading_state,
        attributes: %{
          messages: [],
          current_user_id: "user-123",
          loading: true
        }
      },
      %Variation{
        id: :single_message,
        attributes: %{
          messages: [
            %{
              id: "msg-1",
              content: "Hello everyone!",
              sender_id: "user-456",
              sender: %{name: "John Doe", email: "john@example.com"},
              inserted_at: ~N[2024-01-15 10:30:00]
            }
          ],
          current_user_id: "user-123",
          loading: false
        }
      },
      %Variation{
        id: :conversation,
        attributes: %{
          messages: [
            %{
              id: "msg-1",
              content: "Hey team, how's everyone doing today?",
              sender_id: "user-456",
              sender: %{name: "Alice Smith", email: "alice@example.com"},
              inserted_at: ~N[2024-01-15 09:00:00]
            },
            %{
              id: "msg-2",
              content: "Good morning Alice! I'm doing well, thanks for asking.",
              sender_id: "user-123",
              sender: %{name: "You", email: "you@example.com"},
              inserted_at: ~N[2024-01-15 09:01:00]
            },
            %{
              id: "msg-3",
              content: "Great! I wanted to discuss the new project timeline.",
              sender_id: "user-456",
              sender: %{name: "Alice Smith", email: "alice@example.com"},
              inserted_at: ~N[2024-01-15 09:02:00]
            },
            %{
              id: "msg-4",
              content: "Sure, I'm all ears. What's the current status?",
              sender_id: "user-789",
              sender: %{name: "Bob Johnson", email: "bob@example.com"},
              inserted_at: ~N[2024-01-15 09:05:00]
            },
            %{
              id: "msg-5",
              content:
                "We're on track to finish by the end of the month. The main features are complete and we're now in testing phase.",
              sender_id: "user-123",
              sender: %{name: "You", email: "you@example.com"},
              inserted_at: ~N[2024-01-15 09:06:00]
            }
          ],
          current_user_id: "user-123",
          loading: false
        }
      },
      %Variation{
        id: :mixed_message_types,
        attributes: %{
          messages: [
            %{
              id: "msg-1",
              content: "Alice Smith joined the room",
              sender_id: "system",
              sender: %{name: "System", email: "system@example.com"},
              inserted_at: ~N[2024-01-15 08:59:00],
              message_type: "system"
            },
            %{
              id: "msg-2",
              content: "Welcome to the team chat!",
              sender_id: "user-456",
              sender: %{name: "Alice Smith", email: "alice@example.com"},
              inserted_at: ~N[2024-01-15 09:00:00]
            },
            %{
              id: "msg-3",
              content: "Thanks! Happy to be here.",
              sender_id: "user-123",
              sender: %{name: "You", email: "you@example.com"},
              inserted_at: ~N[2024-01-15 09:01:00]
            },
            %{
              id: "msg-4",
              content: "Failed to send previous message",
              sender_id: "system",
              sender: %{name: "System", email: "system@example.com"},
              inserted_at: ~N[2024-01-15 09:02:00],
              message_type: "error"
            }
          ],
          current_user_id: "user-123",
          loading: false
        }
      },
      %Variation{
        id: :custom_empty_state,
        attributes: %{
          messages: [],
          current_user_id: "user-123",
          loading: false,
          empty_title: "Welcome to the chat!",
          empty_description: "This is a brand new room. Be the first to say hello!",
          empty_icon: "hand-wave"
        }
      },
      %Variation{
        id: :compact_height,
        attributes: %{
          messages: [
            %{
              id: "msg-1",
              content: "This is a compact messages list",
              sender_id: "user-456",
              sender: %{name: "John Doe", email: "john@example.com"},
              inserted_at: ~N[2024-01-15 10:30:00]
            },
            %{
              id: "msg-2",
              content: "Perfect for smaller interfaces",
              sender_id: "user-123",
              sender: %{name: "You", email: "you@example.com"},
              inserted_at: ~N[2024-01-15 10:31:00]
            }
          ],
          current_user_id: "user-123",
          loading: false,
          max_height: "200px"
        }
      }
    ]
  end
end
