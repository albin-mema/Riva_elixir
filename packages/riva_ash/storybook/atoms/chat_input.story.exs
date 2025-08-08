defmodule Storybook.Atoms.ChatInput do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.ChatInput.chat_input/1

  def doc do
    """
    # Chat Input (Atom)

    A specialized input component for chat messages with integrated send button and character counter.
    Supports keyboard shortcuts, loading states, and input validation.

    ## Features

    - **Integrated send button**: Submit messages with button click or Enter key
    - **Character counter**: Shows remaining characters with visual feedback
    - **Loading states**: Displays spinner when sending messages
    - **Input validation**: Prevents sending empty messages
    - **Keyboard shortcuts**: Enter to send, Shift+Enter for new line
    - **Responsive design**: Adapts to different container sizes

    ## Usage

    ```heex
    <.chat_input 
      value={@message} 
      placeholder="Type your message..."
      on_send="send_message"
      on_change="update_message"
    />
    ```
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          value: "",
          placeholder: "Type your message...",
          on_send: "send_message",
          on_change: "update_message"
        }
      },
      %Variation{
        id: :with_text,
        attributes: %{
          value: "Hello everyone!",
          placeholder: "Type your message...",
          on_send: "send_message",
          on_change: "update_message"
        }
      },
      %Variation{
        id: :loading,
        attributes: %{
          value: "Sending this message...",
          placeholder: "Type your message...",
          on_send: "send_message",
          on_change: "update_message",
          loading: true
        }
      },
      %Variation{
        id: :disabled,
        attributes: %{
          value: "",
          placeholder: "Chat is disabled...",
          on_send: "send_message",
          on_change: "update_message",
          disabled: true
        }
      },
      %Variation{
        id: :small_size,
        attributes: %{
          value: "",
          placeholder: "Type message...",
          on_send: "send_message",
          on_change: "update_message",
          size: "sm"
        }
      },
      %Variation{
        id: :large_size,
        attributes: %{
          value: "",
          placeholder: "Type your message...",
          on_send: "send_message",
          on_change: "update_message",
          size: "lg"
        }
      },
      %Variation{
        id: :character_limit,
        attributes: %{
          value: "This message is getting close to the character limit and will show visual feedback",
          placeholder: "Type your message...",
          on_send: "send_message",
          on_change: "update_message",
          max_length: 100
        }
      },
      %Variation{
        id: :near_limit,
        attributes: %{
          value: "This message is very close to the character limit and shows warning colors in the counter",
          placeholder: "Type your message...",
          on_send: "send_message",
          on_change: "update_message",
          max_length: 120
        }
      },
      %Variation{
        id: :different_states,
        template: """
        <div class="space-y-4 max-w-lg mx-auto p-4">
          <div>
            <h4 class="text-sm font-medium text-gray-700 mb-2">Default State</h4>
            <.chat_input 
              value=""
              placeholder="Type your message..."
              on_send="send_message"
              on_change="update_message"
            />
          </div>
          
          <div>
            <h4 class="text-sm font-medium text-gray-700 mb-2">With Text</h4>
            <.chat_input 
              value="Ready to send!"
              placeholder="Type your message..."
              on_send="send_message"
              on_change="update_message"
            />
          </div>
          
          <div>
            <h4 class="text-sm font-medium text-gray-700 mb-2">Loading</h4>
            <.chat_input 
              value="Sending..."
              placeholder="Type your message..."
              on_send="send_message"
              on_change="update_message"
              loading={true}
            />
          </div>
          
          <div>
            <h4 class="text-sm font-medium text-gray-700 mb-2">Disabled</h4>
            <.chat_input 
              value=""
              placeholder="Chat is disabled"
              on_send="send_message"
              on_change="update_message"
              disabled={true}
            />
          </div>
        </div>
        """
      }
    ]
  end
end
