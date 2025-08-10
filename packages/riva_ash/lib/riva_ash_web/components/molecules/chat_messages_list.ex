alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms

defmodule RivaAshWeb.Components.Molecules.ChatMessagesList do
  @moduledoc """
  Chat messages list molecule component.

  Displays a scrollable list of chat messages with auto-scroll and empty states.
  Handles message grouping and optimized rendering for performance.
  """

  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.ChatMessage
  import RivaAshWeb.Components.Molecules.EmptyState

  @doc """
  Renders a list of chat messages.

  ## Examples

      <.chat_messages_list
        messages={@messages}
        current_user_id={@current_user.id}
        loading={@loading}
      />

      <.chat_messages_list
        messages={[]}
        current_user_id={@current_user.id}
        loading={false}
        empty_title="No messages yet"
        empty_description="Start the conversation!"
      />
  """
  attr :messages, :list, default: [], doc: "List of message structs"
  attr :current_user_id, :string, required: true, doc: "Current user ID for message alignment"
  attr :loading, :boolean, default: false, doc: "Whether messages are loading"
  attr :auto_scroll, :boolean, default: true, doc: "Whether to auto-scroll to bottom"
  attr :empty_title, :string, default: "No messages yet", doc: "Title for empty state"

  attr :empty_description, :string,
    default: "Start the conversation by sending a message",
    doc: "Description for empty state"

  attr :empty_icon, :string, default: "chat-bubble-left-right", doc: "Icon for empty state"
  attr :max_height, :string, default: "400px", doc: "Maximum height of the messages container"
  attr :class, :string, default: "", doc: "Additional CSS classes"
  attr :rest, :global

  def chat_messages_list(assigns) do
    ~H"""
    <div
      class={[
        "flex flex-col overflow-hidden",
        @class
      ]}
      style={"max-height: #{@max_height}"}
      {@rest}
    >
      <%= if @loading do %>
        <div class="flex items-center justify-center p-8">
          <div class="flex items-center space-x-2 text-gray-500">
            <svg class="w-5 h-5 animate-spin" fill="none" viewBox="0 0 24 24">
              <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
              <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
            </svg>
            <span class="text-sm">Loading messages...</span>
          </div>
        </div>
      <% else %>
        <%= if Enum.empty?(@messages) do %>
          <div class="flex-1 flex items-center justify-center p-8">
            <.empty_state
              title={@empty_title}
              description={@empty_description}
              icon={@empty_icon}
              size="sm"
            />
          </div>
        <% else %>
          <div
            class="flex-1 overflow-y-auto p-4 space-y-1"
            id="messages-container"
            phx-hook={if @auto_scroll, do: "ChatAutoScroll", else: nil}
          >
            <%= for {message, index} <- Enum.with_index(@messages) do %>
              <.chat_message
                content={message.content}
                sender_name={get_sender_name(message, @current_user_id)}
                sender_avatar={get_sender_avatar(message)}
                timestamp={message.inserted_at}
                is_own_message={message.sender_user_id == @current_user_id}
                message_type={get_message_type(message)}
                id={"message-#{message.id}"}
                class={message_spacing_class(message, @messages, index)}
              />
            <% end %>

            <!-- Scroll anchor for auto-scroll -->
            <%= if @auto_scroll do %>
              <div id="messages-end" phx-hook="ScrollAnchor"></div>
            <% end %>
          </div>
        <% end %>
      <% end %>
    </div>
    """
  end

  # Private functions

  defp get_sender_name(message, current_user_id) do
    cond do
      message.sender_user_id == current_user_id ->
        "You"

      Map.has_key?(message, :sender_user) && message.sender_user && message.sender_user.name ->
        message.sender_user.name

      Map.has_key?(message, :sender_user) && message.sender_user && message.sender_user.email ->
        message.sender_user.email

      Map.has_key?(message, :sender_client) && message.sender_client && message.sender_client.name ->
        message.sender_client.name

      Map.has_key?(message, :sender_client) && message.sender_client && message.sender_client.email ->
        message.sender_client.email

      true ->
        "Unknown"
    end
  end

  defp get_sender_avatar(message) do
    cond do
      Map.has_key?(message, :sender_user) && message.sender_user && Map.get(message.sender_user, :avatar_url) ->
        message.sender_user.avatar_url

      Map.has_key?(message, :sender_client) && message.sender_client && Map.get(message.sender_client, :avatar_url) ->
        message.sender_client.avatar_url

      true ->
        nil
    end
  end

  defp get_message_type(message) do
    Map.get(message, :message_type, "text")
  end

  defp message_spacing_class(current_message, messages, index) do
    previous_message = if index > 0, do: Enum.at(messages, index - 1), else: nil

    cond do
      # First message or different sender
      is_nil(previous_message) or previous_message.sender_user_id != current_message.sender_user_id ->
        "mt-4"

      # Same sender, check time gap
      time_gap_minutes(previous_message.inserted_at, current_message.inserted_at) > 5 ->
        "mt-4"

      # Same sender, close in time
      true ->
        "mt-1"
    end
  end

  defp time_gap_minutes(earlier, later) do
    case {earlier, later} do
      {%NaiveDateTime{} = e, %NaiveDateTime{} = l} ->
        NaiveDateTime.diff(l, e, :second) / 60

      {%DateTime{} = e, %DateTime{} = l} ->
        DateTime.diff(l, e, :second) / 60

      _unmatchedunmatched ->
        0
    end
  end
end
