alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms

defmodule RivaAshWeb.Components.Organisms.ChatInterface do
  @moduledoc """
  Chat interface organism component.

  Complete chat interface combining room list, messages, and input.
  Supports both full-page and widget modes with responsive design.
  """

  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.ChatMessagesList
  import RivaAshWeb.Components.Molecules.RoomList
  import RivaAshWeb.Components.Atoms.ChatInput
  import RivaAshWeb.Components.Molecules.EmptyState

  @doc """
  Renders a complete chat interface.

  ## Examples

      <.chat_interface 
        mode="page"
        rooms={@rooms}
        messages={@messages}
        current_room={@current_room}
        current_user={@current_user}
        message_value={@new_message}
        on_room_select="select_room"
        on_message_send="send_message"
        on_message_change="update_message"
      />

      <.chat_interface 
        mode="widget"
        rooms={@rooms}
        messages={@messages}
        current_room={@current_room}
        current_user={@current_user}
        message_value={@new_message}
        on_room_select="select_room"
        on_message_send="send_message"
        on_message_change="update_message"
        can_create_rooms={false}
      />
  """
  attr :mode, :string, default: "page", doc: "Interface mode (page, widget)"
  attr :rooms, :list, default: [], doc: "List of available rooms"
  attr :messages, :list, default: [], doc: "Messages in current room"
  attr :current_room, :map, default: nil, doc: "Currently selected room"
  attr :current_user, :map, required: true, doc: "Current user struct"
  attr :message_value, :string, default: "", doc: "Current message input value"
  attr :on_room_select, :string, required: true, doc: "Phoenix event for room selection"
  attr :on_message_send, :string, required: true, doc: "Phoenix event for sending message"
  attr :on_message_change, :string, required: true, doc: "Phoenix event for message input change"
  attr :on_room_create, :string, default: nil, doc: "Phoenix event for room creation"
  attr :can_create_rooms, :boolean, default: true, doc: "Whether user can create rooms"
  attr :loading_messages, :boolean, default: false, doc: "Whether messages are loading"
  attr :loading_rooms, :boolean, default: false, doc: "Whether rooms are loading"
  attr :sending_message, :boolean, default: false, doc: "Whether message is being sent"
  attr :online_users, :list, default: [], doc: "List of online users in current room"
  attr :class, :string, default: "", doc: "Additional CSS classes"
  attr :rest, :global

  def chat_interface(assigns) do
    ~H"""
    <div class={[
      "flex",
      interface_classes(@mode),
      @class
    ]} {@rest}>
      <!-- Messages Area (Left Side) -->
      <div class="flex flex-col flex-1 min-w-0">
        <%= if @current_room do %>
          <!-- Chat Header -->
          <div class="bg-white border-b border-gray-200 p-4 shrink-0">
            <div class="flex items-center justify-between">
              <div class="min-w-0 flex-1">
                <h2 class="font-semibold text-gray-900 truncate">
                  <%= @current_room.name %>
                </h2>
                <div class="flex items-center space-x-4 mt-1">
                  <span class="text-sm text-gray-500">
                    <%= room_type_label(@current_room.room_type) %>
                  </span>
                  <%= if length(@online_users) > 0 do %>
                    <span class="text-sm text-green-600">
                      <%= length(@online_users) %> online
                    </span>
                  <% end %>
                </div>
              </div>
              
              <%= if @mode == "widget" do %>
                <button 
                  phx-click="toggle_chat" 
                  class="text-gray-400 hover:text-gray-600 ml-2"
                >
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
                  </svg>
                </button>
              <% end %>
            </div>
          </div>

          <!-- Messages List -->
          <.chat_messages_list
            messages={@messages}
            current_user_id={@current_user.id}
            loading={@loading_messages}
            max_height={messages_height(@mode)}
            class="flex-1"
          />

          <!-- Message Input -->
          <div class="bg-white border-t border-gray-200 p-4 shrink-0">
            <.chat_input
              value={@message_value}
              placeholder="Type your message..."
              on_send={@on_message_send}
              on_change={@on_message_change}
              loading={@sending_message}
              size={input_size(@mode)}
            />
          </div>
        <% else %>
          <!-- No Room Selected -->
          <div class="flex flex-1 items-center justify-center bg-gray-50">
            <.empty_state
              title="Select a room to start chatting"
              description="Choose a room from the sidebar to begin the conversation"
              icon="chat-bubble-left-right"
              size={empty_state_size(@mode)}
            />
          </div>
        <% end %>
      </div>

      <!-- Room List Sidebar (Right Side) -->
      <div class={[
        "bg-gray-50 border-l border-gray-200 shrink-0",
        sidebar_classes(@mode)
      ]}>
        <.room_list
          rooms={@rooms}
          selected_room_id={@current_room && @current_room.id}
          on_room_select={@on_room_select}
          on_room_create={@on_room_create}
          can_create_rooms={@can_create_rooms}
          loading={@loading_rooms}
          size={room_list_size(@mode)}
          class="h-full"
        />
      </div>
    </div>
    """
  end

  # Private functions

  defp interface_classes("widget"), do: "h-96 max-w-md"
  defp interface_classes("page"), do: "h-screen"
  defp interface_unmatchedclasses(_unmatched), do: "h-full"

  defp sidebar_classes("widget"), do: "w-24"
  defp sidebar_classes("page"), do: "w-1/3 max-w-sm"
  defp sidebar_unmatchedclasses(_unmatched), do: "w-1/4"

  defp messages_height("widget"), do: "240px"
  defp messages_height("page"), do: "calc(100vh - 200px)"
  defp messages_unmatchedheight(_unmatched), do: "400px"

  defp input_size("widget"), do: "sm"
  defp input_unmatchedsize(_unmatched), do: "default"

  defp room_list_size("widget"), do: "sm"
  defp room_unmatchedlist_unmatchedsize(_unmatched), do: "default"

  defp empty_state_size("widget"), do: "sm"
  defp empty_unmatchedstate_unmatchedsize(_unmatched), do: "default"

  defp room_type_label("general"), do: "General Discussion"
  defp room_type_label("support"), do: "Customer Support"
  defp room_type_label("team"), do: "Team Chat"
  defp room_type_label("reservation"), do: "Reservations"
  defp room_type_label(type), do: String.capitalize(type)
end
