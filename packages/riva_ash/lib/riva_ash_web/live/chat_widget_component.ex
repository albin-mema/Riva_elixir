alias RivaAsh.Resources, as: Resources
alias RivaAshWeb.Components.Organisms, as: Organisms
alias Phoenix.PubSub, as: PubSub

defmodule RivaAshWeb.ChatWidgetComponent do
  @moduledoc """
  Chat widget component that can be embedded in any page as a sidebar.
  """

  use RivaAshWeb, :live_component

  alias RivaAsh.Resources.{ChatMessage, ChatRoom}
  alias RivaAsh.ErrorHelpers

  import RivaAshWeb.Components.Organisms.ChatInterface

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:expanded, false)
     |> assign(:rooms, [])
     |> assign(:current_room, nil)
     |> assign(:messages, [])
     |> assign(:new_message, "")
     |> assign(:unread_count, 0)}
  end

  @impl true
  def update(%{current_user: current_user} = assigns, socket) do
    socket = assign(socket, assigns)

    if connected?(socket) do
      case load_widget_data(current_user) do
        {:ok, %{rooms: rooms, current_room: current_room, messages: messages}} ->
          # Subscribe to the current room if exists
          if current_room do
            Phoenix.PubSub.subscribe(RivaAsh.PubSub, "chat:room:#{current_room.id}")
          end

          {:ok,
           socket
           |> assign(:rooms, rooms)
           |> assign(:current_room, current_room)
           |> assign(:messages, messages)}

        {:error, _reason} ->
          {:ok, socket}
      end
    else
      {:ok, socket}
    end
  end

  @impl true
  def handle_event("toggle_chat", _params, socket) do
    {:noreply, assign(socket, :expanded, !socket.assigns.expanded)}
  end

  def handle_event("select_room", %{"room_id" => room_id}, socket) do
    case load_room_data(room_id, socket.assigns.current_user) do
      {:ok, %{room: room, messages: messages}} ->
        # Unsubscribe from previous room
        if socket.assigns.current_room do
          Phoenix.PubSub.unsubscribe(RivaAsh.PubSub, "chat:room:#{socket.assigns.current_room.id}")
        end

        # Subscribe to new room
        Phoenix.PubSub.subscribe(RivaAsh.PubSub, "chat:room:#{room_id}")

        {:noreply, assign(socket, current_room: room, messages: messages)}

      {:error, _reason} ->
        {:noreply, socket}
    end
  end

  def handle_event("send_message", %{"message" => content}, socket) when content != "" do
    if socket.assigns.current_room do
      message_params = %{
        content: String.trim(content),
        room_id: socket.assigns.current_room.id
      }

      case Ash.create(ChatMessage, :create, message_params, actor: socket.assigns.current_user, domain: RivaAsh.Domain) do
        {:ok, message} ->
          # Load the message with sender info
          message_with_sender = Ash.load!(message, [:sender], domain: RivaAsh.Domain)

          # Broadcast to all users in this room
          Phoenix.PubSub.broadcast(RivaAsh.PubSub, "chat:room:#{socket.assigns.current_room.id}", {
            :new_message,
            message_with_sender
          })

          {:noreply, assign(socket, :new_message, "")}

        {:error, _error} ->
          {:noreply, socket}
      end
    else
      {:noreply, socket}
    end
  end

  def handle_event("send_message", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("update_message", %{"value" => content}, socket) do
    {:noreply, assign(socket, :new_message, content)}
  end

  @impl true
  def handle_info({:new_message, message}, socket) do
    # Only add message if it's for the current room
    if socket.assigns.current_room && message.room_id == socket.assigns.current_room.id do
      updated_messages = socket.assigns.messages ++ [message]
      {:noreply, assign(socket, :messages, updated_messages)}
    else
      # Update unread count for other rooms
      {:noreply, assign(socket, :unread_count, socket.assigns.unread_count + 1)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="right-4 bottom-4 z-50 fixed">
      <%= if @expanded do %>
        <!-- Expanded Chat Widget -->
        <div class="flex flex-col bg-white shadow-xl border border-gray-200 rounded-lg w-80 h-96">
          <!-- Header -->
          <div class="flex justify-between items-center bg-blue-500 p-3 rounded-t-lg text-white">
            <h3 class="font-semibold">💬 Chat</h3>
            <button phx-click="toggle_chat" phx-target={@myself} class="text-white hover:text-gray-200">
              ✕
            </button>
          </div>

          <!-- Chat Content -->
          <div class="flex flex-1 min-h-0">
            <!-- Messages Area -->
            <div class="flex flex-col flex-1">
              <%= if @current_room do %>
                <!-- Room Header -->
                <div class="bg-gray-50 p-2 border-gray-200 border-b">
                  <div class="font-medium text-gray-900 text-sm"><%= @current_room.name %></div>
                </div>

                <!-- Messages -->
                <div class="flex-1 space-y-2 p-2 overflow-y-auto text-sm">
                  <%= for message <- @messages do %>
                    <div class={[
                      "flex",
                      if(message.sender_id == @current_user.id, do: "justify-end", else: "justify-start")
                    ]}>
                      <div class={[
                        "max-w-xs px-2 py-1 rounded text-xs",
                        if(message.sender_id == @current_user.id,
                           do: "bg-blue-500 text-white",
                           else: "bg-gray-200 text-gray-900")
                      ]}>
                        <%= if message.sender_id != @current_user.id do %>
                          <div class="mb-1 font-semibold">
                            <%= message.sender.name || message.sender.email %>
                          </div>
                        <% end %>
                        <div><%= message.content %></div>
                      </div>
                    </div>
                  <% end %>
                </div>

                <!-- Message Input -->
                <div class="p-2 border-gray-200 border-t">
                  <form phx-submit="send_message" phx-target={@myself} class="flex space-x-1">
                    <input
                      type="text"
                      name="message"
                      value={@new_message}
                      phx-change="update_message"
                      phx-target={@myself}
                      placeholder="Type message..."
                      class="flex-1 px-2 py-1 border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-blue-500 text-xs"
                      autocomplete="off"
                    />
                    <button type="submit" class="bg-blue-500 hover:bg-blue-600 px-2 py-1 rounded text-white text-xs">
                      Send
                    </button>
                  </form>
                </div>
              <% else %>
                <div class="flex flex-1 justify-center items-center text-gray-500 text-sm">
                  Select a room to chat
                </div>
              <% end %>
            </div>

            <!-- Rooms Sidebar -->
            <div class="flex flex-col bg-gray-50 border-gray-200 border-l w-24">
              <div class="p-2 border-gray-200 border-b font-semibold text-gray-700 text-xs">
                Rooms
              </div>
              <div class="flex-1 overflow-y-auto">
                <%= for room <- @rooms do %>
                  <div
                    phx-click="select_room"
                    phx-value-room_id={room.id}
                    phx-target={@myself}
                    class={[
                      "p-2 border-b border-gray-100 cursor-pointer hover:bg-gray-100 text-xs",
                      if(@current_room && @current_room.id == room.id, do: "bg-blue-50 border-l-2 border-l-blue-500", else: "")
                    ]}
                  >
                    <div class="font-medium text-gray-900 truncate"><%= room.name %></div>
                  </div>
                <% end %>
              </div>
            </div>
          </div>
        </div>
      <% else %>
        <!-- Collapsed Chat Button -->
        <button
          phx-click="toggle_chat"
          phx-target={@myself}
          class="bg-blue-500 hover:bg-blue-600 shadow-lg p-3 rounded-full text-white transition-colors"
        >
          💬
          <%= if @unread_count > 0 do %>
            <span class="-top-1 -right-1 absolute flex justify-center items-center bg-red-500 rounded-full w-5 h-5 text-white text-xs">
              <%= @unread_count %>
            </span>
          <% end %>
        </button>
      <% end %>
    </div>
    """
  end

  # Private functions

  defp load_widget_data(user) do
    with {:ok, rooms} <- load_rooms(user),
         current_room <- List.first(rooms),
         {:ok, messages} <- load_room_messages(current_room, user) do
      {:ok, %{rooms: rooms, current_room: current_room, messages: messages}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp load_rooms(_user) do
    case Ash.read(ChatRoom, :active_rooms, %{}, domain: RivaAsh.Domain) do
      # Limit to 5 rooms for widget
      {:ok, rooms} -> {:ok, Enum.take(rooms, 5)}
      {:error, error} -> {:error, error}
    end
  end

  defp load_room_data(room_id, user) do
    with {:ok, room} <- Ash.get(ChatRoom, room_id, domain: RivaAsh.Domain),
         {:ok, messages} <- load_room_messages(room, user) do
      {:ok, %{room: room, messages: messages}}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp load_room_messages(nil, _user), do: {:ok, []}

  defp load_room_messages(room, user) do
    case Ash.read(ChatMessage, :for_room, %{room_id: room.id}, actor: user, domain: RivaAsh.Domain) do
      # Last 20 messages
      {:ok, messages} -> {:ok, Enum.take(messages, -20)}
      {:error, error} -> {:error, error}
    end
  end
end
