alias RivaAsh.Resources, as: Resources
alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Live, as: Live
alias Phoenix.PubSub, as: PubSub

defmodule RivaAshWeb.ChatLive do
  @moduledoc """
  Multi-room chat interface with sidebar and full-page modes.
  """

  use RivaAshWeb, :live_view

  alias RivaAsh.Resources.{ChatMessage, ChatRoom}
  alias RivaAshWeb.Presence
  alias RivaAsh.ErrorHelpers

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.ChatInterface
  import RivaAshWeb.Live.AuthHelpers

  @impl true
  def mount(params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        # "page" or "sidebar"
        mode = Map.get(params, "mode", "page")

        if connected?(socket) do
          # Subscribe to general chat updates (kept minimal for dev)
          Phoenix.PubSub.subscribe(RivaAsh.PubSub, "chat:general")
        end

        case load_initial_data(user) do
          {:ok, %{rooms: rooms, current_room: current_room, messages: messages}} ->
            socket =
              socket
              |> assign(:current_user, user)
              |> assign(:mode, mode)
              |> assign(:rooms, rooms)
              |> assign(:current_room, current_room)
              |> assign(:messages, messages)
              |> assign(:new_message, "")
              |> assign(:online_users, [])
              |> assign(:loading, false)
              |> assign(:show_new_room_form, false)
              |> assign(:new_room_name, "")

            {:ok, socket}

          {:error, reason} ->
            {:ok,
             socket
             |> put_flash(:error, "Failed to load chat: #{inspect(reason)}")
             |> redirect(to: "/dashboard")}
        end

      {:error, _unmatched} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def handle_event("send_message", %{"message" => content}, socket) when content != "" do
    if socket.assigns.current_room do
      message_params = %{
        content: String.trim(content),
        room_id: socket.assigns.current_room.id
      }

      case Ash.create(ChatMessage, :create, message_params, actor: socket.assigns.current_user, domain: RivaAsh.Domain) do
        {:ok, message} ->
          # Load message with sender info (user/client), simplified for dev
          message_with_sender = Ash.load!(message, [:sender_user, :sender_client], domain: RivaAsh.Domain)

          # Broadcast to all users in this room
          Phoenix.PubSub.broadcast(RivaAsh.PubSub, "chat:room:#{socket.assigns.current_room.id}", {
            :new_message,
            message_with_sender
          })

          {:noreply, assign(socket, :new_message, "")}

        {:error, error} ->
          error_message = ErrorHelpers.format_error(error)
          {:noreply, put_flash(socket, :error, "Failed to send message: #{error_message}")}
      end
    else
      {:noreply, put_flash(socket, :error, "No room selected")}
    end
  end

  def handle_event("send_message", _params, socket) do
    {:noreply, put_flash(socket, :error, "Message cannot be empty")}
  end

  def handle_event("update_message", %{"value" => content}, socket) do
    {:noreply, assign(socket, :new_message, content)}
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
        {:noreply, put_flash(socket, :error, "Failed to load room")}
    end
  end

  def handle_event("toggle_new_room_form", _params, socket) do
    {:noreply, assign(socket, :show_new_room_form, !socket.assigns.show_new_room_form)}
  end

  def handle_event("update_new_room_name", %{"value" => name}, socket) do
    {:noreply, assign(socket, :new_room_name, name)}
  end

  def handle_event("create_room", %{"name" => name}, socket) when name != "" do
    # For now (dev), create internal rooms for the user's first business
    room_params =
      case get_user_default_business(socket.assigns.current_user) do
        {:ok, business_id} ->
          %{
            name: String.trim(name),
            room_type: "internal",
            business_id: business_id
          }

        {:error, _} ->
          %{
            name: String.trim(name),
            room_type: "internal",
            # Fallback: let the create fail with a clear error
            business_id: nil
          }
      end

    case Ash.create(ChatRoom, :create, room_params, actor: socket.assigns.current_user, domain: RivaAsh.Domain) do
      {:ok, room} ->
        updated_rooms = [room | socket.assigns.rooms]

        {:noreply,
         socket
         |> assign(:rooms, updated_rooms)
         |> assign(:show_new_room_form, false)
         |> assign(:new_room_name, "")
         |> put_flash(:info, "Room '#{room.name}' created successfully")}

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)
        {:noreply, put_flash(socket, :error, "Failed to create room: #{error_message}")}
    end
  end

  def handle_event("create_room", _params, socket) do
    {:noreply, put_flash(socket, :error, "Room name cannot be empty")}
  end

  @impl true
  def handle_info({:new_message, message}, socket) do
    # Only add message if it's for the current room
    if socket.assigns.current_room && message.room_id == socket.assigns.current_room.id do
      updated_messages = socket.assigns.messages ++ [message]
      {:noreply, assign(socket, :messages, updated_messages)}
    else
      {:noreply, socket}
    end
  end

  def handle_info(%{event: "presence_diff", payload: _diff}, socket) do
    if socket.assigns.current_room do
      online_users = Presence.list_users("chat_presence:#{socket.assigns.current_room.id}")
      {:noreply, assign(socket, :online_users, online_users)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.chat_interface
      mode={@mode}
      rooms={@rooms}
      messages={@messages}
      current_room={@current_room}
      current_user={@current_user}
      message_value={@new_message}
      on_room_select="select_room"
      on_message_send="send_message"
      on_message_change="update_message"
      on_room_create="toggle_new_room_form"
      can_create_rooms={true}
      loading_messages={@loading}
      sending_message={false}
      online_users={@online_users}
    />

    <!-- Room Creation Modal (if needed) -->
    <%= if @show_new_room_form do %>
      <div class="z-50 fixed inset-0 flex justify-center items-center bg-black bg-opacity-50">
        <div class="bg-white mx-4 p-6 rounded-lg w-full max-w-md">
          <h3 class="mb-4 font-semibold text-lg">Create New Room</h3>
          <form phx-submit="create_room" class="space-y-4">
            <div>
              <label class="block mb-1 font-medium text-gray-700 text-sm">Room Name</label>
              <input
                type="text"
                name="name"
                value={@new_room_name}
                phx-change="update_new_room_name"
                placeholder="Enter room name..."
                class="px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
                autocomplete="off"
                required
              />
            </div>
            <div class="flex justify-end space-x-2">
              <button
                type="button"
                phx-click="toggle_new_room_form"
                class="px-4 py-2 text-gray-600 hover:text-gray-800"
              >
                Cancel
              </button>
              <button
                type="submit"
                class="bg-blue-500 hover:bg-blue-600 px-4 py-2 rounded-lg text-white"
              >
                Create Room
              </button>
            </div>
          </form>
        </div>
      </div>
    <% end %>
    """
  end

  # Private functions

  defp load_initial_data(user) do
    with {:ok, rooms} <- load_rooms(user),
         current_room <- List.first(rooms),
         {:ok, messages} <- load_room_messages(current_room, user) do
      {:ok, %{rooms: rooms, current_room: current_room, messages: messages}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp load_rooms(_user) do
    # For now (dev), load all active rooms
    case Ash.read(ChatRoom, :active_rooms, %{}, domain: RivaAsh.Domain) do
      {:ok, rooms} -> {:ok, rooms}
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

  # Dev helper: get a default business for a user
  defp get_user_default_business(_user) do
    # TODO: replace with real query for user's businesses; for now, use first active room's business
    case Ash.read(RivaAsh.Resources.ChatRoom, :active_rooms, %{}, domain: RivaAsh.Domain) do
      {:ok, [first | _rest]} -> {:ok, first.business_id}
      _ -> {:error, :no_business}
    end
  end

  defp load_room_messages(nil, _user), do: {:ok, []}

  defp load_room_messages(room, user) do
    case Ash.read(ChatMessage, :for_room, %{room_id: room.id}, actor: user, domain: RivaAsh.Domain) do
      {:ok, messages} -> {:ok, messages}
      {:error, error} -> {:error, error}
    end
  end
end
