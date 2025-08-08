alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms

defmodule RivaAshWeb.Components.Molecules.RoomList do
  import RivaAshWeb.Gettext, only: [dgettext: 2, dgettext: 3]

  @moduledoc """
  Room list molecule component.

  Displays a list of chat rooms with creation functionality and search.
  Handles room selection, filtering, and management actions.
  """

  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.RoomListItem
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Components.Molecules.EmptyState

  @doc """
  Renders a list of chat rooms.

  ## Examples

      <.room_list 
        rooms={@rooms}
        selected_room_id={@current_room_id}
        on_room_select="select_room"
        on_room_create="create_room"
        can_create_rooms={true}
      />

      <.room_list 
        rooms={[]}
        selected_room_id={nil}
        on_room_select="select_room"
        on_room_create="create_room"
        can_create_rooms={false}
        show_search={false}
      />
  """
  attr :rooms, :list, default: [], doc: "List of room structs"
  attr :selected_room_id, :string, default: nil, doc: "ID of currently selected room"
  attr :on_room_select, :string, required: true, doc: "Phoenix event for room selection"
  attr :on_room_create, :string, default: nil, doc: "Phoenix event for room creation"
  attr :can_create_rooms, :boolean, default: false, doc: "Whether user can create new rooms"
  attr :show_search, :boolean, default: true, doc: "Whether to show search functionality"
  attr :search_value, :string, default: "", doc: "Current search filter value"
  attr :on_search, :string, default: "search_rooms", doc: "Phoenix event for search"
  attr :loading, :boolean, default: false, doc: "Whether rooms are loading"
  attr :title, :string, default: nil, doc: "Header title"
  attr :size, :string, default: "default", doc: "Size variant (sm, default, lg)"
  attr :class, :string, default: "", doc: "Additional CSS classes"
  attr :rest, :global

  def room_list(assigns) do
    ~H"""
    <% translated_title = @title || dgettext("ui", "Chat Rooms") %>
    <div class={[
      "flex flex-col bg-gray-50 border-gray-200",
      size_classes(@size),
      @class
    ]} {@rest}>
      <!-- Header -->
      <div class="bg-white border-b border-gray-200 p-4">
        <div class="flex items-center justify-between mb-3">
          <h3 class="font-semibold text-gray-900"><%= translated_title %></h3>
          
          <%= if @can_create_rooms and @on_room_create do %>
            <.button 
              phx-click={@on_room_create}
              variant="secondary" 
              size="sm"
              class="text-xs"
            >
              <svg class="w-3 h-3 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"></path>
              </svg>
              <%= dgettext("ui", "New") %>
            </.button>
          <% end %>
        </div>
        
        <%= if @show_search do %>
          <.input
            type="text"
            value={@search_value}
            phx-change={@on_search}
            placeholder={dgettext("ui", "Search rooms...")}
            size="sm"
            class="w-full"
          />
        <% end %>
      </div>

      <!-- Rooms List -->
      <div class="flex-1 overflow-y-auto">
        <%= if @loading do %>
          <div class="flex items-center justify-center p-8">
            <div class="flex items-center space-x-2 text-gray-500">
              <svg class="w-4 h-4 animate-spin" fill="none" viewBox="0 0 24 24">
                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
              </svg>
              <span class="text-sm"><%= dgettext("ui", "Loading rooms...") %></span>
            </div>
          </div>
        <% else %>
          <%= if Enum.empty?(filtered_rooms(@rooms, @search_value)) do %>
            <div class="p-8">
              <%= if String.trim(@search_value) != "" do %>
                <.empty_state
                  title={dgettext("ui", "No rooms found")}
                  description={dgettext("ui", "No rooms match \"%{query}\"", query: @search_value)}
                  icon="magnifying-glass"
                  size="sm"
                />
              <% else %>
                <.empty_state
                  title={dgettext("ui", "No rooms yet")}
                  description={dgettext("ui", "Create your first room to start chatting")}
                  icon="chat-bubble-left-right"
                  size="sm"
                />
              <% end %>
            </div>
          <% else %>
            <%= for room <- filtered_rooms(@rooms, @search_value) do %>
              <.room_list_item
                name={room.name}
                room_type={room.room_type}
                description={room.description}
                is_selected={room.id == @selected_room_id}
                unread_count={get_unread_count(room)}
                online_count={get_online_count(room)}
                on_click={@on_room_select}
                room_id={room.id}
                size={item_size(@size)}
              />
            <% end %>
          <% end %>
        <% end %>
      </div>
    </div>
    """
  end

  # Private functions

  defp size_classes("sm"), do: "text-sm"
  defp size_classes("lg"), do: "text-base"
  defp size_unmatchedclasses(_unmatched), do: "text-sm"

  defp item_size("sm"), do: "sm"
  defp item_size("lg"), do: "lg"
  defp item_unmatchedsize(_unmatched), do: "default"

  defp filtered_rooms(rooms, search_value) do
    if String.trim(search_value) == "" do
      rooms
    else
      search_term = String.downcase(String.trim(search_value))

      Enum.filter(rooms, fn room ->
        String.contains?(String.downcase(room.name), search_term) or
          (room.description && String.contains?(String.downcase(room.description), search_term))
      end)
    end
  end

  defp get_unread_count(room) do
    # This would be populated from the parent component
    Map.get(room, :unread_count, 0)
  end

  defp get_online_count(room) do
    # This would be populated from the parent component
    Map.get(room, :online_count, 0)
  end
end
