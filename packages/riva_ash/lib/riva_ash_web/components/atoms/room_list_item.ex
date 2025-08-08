alias RivaAshWeb.Components.Atoms, as: Atoms

defmodule RivaAshWeb.Components.Atoms.RoomListItem do
  @moduledoc """
  Room list item atom component.

  Displays a single chat room in a list with name, type, and selection state.
  Supports different room types and visual indicators.
  """

  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Badge
  import RivaAshWeb.Components.Atoms.Text

  @doc """
  Renders a room list item.

  ## Examples

      <.room_list_item 
        name="General" 
        room_type="general"
        is_selected={false}
        unread_count={3}
        on_click="select_room"
        room_id="123"
      />

      <.room_list_item 
        name="Support" 
        room_type="support"
        is_selected={true}
        unread_count={0}
        on_click="select_room"
        room_id="456"
      />
  """
  attr :name, :string, required: true, doc: "Room name"
  attr :room_type, :string, default: "general", doc: "Type of room (general, support, team, reservation)"
  attr :description, :string, default: nil, doc: "Optional room description"
  attr :is_selected, :boolean, default: false, doc: "Whether this room is currently selected"
  attr :unread_count, :integer, default: 0, doc: "Number of unread messages"
  attr :online_count, :integer, default: 0, doc: "Number of users online in this room"
  attr :on_click, :string, required: true, doc: "Phoenix event for room selection"
  attr :room_id, :string, required: true, doc: "Room ID for event handling"
  attr :size, :string, default: "default", doc: "Size variant (sm, default, lg)"
  attr :class, :string, default: "", doc: "Additional CSS classes"
  attr :rest, :global

  def room_list_item(assigns) do
    ~H"""
    <div 
      phx-click={@on_click} 
      phx-value-room_id={@room_id}
      class={[
        "cursor-pointer transition-colors duration-150 border-b border-gray-100 last:border-b-0",
        "hover:bg-gray-50 active:bg-gray-100",
        room_size_classes(@size),
        if(@is_selected, do: selected_classes(), else: ""),
        @class
      ]} 
      {@rest}
    >
      <div class="flex items-center justify-between">
        <div class="flex-1 min-w-0">
          <div class="flex items-center space-x-2">
            <div class={[
              "w-2 h-2 rounded-full shrink-0",
              room_type_color(@room_type)
            ]}></div>
            
            <.text 
              size={text_size(@size)} 
              weight="medium" 
              class={[
                "truncate",
                if(@is_selected, do: "text-blue-900", else: "text-gray-900")
              ]}
            >
              <%= @name %>
            </.text>
          </div>
          
          <%= if @description do %>
            <.text 
              size="xs" 
              class={[
                "mt-1 truncate",
                if(@is_selected, do: "text-blue-700", else: "text-gray-500")
              ]}
            >
              <%= @description %>
            </.text>
          <% end %>
          
          <div class="flex items-center space-x-2 mt-1">
            <.badge variant="secondary" size="xs">
              <%= room_type_label(@room_type) %>
            </.badge>
            
            <%= if @online_count > 0 do %>
              <.text size="xs" class="text-green-600">
                <%= @online_count %> online
              </.text>
            <% end %>
          </div>
        </div>
        
        <%= if @unread_count > 0 do %>
          <.badge variant="primary" size="sm" class="ml-2 shrink-0">
            <%= if @unread_count > 99, do: "99+", else: @unread_count %>
          </.badge>
        <% end %>
      </div>
    </div>
    """
  end

  # Private functions

  defp room_size_classes("sm"), do: "p-2"
  defp room_size_classes("lg"), do: "p-4"
  defp room_size_classes(_), do: "p-3"

  defp text_size("sm"), do: "sm"
  defp text_size("lg"), do: "base"
  defp text_size(_), do: "sm"

  defp selected_classes do
    "bg-blue-50 border-l-4 border-l-blue-500 hover:bg-blue-50"
  end

  defp room_type_color("general"), do: "bg-gray-400"
  defp room_type_color("support"), do: "bg-green-400"
  defp room_type_color("team"), do: "bg-blue-400"
  defp room_type_color("reservation"), do: "bg-purple-400"
  defp room_type_color(_), do: "bg-gray-400"

  defp room_type_label("general"), do: "General"
  defp room_type_label("support"), do: "Support"
  defp room_type_label("team"), do: "Team"
  defp room_type_label("reservation"), do: "Reservations"
  defp room_type_label(type), do: String.capitalize(type)
end
