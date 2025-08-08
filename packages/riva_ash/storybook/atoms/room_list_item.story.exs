defmodule Storybook.Atoms.RoomListItem do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.RoomListItem.room_list_item/1

  def doc do
    """
    # Room List Item (Atom)

    A room list item component that displays chat room information with selection states and indicators.
    Shows room name, type, description, and various status indicators like unread messages and online users.

    ## Features

    - **Room types**: Visual indicators for different room types (general, support, team, reservation)
    - **Selection state**: Highlighted appearance when room is selected
    - **Unread counter**: Badge showing number of unread messages
    - **Online indicator**: Shows number of users currently online
    - **Hover effects**: Interactive feedback for better UX
    - **Responsive design**: Adapts to different container sizes

    ## Usage

    ```heex
    <.room_list_item 
      name="General" 
      room_type="general"
      is_selected={false}
      unread_count={3}
      on_click="select_room"
      room_id="123"
    />
    ```
    """
  end

  def variations do
    [
      %Variation{
        id: :general_room,
        attributes: %{
          name: "General",
          room_type: "general",
          description: "General discussion for all team members",
          is_selected: false,
          unread_count: 0,
          online_count: 5,
          on_click: "select_room",
          room_id: "general-123"
        }
      },
      %Variation{
        id: :support_room,
        attributes: %{
          name: "Support",
          room_type: "support",
          description: "Customer support and help desk",
          is_selected: false,
          unread_count: 3,
          online_count: 2,
          on_click: "select_room",
          room_id: "support-456"
        }
      },
      %Variation{
        id: :team_room_selected,
        attributes: %{
          name: "Team",
          room_type: "team",
          description: "Internal team coordination",
          is_selected: true,
          unread_count: 0,
          online_count: 8,
          on_click: "select_room",
          room_id: "team-789"
        }
      },
      %Variation{
        id: :reservation_room,
        attributes: %{
          name: "Reservations",
          room_type: "reservation",
          description: "Booking discussions and updates",
          is_selected: false,
          unread_count: 12,
          online_count: 3,
          on_click: "select_room",
          room_id: "reservation-101"
        }
      },
      %Variation{
        id: :high_unread_count,
        attributes: %{
          name: "Busy Room",
          room_type: "general",
          description: "Very active discussion room",
          is_selected: false,
          unread_count: 150,
          online_count: 25,
          on_click: "select_room",
          room_id: "busy-202"
        }
      },
      %Variation{
        id: :no_description,
        attributes: %{
          name: "Quick Chat",
          room_type: "general",
          is_selected: false,
          unread_count: 1,
          online_count: 0,
          on_click: "select_room",
          room_id: "quick-303"
        }
      },
      %Variation{
        id: :small_size,
        attributes: %{
          name: "Compact",
          room_type: "team",
          description: "Small size variant",
          is_selected: false,
          unread_count: 2,
          online_count: 4,
          on_click: "select_room",
          room_id: "compact-404",
          size: "sm"
        }
      },
      %Variation{
        id: :large_size,
        attributes: %{
          name: "Spacious Room",
          room_type: "support",
          description: "Large size variant with more padding",
          is_selected: false,
          unread_count: 5,
          online_count: 12,
          on_click: "select_room",
          room_id: "spacious-505",
          size: "lg"
        }
      },
      %Variation{
        id: :room_list_example,
        template: """
        <div class="max-w-sm mx-auto bg-white border border-gray-200 rounded-lg overflow-hidden">
          <div class="bg-gray-50 px-4 py-2 border-b border-gray-200">
            <h3 class="font-semibold text-gray-900">Chat Rooms</h3>
          </div>
          <div class="divide-y divide-gray-100">
            <.room_list_item 
              name="General"
              room_type="general"
              description="General discussion"
              is_selected={true}
              unread_count={0}
              online_count={8}
              on_click="select_room"
              room_id="general"
            />
            <.room_list_item 
              name="Support"
              room_type="support"
              description="Customer support"
              is_selected={false}
              unread_count={5}
              online_count={3}
              on_click="select_room"
              room_id="support"
            />
            <.room_list_item 
              name="Team"
              room_type="team"
              description="Internal coordination"
              is_selected={false}
              unread_count={0}
              online_count={12}
              on_click="select_room"
              room_id="team"
            />
            <.room_list_item 
              name="Reservations"
              room_type="reservation"
              description="Booking discussions"
              is_selected={false}
              unread_count={23}
              online_count={5}
              on_click="select_room"
              room_id="reservations"
            />
          </div>
        </div>
        """
      }
    ]
  end
end
