defmodule Storybook.Templates.CalendarTemplate do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Templates.CalendarTemplate.calendar_template/1

  def doc do
    """
    # CalendarTemplate

    Calendar page template with view controls and filters.

    ## Features

    - Calendar view with day/week/month navigation
    - Filter panel integration
    - Action slots for custom controls
    - Sidebar content area
    - Responsive layout
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          title: "Reservations Calendar",
          description: "View and manage all reservations",
          current_date: "2025-07-29",
          view_mode: "month",
          events: [
            %{id: 1, title: "Meeting with Client", start: "2025-07-29T10:00:00", end: "2025-07-29T11:00:00"},
            %{id: 2, title: "Team Lunch", start: "2025-07-29T12:30:00", end: "2025-07-29T13:30:00"}
          ],
          filters: [
            %{type: "text", label: "Search", key: "search"},
            %{type: "select", label: "Status", key: "status", options: ["Confirmed", "Pending", "Cancelled"]},
            %{type: "date_range", label: "Date Range", key: "date_range"}
          ],
          filter_values: %{},
          on_date_click: "date_clicked",
          on_event_click: "event_clicked",
          on_view_change: "view_changed",
          on_navigate: "navigate"
        },
        slots: [
          """
          <:actions>
            <.button variant="default">
              <.icon name={:plus} class="mr-2 h-4 w-4" />
              New Reservation
            </.button>
          </:actions>
          """,
          """
          <:sidebar_content>
            <div class="p-4">
              <h3 class="font-semibold mb-2">Upcoming Events</h3>
              <ul class="space-y-2">
                <li class="text-sm">• Team Meeting - Today 2:00 PM</li>
                <li class="text-sm">• Client Call - Tomorrow 10:00 AM</li>
              </ul>
            </div>
          </:sidebar_content>
          """
        ]
      },
      %Variation{
        id: :week_view,
        attributes: %{
          title: "Weekly Schedule",
          current_date: "2025-07-29",
          view_mode: "week",
          events: [
            %{id: 1, title: "Project Review", start: "2025-07-29T09:00:00", end: "2025-07-29T10:30:00"},
            %{id: 2, title: "Lunch Break", start: "2025-07-29T12:00:00", end: "2025-07-29T13:00:00"}
          ],
          filters: [],
          on_date_click: "date_clicked",
          on_event_click: "event_clicked",
          on_view_change: "view_changed",
          on_navigate: "navigate"
        }
      },
      %Variation{
        id: :day_view,
        attributes: %{
          title: "Daily View",
          current_date: "2025-07-29",
          view_mode: "day",
          events: [
            %{id: 1, title: "Morning Standup", start: "2025-07-29T09:00:00", end: "2025-07-29T09:30:00"},
            %{id: 2, title: "Code Review", start: "2025-07-29T11:00:00", end: "2025-07-29T12:00:00"}
          ],
          filters: [],
          on_date_click: "date_clicked",
          on_event_click: "event_clicked",
          on_view_change: "view_changed",
          on_navigate: "navigate"
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :without_filters,
        description: "Calendar template without filters",
        attributes: %{
          title: "Simple Calendar",
          current_date: "2025-07-29",
          show_filters: false,
          events: [
            %{id: 1, title: "Event 1", start: "2025-07-29T10:00:00", end: "2025-07-29T11:00:00"}
          ],
          on_date_click: "date_clicked",
          on_event_click: "event_clicked",
          on_view_change: "view_changed",
          on_navigate: "navigate"
        }
      },
      %Example{
        id: :with_custom_class,
        description: "Calendar template with custom CSS classes",
        attributes: %{
          title: "Styled Calendar",
          current_date: "2025-07-29",
          class: "bg-gray-50 rounded-lg p-4",
          events: [],
          filters: [],
          on_date_click: "date_clicked",
          on_event_click: "event_clicked",
          on_view_change: "view_changed",
          on_navigate: "navigate"
        }
      }
    ]
  end
end
