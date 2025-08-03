defmodule Storybook.Organisms.CalendarView do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.CalendarView.calendar_view/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          events: [
            %{title: "Meeting with client", date: "2024-06-15"},
            %{title: "Team lunch", date: "2024-06-18"},
            %{title: "Project deadline", date: "2024-06-20"}
          ],
          current_date: "June 2024",
          view_mode: "month",
          on_date_click: "date_clicked",
          on_event_click: "event_clicked",
          on_view_change: "view_changed",
          on_navigate: "navigate",
          editable: false
        }
      },
      %Variation{
        id: :week_view,
        attributes: %{
          events: [
            %{title: "Team standup", date: "2024-06-17"},
            %{title: "Client presentation", date: "2024-06-19"}
          ],
          current_date: "Week of June 16, 2024",
          view_mode: "week",
          on_date_click: "date_clicked",
          on_event_click: "event_clicked",
          on_view_change: "view_changed",
          on_navigate: "navigate",
          editable: true
        }
      },
      %Variation{
        id: :day_view,
        attributes: %{
          events: [
            %{title: "Morning meeting", date: "2024-06-17 09:00"},
            %{title: "Lunch break", date: "2024-06-17 12:00"},
            %{title: "Code review", date: "2024-06-17 14:00"}
          ],
          current_date: "June 17, 2024",
          view_mode: "day",
          on_date_click: "date_clicked",
          on_event_click: "event_clicked",
          on_view_change: "view_changed",
          on_navigate: "navigate",
          editable: true
        }
      }
    ]
  end
end
