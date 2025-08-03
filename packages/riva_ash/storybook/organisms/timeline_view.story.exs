defmodule Storybook.Organisms.TimelineView do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.TimelineView.timeline_view/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          events: [
            %{
              id: "1",
              timestamp: "2024-06-15 10:00:00",
              status: "completed",
              title: "Reservation Created",
              description: "John Doe booked Beach Umbrella for June 20th"
            },
            %{
              id: "2",
              timestamp: "2024-06-15 10:05:00",
              status: "completed",
              title: "Payment Processed",
              description: "Payment of $50.00 received via credit card"
            },
            %{
              id: "3",
              timestamp: "2024-06-15 10:10:00",
              status: "pending",
              title: "Confirmation Email Sent",
              description: "Reservation details sent to john.doe@example.com"
            },
            %{
              id: "4",
              timestamp: "2024-06-20 09:00:00",
              status: "upcoming",
              title: "Reservation Start",
              description: "Beach Umbrella will be available for pickup"
            }
          ],
          orientation: "vertical",
          show_time: true,
          show_status: true,
          on_event_click: "event_clicked"
        }
      },
      %Variation{
        id: :horizontal,
        attributes: %{
          events: [
            %{
              id: "1",
              timestamp: "2024-06-15 10:00:00",
              status: "completed",
              title: "Created",
              description: "Reservation created"
            },
            %{
              id: "2",
              timestamp: "2024-06-15 10:05:00",
              status: "completed",
              title: "Paid",
              description: "Payment processed"
            },
            %{
              id: "3",
              timestamp: "2024-06-15 10:10:00",
              status: "pending",
              title: "Confirmed",
              description: "Email sent"
            },
            %{
              id: "4",
              timestamp: "2024-06-20 09:00:00",
              status: "upcoming",
              title: "Start",
              description: "Reservation begins"
            }
          ],
          orientation: "horizontal",
          show_time: true,
          show_status: true,
          on_event_click: "event_clicked"
        }
      },
      %Variation{
        id: :without_status,
        attributes: %{
          events: [
            %{
              id: "1",
              timestamp: "2024-06-15 10:00:00",
              title: "Reservation Created",
              description: "John Doe booked Beach Umbrella for June 20th"
            },
            %{
              id: "2",
              timestamp: "2024-06-15 10:05:00",
              title: "Payment Processed",
              description: "Payment of $50.00 received via credit card"
            },
            %{
              id: "3",
              timestamp: "2024-06-15 10:10:00",
              title: "Confirmation Email Sent",
              description: "Reservation details sent to john.doe@example.com"
            }
          ],
          orientation: "vertical",
          show_time: true,
          show_status: false,
          on_event_click: "event_clicked"
        }
      }
    ]
  end
end
