defmodule RivaAshWeb.Components.Interactive.DailySchedule do
  @moduledoc """
  Daily schedule component with hourly time slots.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a daily schedule view.
  """
  attr(:current_date, :string, required: true)
  attr(:events, :list, default: [])
  attr(:on_slot_click, :string, required: true)
  attr(:on_event_click, :string, default: nil)
  attr(:on_navigate, :string, required: true)
  attr(:start_hour, :integer, default: 8)
  attr(:end_hour, :integer, default: 18)
  attr(:slot_duration, :integer, default: 30)
  attr(:show_all_day, :boolean, default: true)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def daily_schedule(assigns) do
    ~H"""
    <!-- Daily schedule implementation will go here -->
    <div {@rest}>
      <div>
        <.button phx-click={@on_navigate} phx-value-direction="prev">‹ Previous Day</.button>
        <h2><%= @current_date %></h2>
        <.button phx-click={@on_navigate} phx-value-direction="next">Next Day ›</.button>
      </div>
      
      <div :if={@show_all_day}>
        <!-- All day events section -->
        <div>
          <h3>All Day</h3>
          <div :for={event <- Enum.filter(@events, & &1.all_day)}>
            <button phx-click={@on_event_click} phx-value-event={event.id}>
              <%= event.title %>
            </button>
          </div>
        </div>
      </div>
      
      <div>
        <!-- Hourly time slots -->
        <div :for={hour <- @start_hour..@end_hour}>
          <div>
            <span><%= hour %>:00</span>
            <div>
              <button 
                phx-click={@on_slot_click} 
                phx-value-hour={hour}
                phx-value-minute="0"
              >
                <!-- First half hour -->
              </button>
              <button 
                phx-click={@on_slot_click} 
                phx-value-hour={hour}
                phx-value-minute="30"
              >
                <!-- Second half hour -->
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
