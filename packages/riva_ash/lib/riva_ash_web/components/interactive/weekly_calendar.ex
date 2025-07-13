defmodule RivaAshWeb.Components.Interactive.WeeklyCalendar do
  @moduledoc """
  Weekly calendar component with time slots.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a weekly calendar view with time slots.
  """
  attr :current_week, :string, required: true
  attr :events, :list, default: []
  attr :time_slots, :list, default: []
  attr :on_slot_click, :string, required: true
  attr :on_event_click, :string, default: nil
  attr :on_navigate, :string, required: true
  attr :start_hour, :integer, default: 8
  attr :end_hour, :integer, default: 18
  attr :slot_duration, :integer, default: 60
  attr :class, :string, default: ""
  attr :rest, :global

  def weekly_calendar(assigns) do
    ~H"""
    <!-- Weekly calendar implementation will go here -->
    <div {@rest}>
      <div>
        <.button phx-click={@on_navigate} phx-value-direction="prev">‹ Previous Week</.button>
        <h2>Week of <%= @current_week %></h2>
        <.button phx-click={@on_navigate} phx-value-direction="next">Next Week ›</.button>
      </div>
      
      <div>
        <!-- Time column -->
        <div>
          <div :for={hour <- @start_hour..@end_hour}>
            <%= hour %>:00
          </div>
        </div>
        
        <!-- Day columns -->
        <div :for={day <- ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]}>
          <div><%= day %></div>
          <div :for={hour <- @start_hour..@end_hour}>
            <button 
              phx-click={@on_slot_click} 
              phx-value-day={day} 
              phx-value-hour={hour}
            >
              <!-- Time slot content -->
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
