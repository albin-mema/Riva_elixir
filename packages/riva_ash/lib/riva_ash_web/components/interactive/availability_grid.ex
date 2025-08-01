defmodule RivaAshWeb.Components.Interactive.AvailabilityGrid do
  @moduledoc """
  Weekly availability grid editor component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Toggle

  @doc """
  Renders a weekly availability grid editor.
  """
  attr(:availability, :map, required: true)
  attr(:on_slot_toggle, :string, required: true)
  attr(:on_bulk_action, :string, default: nil)
  attr(:time_slots, :list, default: [])
  attr(:start_hour, :integer, default: 8)
  attr(:end_hour, :integer, default: 18)
  attr(:slot_duration, :integer, default: 60)
  attr(:editable, :boolean, default: true)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def availability_grid(assigns) do
    ~H"""
    <!-- Availability grid implementation will go here -->
    <div {@rest}>
      <div :if={@on_bulk_action && @editable}>
        <h3>Bulk Actions</h3>
        <.button phx-click={@on_bulk_action} phx-value-action="select_all">Select All</.button>
        <.button phx-click={@on_bulk_action} phx-value-action="clear_all">Clear All</.button>
        <.button phx-click={@on_bulk_action} phx-value-action="copy_day">Copy Day</.button>
      </div>
      
      <div>
        <!-- Header row with days -->
        <div>
          <div>Time</div>
          <div>Monday</div>
          <div>Tuesday</div>
          <div>Wednesday</div>
          <div>Thursday</div>
          <div>Friday</div>
          <div>Saturday</div>
          <div>Sunday</div>
        </div>
        
        <!-- Time slot rows -->
        <div :for={hour <- @start_hour..@end_hour}>
          <div>
            <span><%= hour %>:00</span>
            
            <div :for={day <- ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]}>
              <.toggle
                :if={@editable}
                checked={get_availability(@availability, day, hour)}
                phx-click={@on_slot_toggle}
                phx-value-day={day}
                phx-value-hour={hour}
              />
              
              <div :if={!@editable} class={[
                "availability-indicator",
                if(get_availability(@availability, day, hour), do: "available", else: "unavailable")
              ]}>
                <%= if get_availability(@availability, day, hour), do: "✓", else: "✗" %>
              </div>
            </div>
          </div>
        </div>
      </div>
      
      <div :if={@editable}>
        <h4>Quick Templates</h4>
        <.button phx-click={@on_bulk_action} phx-value-action="business_hours">Business Hours (9-5)</.button>
        <.button phx-click={@on_bulk_action} phx-value-action="weekdays_only">Weekdays Only</.button>
        <.button phx-click={@on_bulk_action} phx-value-action="weekends_only">Weekends Only</.button>
      </div>
    </div>
    """
  end

  # Helper function to get availability for a specific day/hour
  defp get_availability(availability, day, hour) do
    availability[day][hour] || false
  end
end
