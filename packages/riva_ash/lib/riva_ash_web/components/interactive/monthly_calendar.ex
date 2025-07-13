defmodule RivaAshWeb.Components.Interactive.MonthlyCalendar do
  @moduledoc """
  Monthly calendar component with reservation display.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a monthly calendar view.
  """
  attr :current_date, :string, required: true
  attr :events, :list, default: []
  attr :on_date_click, :string, required: true
  attr :on_event_click, :string, default: nil
  attr :on_navigate, :string, required: true
  attr :selectable_dates, :list, default: []
  attr :disabled_dates, :list, default: []
  attr :class, :string, default: ""
  attr :rest, :global

  def monthly_calendar(assigns) do
    ~H"""
    <!-- Monthly calendar implementation will go here -->
    <div {@rest}>
      <div>
        <.button phx-click={@on_navigate} phx-value-direction="prev">‹</.button>
        <h2><%= @current_date %></h2>
        <.button phx-click={@on_navigate} phx-value-direction="next">›</.button>
      </div>
      
      <div>
        <!-- Calendar grid -->
        <div>Sun</div>
        <div>Mon</div>
        <div>Tue</div>
        <div>Wed</div>
        <div>Thu</div>
        <div>Fri</div>
        <div>Sat</div>
        
        <!-- Calendar days will be generated here -->
        <div :for={day <- 1..31}>
          <button phx-click={@on_date_click} phx-value-date={day}>
            <%= day %>
          </button>
        </div>
      </div>
    </div>
    """
  end
end
