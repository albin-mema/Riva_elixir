defmodule RivaAshWeb.Components.Organisms.CalendarView do
  @moduledoc """
  Calendar view component for displaying reservations and events.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Molecules.TabNavigation

  @doc """
  Renders a calendar view with different display modes.
  """
  attr(:events, :list, default: [])
  attr(:current_date, :string, required: true)
  attr(:view_mode, :string, default: "month", values: ~w(day week month))
  attr(:on_date_click, :string, required: true)
  attr(:on_event_click, :string, default: nil)
  attr(:on_view_change, :string, required: true)
  attr(:on_navigate, :string, required: true)
  attr(:editable, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def calendar_view(assigns) do
    ~H"""
    <!-- Calendar view implementation will go here -->
    <div {@rest}>
      <div>
        <.button phx-click={@on_navigate} phx-value-direction="prev">Previous</.button>
        <span><%= @current_date %></span>
        <.button phx-click={@on_navigate} phx-value-direction="next">Next</.button>
      </div>
      
      <.tab_navigation
        tabs={[
          %{id: "day", label: "Day"},
          %{id: "week", label: "Week"},
          %{id: "month", label: "Month"}
        ]}
        active_tab={@view_mode}
        on_tab_change={@on_view_change}
      />
      
      <div>
        <!-- Calendar grid will go here -->
        <div :for={event <- @events}>
          <%= event.title %>
        </div>
      </div>
    </div>
    """
  end
end
