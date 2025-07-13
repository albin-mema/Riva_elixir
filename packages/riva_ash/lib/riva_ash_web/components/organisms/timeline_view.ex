defmodule RivaAshWeb.Components.Organisms.TimelineView do
  @moduledoc """
  Timeline view component for displaying chronological events.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.StatusIndicator

  @doc """
  Renders a timeline view of events.
  """
  attr :events, :list, required: true
  attr :orientation, :string, default: "vertical", values: ~w(horizontal vertical)
  attr :show_time, :boolean, default: true
  attr :show_status, :boolean, default: true
  attr :on_event_click, :string, default: nil
  attr :class, :string, default: ""
  attr :rest, :global

  def timeline_view(assigns) do
    ~H"""
    <!-- Timeline view implementation will go here -->
    <div {@rest}>
      <div :for={event <- @events}>
        <div>
          <time :if={@show_time}><%= event.timestamp %></time>
          <.status_indicator :if={@show_status} status={event.status} />
          <div>
            <h4><%= event.title %></h4>
            <p><%= event.description %></p>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
