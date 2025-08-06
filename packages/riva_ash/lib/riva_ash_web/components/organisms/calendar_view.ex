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

  @spec calendar_view(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def calendar_view(assigns) do
    # Render calendar view using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:navigation_class, build_navigation_class())
    |> Map.put_new(:tab_navigation_class, build_tab_navigation_class())
    |> Map.put_new(:content_class, build_content_class(assigns.editable))
    |> render_calendar_view_component()
  end

  # Private helper for calendar view rendering
  @spec render_calendar_view_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_calendar_view_component(assigns) do
    ~H"""
    <!-- Calendar view implementation will go here -->
    <div {@rest} class={@container_class}>
      <div class={@navigation_class}>
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
        class={@tab_navigation_class}
      />

      <div class={@content_class}>
        <!-- Calendar grid will go here -->
        <div :for={event <- @events}>
          <%= event.title %>
        </div>
      </div>
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build navigation classes
  @spec build_navigation_class() :: String.t()
  defp build_navigation_class, do: ""

  # Helper function to build tab navigation classes
  @spec build_tab_navigation_class() :: String.t()
  defp build_tab_navigation_class, do: ""

  # Helper function to build content classes
  @spec build_content_class(boolean()) :: String.t()
  defp build_content_class(editable) do
    ""
  end
end
