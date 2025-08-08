alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Organisms.TimelineView do
  @moduledoc """
  Timeline view component for displaying chronological events.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.StatusIndicator

  @doc """
  Renders a timeline view of events.
  """
  attr(:events, :list, required: true)
  attr(:orientation, :string, default: "vertical", values: ~w(horizontal vertical))
  attr(:show_time, :boolean, default: true)
  attr(:show_status, :boolean, default: true)
  attr(:on_event_click, :string, default: nil)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec timeline_view(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def timeline_view(assigns) do
    # Render timeline view using functional composition
    assigns
    |> Map.put_new(:timeline_class, build_timeline_class(assigns.class, assigns.orientation))
    |> Map.put_new(:events_container_class, build_events_container_class(assigns.events))
    |> Map.put_new(:event_item_class, build_event_item_class(assigns.events))
    |> Map.put_new(:event_time_class, build_event_time_class(assigns.show_time))
    |> Map.put_new(:event_status_class, build_event_status_class(assigns.show_status))
    |> Map.put_new(:event_content_class, build_event_content_class(assigns.events))
    |> Map.put_new(:event_title_class, build_event_title_class(assigns.events))
    |> Map.put_new(:event_description_class, build_event_description_class(assigns.events))
    |> render_timeline_view_component()
  end

  # Private helper for timeline view rendering
  @spec render_timeline_view_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_timeline_view_component(assigns) do
    ~H"""
    <div class={@timeline_class} {@rest}>
      <%= for event <- @events do %>
        <div class={@event_item_class}>
          <time class={@event_time_class}>
            <%= event.timestamp %>
          </time>
          <.status_indicator class={@event_status_class} status={event.status} />
          <div class={@event_content_class}>
            <h4 class={@event_title_class}>
              <%= event.title %>
            </h4>
            <p class={@event_description_class}>
              <%= event.description %>
            </p>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  # Helper function to build timeline classes
  @spec build_timeline_class(String.t(), String.t()) :: String.t()
  defp build_timeline_class(class, orientation) do
    base =
      case orientation do
        "horizontal" -> "flex flex-row gap-4 overflow-x-auto"
        "vertical" -> "flex flex-col gap-4"
        _unmatchedunmatched -> "flex flex-col gap-4"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build events container classes
  @spec build_events_container_class(list()) :: String.t()
  defp build_events_container_class(events) do
    if events != [], do: "space-y-4", else: "hidden"
  end

  # Helper function to build event item classes
  @spec build_event_item_class(list()) :: String.t()
  defp build_event_item_class(events) do
    if events != [], do: "flex items-start gap-3 p-4 rounded-lg border", else: "hidden"
  end

  # Helper function to build event time classes
  @spec build_event_time_class(boolean()) :: String.t()
  defp build_event_time_class(show_time) do
    if show_time, do: "text-sm text-muted-foreground", else: "hidden"
  end

  # Helper function to build event status classes
  @spec build_event_status_class(boolean()) :: String.t()
  defp build_event_status_class(show_status) do
    if show_status, do: "flex-shrink-0", else: "hidden"
  end

  # Helper function to build event content classes
  @spec build_event_content_class(list()) :: String.t()
  defp build_event_content_class(events) do
    if events != [], do: "flex-1", else: "hidden"
  end

  # Helper function to build event title classes
  @spec build_event_title_class(list()) :: String.t()
  defp build_event_title_class(events) do
    if events != [], do: "font-semibold text-base", else: "hidden"
  end

  # Helper function to build event description classes
  @spec build_event_description_class(list()) :: String.t()
  defp build_event_description_class(events) do
    if events != [], do: "text-sm text-muted-foreground mt-1", else: "hidden"
  end
end
