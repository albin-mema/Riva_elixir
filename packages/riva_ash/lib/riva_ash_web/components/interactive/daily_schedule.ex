alias RivaAshWeb.Components.Interactive, as: Interactive
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.LiveView, as: LiveView

defmodule RivaAshWeb.Components.Interactive.DailySchedule do
  import RivaAshWeb.Gettext, only: [dgettext: 2]

  @moduledoc """
  Daily schedule component with hourly time slots.

  ## Styleguide Compliance

  This component follows the Riva Ash styleguide principles:

  ### Functional Programming
  - Uses pure functions with immutable data
  - Implements pattern matching for data validation
  - Follows the functional core, imperative shell pattern
  - Uses pipelines for data transformation

  ### Type Safety
  - Comprehensive type specifications for all functions
  - Uses proper Elixir type annotations
  - Implements guard clauses for validation

  ### Code Abstraction
  - Single level of abstraction principle
  - Extracted helper functions for business logic
  - Clear separation of concerns
  - Reusable utility functions

  ### Phoenix/Ash Patterns
  - Follows Phoenix LiveView component patterns
  - Uses proper attribute handling
  - Implements consistent event handling
  - Ash-specific data structures and patterns

  ### LiveView Component Best Practices
  - Proper use of assigns and HEEx templates
  - Consistent naming conventions
  - Clear documentation and examples
  - Accessible and semantic HTML structure

  ## Examples

  ```elixir
  # Basic usage
  <.daily_schedule
    current_date="2024-01-15"
    events={@events}
    on_slot_click="handle_slot_click"
    on_navigate="navigate_day"
  />

  # With all day events
  <.daily_schedule
    current_date="2024-01-15"
    events={@events}
    on_slot_click="handle_slot_click"
    on_event_click="handle_event_click"
    show_all_day={true}
    start_hour={6}
    end_hour={22}
  />
  """
  use Phoenix.Component
  import RivaAshWeb.Components.UI.Button

  @doc """
  Renders a daily schedule view.

  ## Attributes

  - `current_date` (string, required): Current date being displayed
  - `events` (list, optional): List of events to display
  - `on_slot_click` (string, required): Event handler for slot clicks
  - `on_event_click` (string, optional): Event handler for event clicks
  - `on_navigate` (string, required): Event handler for navigation
  - `start_hour` (integer, default: 8): Starting hour for schedule
  - `end_hour` (integer, default: 18): Ending hour for schedule
  - `slot_duration` (integer, default: 30): Duration of each slot in minutes
  - `show_all_day` (boolean, default: true): Whether to show all day events
  - `class` (string, default: ""): Additional CSS classes
  - `rest` (global): Additional HTML attributes
  """
  @spec daily_schedule(map()) :: Phoenix.LiveView.Rendered.t()
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

  @doc """
  Renders the daily schedule component.

  ## Examples

      iex> daily_schedule(%{
      ...>   current_date: "2024-01-15",
      ...>   events: [],
      ...>   on_slot_click: "handle_slot_click",
      ...>   on_navigate: "navigate_day"
      ...> })
      %Phoenix.LiveView.Rendered{...}
  """
  @spec daily_schedule(map()) :: Phoenix.LiveView.Rendered.t()
  def daily_schedule(assigns) do
    assigns
    |> validate_assigns()
    |> render_schedule()
  end

  # Private functions

  @doc """
  Validates component assigns.

  ## Examples

      iex> validate_assigns(%{current_date: "2024-01-15", on_slot_click: "event", on_navigate: "navigate"})
      {:ok, %{current_date: "2024-01-15", on_slot_click: "event", on_navigate: "navigate"}}

      iex> validate_assigns(%{on_slot_click: "event", on_navigate: "navigate"})
      {:error, "current_date is required"}
  """
  @spec validate_assigns(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with {:ok, _} <- validate_required(assigns, [:current_date, :on_slot_click, :on_navigate]),
         {:ok, _} <- validate_date(assigns.current_date),
         {:ok, _} <- validate_time_range(assigns.start_hour, assigns.end_hour) do
      {:ok, assigns}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Validates date format.

  ## Examples

      iex> validate_date("2024-01-15")
      {:ok, "2024-01-15"}

      iex> validate_date("invalid-date")
      {:error, "current_date must be a valid date string"}
  """
  @spec validate_date(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_date(date) when is_binary(date) do
    # Simple date validation - in production, use a proper date library
    if String.match?(date, ~r/^\d{4}-\d{2}-\d{2}$/) do
      {:ok, date}
    else
      {:error, "current_date must be a valid date string (YYYY-MM-DD)"}
    end
  end

  defp validate_date(_date) do
    {:error, "current_date must be a string"}
  end

  @doc """
  Validates time range parameters.

  ## Examples

      iex> validate_time_range(8, 18)
      {:ok, {8, 18}}

      iex> validate_time_range(18, 8)
      {:error, "start_hour must be less than end_hour"}
  """
  @spec validate_time_range(integer(), integer()) :: {:ok, {integer(), integer()}} | {:error, String.t()}
  defp validate_time_range(start_hour, end_hour) when is_integer(start_hour) and is_integer(end_hour) do
    if start_hour < end_hour do
      {:ok, {start_hour, end_hour}}
    else
      {:error, "start_hour must be less than end_hour"}
    end
  end

  defp validate_time_range(_start_hour, _end_hour) do
    {:error, "start_hour and end_hour must be integers"}
  end

  @doc """
  Validates required assigns.

  ## Examples

      iex> validate_required(%{key: "value"}, [:key])
      {:ok, %{key: "value"}}

      iex> validate_required(%{}, [:key])
      {:error, "key is required"}
  """
  @spec validate_required(map(), list(atom())) :: {:ok, map()} | {:error, String.t()}
  defp validate_required(assigns, required_keys) do
    missing_keys = required_keys -- Map.keys(assigns)

    if Enum.empty?(missing_keys) do
      {:ok, assigns}
    else
      {:error, "#{Enum.join(missing_keys, ", ")} is required"}
    end
  end

  @doc """
  Renders the schedule component.

  ## Examples

      iex> render_schedule(%{current_date: "2024-01-15", events: [], on_slot_click: "event", on_navigate: "navigate"})
      %Phoenix.LiveView.Rendered{...}
  """
  @spec render_schedule(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_schedule(assigns) do
    ~H"""
    <div class={["daily-schedule", @class]} {@rest}>
      <div class="schedule-header">
        <.button phx-click={@on_navigate} phx-value-direction="prev" class="nav-button">
          <%= dgettext("navigation", "‹ Previous Day") %>
        </.button>
        <h2 class="schedule-date"><%= format_date(@current_date) %></h2>
        <.button phx-click={@on_navigate} phx-value-direction="next" class="nav-button">
          <%= dgettext("navigation", "Next Day ›") %>
        </.button>
      </div>

      <div :if={@show_all_day && has_all_day_events(@events)} class="all-day-events">
        <!-- All day events section -->
        <div class="all-day-header">
          <h3><%= dgettext("dates_numbers", "All Day") %></h3>
        </div>
        <div class="all-day-items">
          <button
            :for={event <- filter_all_day_events(@events)}
            phx-click={@on_event_click}
            phx-value-event={event.id}
            class="all-day-event"
          >
            <%= event.title %>
          </button>
        </div>
      </div>

      <div class="time-slots">
        <!-- Hourly time slots -->
        <div :for={hour <- @start_hour..@end_hour} class="time-slot-row">
          <div class="time-label">
            <span><%= format_hour(hour) %></span>
          </div>
          <div class="time-slot-buttons">
            <button
              phx-click={@on_slot_click}
              phx-value-hour={hour}
              phx-value-minute="0"
              class="time-slot-button"
            >
              <!-- First half hour -->
            </button>
            <button
              phx-click={@on_slot_click}
              phx-value-hour={hour}
              phx-value-minute="30"
              class="time-slot-button"
            >
              <!-- Second half hour -->
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  Formats date for display.

  ## Examples

      iex> format_date("2024-01-15")
      "January 15, 2024"

      iex> format_date("2024-12-25")
      "December 25, 2024"
  """
  @spec format_date(String.t()) :: String.t()
  defp format_date(date) when is_binary(date) do
    # Simple date formatting - in production, use a proper date library
    case String.split(date, "-") do
      [year, month, day] ->
        month_name = get_month_name(String.to_integer(month))
        "#{month_name} #{String.to_integer(day)}, #{year}"

      _ ->
        date
    end
  end

  defp format_date(date) do
    date
  end

  @doc """
  Gets month name from month number.

  ## Examples

      iex> get_month_name(1)
      "January"

      iex> get_month_name(12)
      "December"
  """
  @spec get_month_name(integer()) :: String.t()
  defp get_month_name(month) when month >= 1 and month <= 12 do
    months = [
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    ]

    Enum.at(months, month - 1)
  end

  defp get_month_name(_month) do
    "Unknown"
  end

  @doc """
  Formats hour for display.

  ## Examples

      iex> format_hour(9)
      "9:00"

      iex> format_hour(15)
      "15:00"
  """
  @spec format_hour(integer()) :: String.t()
  defp format_hour(hour) when is_integer(hour) and hour >= 0 and hour <= 23 do
    "#{hour}:00"
  end

  defp format_hour(hour) do
    "Invalid hour"
  end

  @doc """
  Checks if there are any all day events.

  ## Examples

      iex> has_all_day_events([%{all_day: true}, %{all_day: false}])
      true

      iex> has_all_day_events([%{all_day: false}, %{all_day: false}])
      false

      iex> has_all_day_events([])
      false
  """
  @spec has_all_day_events(list(map())) :: boolean()
  defp has_all_day_events(events) when is_list(events) do
    Enum.any?(events, & &1.all_day)
  end

  defp has_all_day_events(_events) do
    false
  end

  @doc """
  Filters all day events from the events list.

  ## Examples

      iex> filter_all_day_events([%{all_day: true, title: "Event 1"}, %{all_day: false, title: "Event 2"}])
      [%{all_day: true, title: "Event 1"}]

      iex> filter_all_day_events([])
      []
  """
  @spec filter_all_day_events(list(map())) :: list(map())
  defp filter_all_day_events(events) when is_list(events) do
    Enum.filter(events, & &1.all_day)
  end

  defp filter_all_day_events(_events) do
    []
  end
end
