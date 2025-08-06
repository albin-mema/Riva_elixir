defmodule RivaAshWeb.Components.Interactive.WeeklyCalendar do
  @moduledoc """
  Weekly calendar component with time slots.

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
  <.weekly_calendar
    current_week="2024-W01"
    on_slot_click="handle_slot_click"
    on_navigate="navigate_week"
  />

  # With events and time slots
  <.weekly_calendar
    current_week="2024-W01"
    events={@events}
    time_slots={@time_slots}
    on_slot_click="handle_slot_click"
    on_event_click="handle_event_click"
    on_navigate="navigate_week"
    start_hour={6}
    end_hour={22}
    slot_duration={30}
  />
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a weekly calendar view with time slots.

  ## Attributes

  - `current_week` (string, required): Current week being displayed (YYYY-Www format)
  - `events` (list, optional): List of events to display
  - `time_slots` (list, optional): List of time slots
  - `on_slot_click` (string, required): Event handler for slot clicks
  - `on_event_click` (string, optional): Event handler for event clicks
  - `on_navigate` (string, required): Event handler for navigation
  - `start_hour` (integer, default: 8): Starting hour for calendar
  - `end_hour` (integer, default: 18): Ending hour for calendar
  - `slot_duration` (integer, default: 60): Duration of each slot in minutes
  - `class` (string, default: ""): Additional CSS classes
  - `rest` (global): Additional HTML attributes
  """
  @spec weekly_calendar(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:current_week, :string, required: true)
  attr(:events, :list, default: [])
  attr(:time_slots, :list, default: [])
  attr(:on_slot_click, :string, required: true)
  attr(:on_event_click, :string, default: nil)
  attr(:on_navigate, :string, required: true)
  attr(:start_hour, :integer, default: 8)
  attr(:end_hour, :integer, default: 18)
  attr(:slot_duration, :integer, default: 60)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @doc """
  Renders the weekly calendar component.

  ## Examples

      iex> weekly_calendar(%{
      ...>   current_week: "2024-W01",
      ...>   events: [],
      ...>   time_slots: [],
      ...>   on_slot_click: "handle_slot_click",
      ...>   on_navigate: "navigate_week"
      ...> })
      %Phoenix.LiveView.Rendered{...}
  """
  @spec weekly_calendar(map()) :: Phoenix.LiveView.Rendered.t()
  def weekly_calendar(assigns) do
    assigns
    |> validate_assigns()
    |> render_calendar()
  end

  # Private functions

  @doc """
  Validates component assigns.

  ## Examples

      iex> validate_assigns(%{current_week: "2024-W01", on_slot_click: "event", on_navigate: "navigate"})
      {:ok, %{current_week: "2024-W01", on_slot_click: "event", on_navigate: "navigate"}}

      iex> validate_assigns(%{on_slot_click: "event", on_navigate: "navigate"})
      {:error, "current_week is required"}
  """
  @spec validate_assigns(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with {:ok, _} <- validate_required(assigns, [:current_week, :on_slot_click, :on_navigate]),
         {:ok, _} <- validate_week(assigns.current_week),
         {:ok, _} <- validate_time_range(assigns.start_hour, assigns.end_hour),
         {:ok, _} <- validate_slot_duration(assigns.slot_duration),
         {:ok, _} <- validate_event_data(assigns.events),
         {:ok, _} <- validate_time_slot_data(assigns.time_slots) do
      {:ok, assigns}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Validates week format.

  ## Examples

      iex> validate_week("2024-W01")
      {:ok, "2024-W01"}

      iex> validate_week("invalid-week")
      {:error, "current_week must be a valid week string (YYYY-Www)"}
  """
  @spec validate_week(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_week(week) when is_binary(week) do
    # Simple week validation - in production, use a proper date library
    if String.match?(week, ~r/^\d{4}-W(0[1-9]|[1-4][0-9]|5[0-3])$/) do
      {:ok, week}
    else
      {:error, "current_week must be a valid week string (YYYY-Www)"}
    end
  end

  defp validate_week(_week) do
    {:error, "current_week must be a string"}
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
  Validates slot duration.

  ## Examples

      iex> validate_slot_duration(60)
      {:ok, 60}

      iex> validate_slot_duration(0)
      {:error, "slot_duration must be greater than 0"}

      iex> validate_slot_duration(-1)
      {:error, "slot_duration must be greater than 0"}
  """
  @spec validate_slot_duration(integer()) :: {:ok, integer()} | {:error, String.t()}
  defp validate_slot_duration(duration) when is_integer(duration) do
    if duration > 0 do
      {:ok, duration}
    else
      {:error, "slot_duration must be greater than 0"}
    end
  end

  defp validate_slot_duration(_duration) do
    {:error, "slot_duration must be an integer"}
  end

  @doc """
  Validates event data.

  ## Examples

      iex> validate_event_data([])
      {:ok, []}

      iex> validate_event_data([%{id: 1, title: "Event"}])
      {:ok, [%{id: 1, title: "Event"}]}

      iex> validate_event_data([%{invalid: "data"}])
      {:error, "Invalid event data"}

      iex> validate_event_data("invalid")
      {:error, "events must be a list"}
  """
  @spec validate_event_data(list(map())) :: {:ok, list(map())} | {:error, String.t()}
  defp validate_event_data(events) when is_list(events) do
    case Enum.find(events, fn event -> not valid_event?(event) end) do
      nil -> {:ok, events}
      _ -> {:error, "Invalid event data"}
    end
  end

  defp validate_event_data(_events) do
    {:error, "events must be a list"}
  end

  @doc """
  Validates time slot data.

  ## Examples

      iex> validate_time_slot_data([])
      {:ok, []}

      iex> validate_time_slot_data([%{id: 1, start_time: "09:00", end_time: "10:00"}])
      {:ok, [%{id: 1, start_time: "09:00", end_time: "10:00"}]}

      iex> validate_time_slot_data([%{invalid: "data"}])
      {:error, "Invalid time slot data"}

      iex> validate_time_slot_data("invalid")
      {:error, "time_slots must be a list"}
  """
  @spec validate_time_slot_data(list(map())) :: {:ok, list(map())} | {:error, String.t()}
  defp validate_time_slot_data(time_slots) when is_list(time_slots) do
    case Enum.find(time_slots, fn slot -> not valid_time_slot?(slot) end) do
      nil -> {:ok, time_slots}
      _ -> {:error, "Invalid time slot data"}
    end
  end

  defp validate_time_slot_data(_time_slots) do
    {:error, "time_slots must be a list"}
  end

  @doc """
  Checks if event data is valid.

  ## Examples

      iex> is_valid_event(%{id: 1, title: "Event"})
      true

      iex> is_valid_event(%{id: 1})
      false

      iex> is_valid_event("invalid")
      false
  """
  @spec valid_event?(map()) :: boolean()
  defp valid_event?(event) when is_map(event) do
    Map.has_key?(event, :id) and
      Map.has_key?(event, :title) and
      is_integer(event.id) and
      event.id > 0
  end

  defp valid_event?(_event) do
    false
  end

  @doc """
  Checks if time slot data is valid.

  ## Examples

      iex> is_valid_time_slot(%{id: 1, start_time: "09:00", end_time: "10:00"})
      true

      iex> is_valid_time_slot(%{id: 1, start_time: "09:00"})
      false

      iex> is_valid_time_slot("invalid")
      false
  """
  @spec valid_time_slot?(map()) :: boolean()
  defp valid_time_slot?(slot) when is_map(slot) do
    Map.has_key?(slot, :id) and
      Map.has_key?(slot, :start_time) and
      Map.has_key?(slot, :end_time) and
      is_integer(slot.id) and
      slot.id > 0
  end

  defp valid_time_slot?(_slot) do
    false
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
  Renders the calendar component.

  ## Examples

      iex> render_calendar(%{current_week: "2024-W01", events: [], time_slots: [], on_slot_click: "event", on_navigate: "navigate"})
      %Phoenix.LiveView.Rendered{...}
  """
  @spec render_calendar(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_calendar(assigns) do
    ~H"""
    <div class={["weekly-calendar", @class]} {@rest}>
      <div class="calendar-header">
        <.button phx-click={@on_navigate} phx-value-direction="prev" class="nav-button">
          ‹ Previous Week
        </.button>
        <h2 class="calendar-title">Week of <%= format_week(@current_week) %></h2>
        <.button phx-click={@on_navigate} phx-value-direction="next" class="nav-button">
          Next Week ›
        </.button>
      </div>

      <div class="calendar-grid">
        <!-- Time column -->
        <div class="time-column">
          <div class="time-header">Time</div>
          <div :for={hour <- @start_hour..@end_hour} class="time-label">
            <%= format_hour(hour) %>
          </div>
        </div>

        <!-- Day columns -->
        <div :for={day <- get_days_of_week()} class="day-column">
          <div class="day-header"><%= day %></div>
          <div :for={hour <- @start_hour..@end_hour} class="time-slot">
            <button
              phx-click={@on_slot_click}
              phx-value-day={day}
              phx-value-hour={hour}
              class="slot-button"
            >
              <!-- Time slot content -->
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  Gets days of the week.

  ## Examples

      iex> get_days_of_week()
      ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
  """
  @spec get_days_of_week() :: list(String.t())
  defp get_days_of_week do
    ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
  end

  @doc """
  Formats week for display.

  ## Examples

      iex> format_week("2024-W01")
      "Week 1, 2024"

      iex> format_week("2024-W52")
      "Week 52, 2024"
  """
  @spec format_week(String.t()) :: String.t()
  defp format_week(week) when is_binary(week) do
    case String.split(week, "-W") do
      [year, week_num] ->
        year = String.to_integer(year)
        week_num = String.to_integer(week_num)
        "Week #{week_num}, #{year}"

      _ ->
        week
    end
  end

  defp format_week(week) do
    week
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
end
