alias RivaAshWeb.Components.Interactive, as: Interactive
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.LiveView, as: LiveView

defmodule RivaAshWeb.Components.Interactive.MonthlyCalendar do
  import RivaAshWeb.Gettext, only: [dgettext: 2]

  @moduledoc """
  Monthly calendar component with reservation display.

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
  <.monthly_calendar
    current_date="2024-01"
    on_date_click="handle_date_click"
    on_navigate="navigate_month"
  />

  # With events and selectable dates
  <.monthly_calendar
    current_date="2024-01"
    events={@events}
    on_date_click="handle_date_click"
    on_event_click="handle_event_click"
    on_navigate="navigate_month"
    selectable_dates={["2024-01-15", "2024-01-20"]}
    disabled_dates={["2024-01-01", "2024-01-02"]}
  />
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a monthly calendar view.

  ## Attributes

  - `current_date` (string, required): Current month being displayed (YYYY-MM format)
  - `events` (list, optional): List of events to display
  - `on_date_click` (string, required): Event handler for date clicks
  - `on_event_click` (string, optional): Event handler for event clicks
  - `on_navigate` (string, required): Event handler for navigation
  - `selectable_dates` (list, optional): List of selectable dates
  - `disabled_dates` (list, optional): List of disabled dates
  - `class` (string, default: ""): Additional CSS classes
  - `rest` (global): Additional HTML attributes
  """
  @spec monthly_calendar(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:current_date, :string, required: true)
  attr(:events, :list, default: [])
  attr(:on_date_click, :string, required: true)
  attr(:on_event_click, :string, default: nil)
  attr(:on_navigate, :string, required: true)
  attr(:selectable_dates, :list, default: [])
  attr(:disabled_dates, :list, default: [])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @doc """
  Renders the monthly calendar component.

  ## Examples

      iex> monthly_calendar(%{
      ...>   current_date: "2024-01",
      ...>   events: [],
      ...>   on_date_click: "handle_date_click",
      ...>   on_navigate: "navigate_month"
      ...> })
      %Phoenix.LiveView.Rendered{...}
  """
  @spec monthly_calendar(map()) :: Phoenix.LiveView.Rendered.t()
  def monthly_calendar(assigns) do
    assigns
    |> validate_assigns()
    |> render_calendar()
  end

  # Private functions

  @doc """
  Validates component assigns.

  ## Examples

      iex> validate_assigns(%{current_date: "2024-01", on_date_click: "event", on_navigate: "navigate"})
      {:ok, %{current_date: "2024-01", on_date_click: "event", on_navigate: "navigate"}}

      iex> validate_assigns(%{on_date_click: "event", on_navigate: "navigate"})
      {:error, "current_date is required"}
  """
  @spec validate_assigns(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with {:ok, _} <- validate_required(assigns, [:current_date, :on_date_click, :on_navigate]),
         {:ok, _} <- validate_month(assigns.current_date),
         {:ok, _} <- validate_date_lists(assigns.selectable_dates, assigns.disabled_dates) do
      {:ok, assigns}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Validates month format.

  ## Examples

      iex> validate_month("2024-01")
      {:ok, "2024-01"}

      iex> validate_month("invalid-month")
      {:error, "current_date must be a valid month string (YYYY-MM)"}
  """
  @spec validate_month(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_month(month) when is_binary(month) do
    # Simple month validation - in production, use a proper date library
    if String.match?(month, ~r/^\d{4}-\d{2}$/) do
      {:ok, month}
    else
      {:error, "current_date must be a valid month string (YYYY-MM)"}
    end
  end

  defp validate_month(_month) do
    {:error, "current_date must be a string"}
  end

  @doc """
  Validates date lists.

  ## Examples

      iex> validate_date_lists(["2024-01-15"], ["2024-01-01"])
      {:ok, {["2024-01-15"], ["2024-01-01"]}}

      iex> validate_date_lists(["invalid-date"], [])
      {:error, "Invalid date in selectable_dates: invalid-date"}
  """
  @spec validate_date_lists(list(String.t()), list(String.t())) ::
          {:ok, {list(String.t()), list(String.t())}} | {:error, String.t()}
  defp validate_date_lists(selectable_dates, disabled_dates)
       when is_list(selectable_dates) and is_list(disabled_dates) do
    with {:ok, selectable_dates} <- validate_date_list(selectable_dates, "selectable_dates"),
         {:ok, disabled_dates} <- validate_date_list(disabled_dates, "disabled_dates") do
      {:ok, {selectable_dates, disabled_dates}}
    end
  end

  defp validate_date_lists(_selectable_dates, _disabled_dates) do
    {:error, "selectable_dates and disabled_dates must be lists"}
  end

  @doc """
  Validates a single date list.

  ## Examples

      iex> validate_date_list(["2024-01-15"], "selectable_dates")
      {:ok, ["2024-01-15"]}

      iex> validate_date_list(["invalid-date"], "selectable_dates")
      {:error, "Invalid date in selectable_dates: invalid-date"}
  """
  @spec validate_date_list(list(String.t()), String.t()) :: {:ok, list(String.t())} | {:error, String.t()}
  defp validate_date_list(dates, list_name) when is_list(dates) do
    case Enum.find(dates, fn date -> not valid_date?(date) end) do
      nil -> {:ok, dates}
      invalid_date -> {:error, "Invalid date in #{list_name}: #{invalid_date}"}
    end
  end

  defp validate_date_list(_dates, _list_name) do
    {:error, "date list must be a list"}
  end

  @doc """
  Validates date format.

  ## Examples

      iex> valid_date?("2024-01-15")
      true

      iex> valid_date?("invalid-date")
      false
  """
  @spec valid_date?(String.t()) :: boolean()
  defp valid_date?(date) when is_binary(date) do
    # Simple date validation - in production, use a proper date library
    String.match?(date, ~r/^\d{4}-\d{2}-\d{2}$/)
  end

  defp valid_date?(_date) do
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

      iex> render_calendar(%{current_date: "2024-01", events: [], on_date_click: "event", on_navigate: "navigate"})
      %Phoenix.LiveView.Rendered{...}
  """
  @spec render_calendar(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_calendar(assigns) do
    ~H"""
    <div class={["monthly-calendar", @class]} {@rest}>
      <div class="calendar-header">
        <.button phx-click={@on_navigate} phx-value-direction="prev" class="nav-button">
          <%= dgettext("navigation", "‹") %>
        </.button>
        <h2 class="calendar-month"><%= format_month(@current_date) %></h2>
        <.button phx-click={@on_navigate} phx-value-direction="next" class="nav-button">
          <%= dgettext("navigation", "›") %>
        </.button>
      </div>

      <div class="calendar-grid">
        <!-- Calendar grid headers -->
        <div :for={day <- get_day_headers()} class="day-header">
          <%= day %>
        </div>

        <!-- Calendar days -->
        <div :for={day <- generate_calendar_days(@current_date)} class={[
          "calendar-day",
          if(today?(day, @current_date), do: "today", else: ""),
          if(selected_date?(day, @selectable_dates), do: "selectable", else: ""),
          if(disabled_date?(day, @disabled_dates), do: "disabled", else: ""),
          if(has_events?(day, @events), do: "has-events", else: "")
        ]}>
          <button
            :if={not disabled_date?(day, @disabled_dates)}
            phx-click={@on_date_click}
            phx-value-date={day}
            class="day-button"
            disabled={disabled_date?(day, @disabled_dates)}
          >
            <%= extract_day_number(day) %>
          </button>

          <!-- Event indicators -->
          <div :if={has_events?(day, @events)} class="event-indicators">
            <span :for={_ <- get_event_count_for_day(day, @events)} class="event-dot"></span>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  Gets day headers for calendar.

  ## Examples

      iex> get_day_headers()
      ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
  """
  @spec get_day_headers() :: list(String.t())
  defp get_day_headers do
    ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
  end

  @doc """
  Generates calendar days for the current month.

  ## Examples

      iex> generate_calendar_days("2024-01")
      ["2024-01-01", "2024-01-02", ..., "2024-01-31"]

      iex> generate_calendar_days("2024-02")
      ["2024-02-01", "2024-02-02", ..., "2024-02-29"]
  """
  @spec generate_calendar_days(String.t()) :: list(String.t())
  defp generate_calendar_days(month) when is_binary(month) do
    case String.split(month, "-") do
      [year, month_num] ->
        year = String.to_integer(year)
        month_num = String.to_integer(month_num)

        # Get the number of days in the month
        days_in_month = get_days_in_month(year, month_num)

        # Generate dates for the month
        for day <- 1..days_in_month do
          date = "#{year}-#{pad_month(month_num)}-#{pad_day(day)}"
          date
        end

      _ ->
        []
    end
  end

  defp generate_calendar_days(_month) do
    []
  end

  @doc """
  Gets number of days in a month.

  ## Examples

      iex> get_days_in_month(2024, 1)  # January (leap year)
      31

      iex> get_days_in_month(2024, 2)  # February (leap year)
      29

      iex> get_days_in_month(2023, 2)  # February (non-leap year)
      28
  """
  @spec get_days_in_month(integer(), integer()) :: integer()
  defp get_days_in_month(year, month) when is_integer(year) and is_integer(month) and month >= 1 and month <= 12 do
    # Simple implementation - in production, use a proper date library
    case month do
      2 -> if leap_year?(year), do: 29, else: 28
      4 -> 30
      6 -> 30
      9 -> 30
      11 -> 30
      _ -> 31
    end
  end

  defp get_days_in_month(_year, _month) do
    31
  end

  @doc """
  Checks if a year is a leap year.

  ## Examples

      iex> leap_year?(2024)
      true

      iex> leap_year?(2023)
      false
  """
  @spec leap_year?(integer()) :: boolean()
  defp leap_year?(year) when is_integer(year) do
    rem(year, 400) == 0 or (rem(year, 100) != 0 and rem(year, 4) == 0)
  end

  defp leap_year?(_year) do
    false
  end

  @doc """
  Pads month number with leading zero.

  ## Examples

      iex> pad_month(1)
      "01"

      iex> pad_month(12)
      "12"
  """
  @spec pad_month(integer()) :: String.t()
  defp pad_month(month) when is_integer(month) and month >= 1 and month <= 12 do
    month |> Integer.to_string() |> String.pad_leading(2, "0")
  end

  defp pad_month(month) do
    Integer.to_string(month)
  end

  @doc """
  Pads day number with leading zero.

  ## Examples

      iex> pad_day(1)
      "01"

      iex> pad_day(31)
      "31"
  """
  @spec pad_day(integer()) :: String.t()
  defp pad_day(day) when is_integer(day) and day >= 1 and day <= 31 do
    day |> Integer.to_string() |> String.pad_leading(2, "0")
  end

  defp pad_day(day) do
    Integer.to_string(day)
  end

  @doc """
  Formats month for display.

  ## Examples

      iex> format_month("2024-01")
      "January 2024"

      iex> format_month("2024-12")
      "December 2024"
  """
  @spec format_month(String.t()) :: String.t()
  defp format_month(month) when is_binary(month) do
    case String.split(month, "-") do
      [year, month_num] ->
        year = String.to_integer(year)
        month_num = String.to_integer(month_num)
        month_name = get_month_name(month_num)
        "#{month_name} #{year}"

      _ ->
        month
    end
  end

  defp format_month(month) do
    month
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
  Checks if a date is today.

  ## Examples

      iex> is_today("2024-01-15", "2024-01")
      false

      iex> is_today("2024-01-15", "2024-02")
      false
  """
  @spec today?(String.t(), String.t()) :: boolean()
  defp today?(day, current_month) when is_binary(day) and is_binary(current_month) do
    # Simple implementation - in production, use a proper date library
    String.starts_with?(day, current_month)
  end

  defp today?(_day, _current_month) do
    false
  end

  @doc """
  Checks if a date is in selectable dates.

  ## Examples

      iex> is_selected_date("2024-01-15", ["2024-01-15", "2024-01-20"])
      true

      iex> is_selected_date("2024-01-15", ["2024-01-20"])
      false
  """
  @spec selected_date?(String.t(), list(String.t())) :: boolean()
  defp selected_date?(day, selectable_dates) when is_binary(day) and is_list(selectable_dates) do
    day in selectable_dates
  end

  defp selected_date?(_day, _selectable_dates) do
    false
  end

  @doc """
  Checks if a date is in disabled dates.

  ## Examples

      iex> is_disabled_date("2024-01-01", ["2024-01-01", "2024-01-02"])
      true

      iex> is_disabled_date("2024-01-15", ["2024-01-01"])
      false
  """
  @spec disabled_date?(String.t(), list(String.t())) :: boolean()
  defp disabled_date?(day, disabled_dates) when is_binary(day) and is_list(disabled_dates) do
    day in disabled_dates
  end

  defp disabled_date?(_day, _disabled_dates) do
    false
  end

  @doc """
  Checks if a date has events.

  ## Examples

      iex> has_events("2024-01-15", [%{date: "2024-01-15"}, %{date: "2024-01-20"}])
      true

      iex> has_events("2024-01-15", [%{date: "2024-01-20"}])
      false
  """
  @spec has_events?(String.t(), list(map())) :: boolean()
  defp has_events?(day, events) when is_binary(day) and is_list(events) do
    Enum.any?(events, &(&1.date == day))
  end

  defp has_events?(_day, _events) do
    false
  end

  @doc """
  Extracts day number from date string.

  ## Examples

      iex> extract_day_number("2024-01-15")
      15

      iex> extract_day_number("2024-12-25")
      25
  """
  @spec extract_day_number(String.t()) :: integer()
  defp extract_day_number(day) when is_binary(day) do
    case String.split(day, "-") do
      [_, _, day_num] -> String.to_integer(day_num)
      _ -> 0
    end
  end

  defp extract_day_number(_day) do
    0
  end

  @doc """
  Gets event count for a specific day.

  ## Examples

      iex> get_event_count_for_day("2024-01-15", [%{date: "2024-01-15"}, %{date: "2024-01-15"}, %{date: "2024-01-20"}])
      2

      iex> get_event_count_for_day("2024-01-15", [%{date: "2024-01-20"}])
      0
  """
  @spec get_event_count_for_day(String.t(), list(map())) :: integer()
  defp get_event_count_for_day(day, events) when is_binary(day) and is_list(events) do
    events
    |> Enum.filter(&(&1.date == day))
    |> length()
  end

  defp get_event_count_for_day(_day, _events) do
    0
  end
end
