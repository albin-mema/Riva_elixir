defmodule RivaAshWeb.Components.Interactive.RecurrencePattern do
  @moduledoc """
  Recurring reservation pattern setup component.

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
  <.recurrence_pattern
    pattern={@pattern}
    on_pattern_change="handle_pattern_change"
  />

  # With preview functionality
  <.recurrence_pattern
    pattern={@pattern}
    on_pattern_change="handle_pattern_change"
    on_preview="preview_dates"
    preview_dates={@preview_dates}
    max_occurrences={100}
  />
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField

  @doc """
  Renders a recurrence pattern configuration interface.

  ## Attributes

  - `pattern` (map, default: %{}): Pattern configuration data
  - `on_pattern_change` (string, required): Event handler for pattern changes
  - `on_preview` (string, optional): Event handler for preview generation
  - `preview_dates` (list, optional): List of preview dates
  - `max_occurrences` (integer, default: 365): Maximum number of occurrences
  - `class` (string, default: ""): Additional CSS classes
  - `rest` (global): Additional HTML attributes
  """
  @spec recurrence_pattern(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:pattern, :map, default: %{})
  attr(:on_pattern_change, :string, required: true)
  attr(:on_preview, :string, default: nil)
  attr(:preview_dates, :list, default: [])
  attr(:max_occurrences, :integer, default: 365)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @doc """
  Renders the recurrence pattern component.

  ## Examples

      iex> recurrence_pattern(%{
      ...>   pattern: %{},
      ...>   on_pattern_change: "handle_pattern_change"
      ...> })
      %Phoenix.LiveView.Rendered{...}
  """
  @spec recurrence_pattern(map()) :: Phoenix.LiveView.Rendered.t()
  def recurrence_pattern(assigns) do
    assigns
    |> validate_assigns()
    |> render_pattern()
  end

  # Private functions

  @doc """
  Validates component assigns.

  ## Examples

      iex> validate_assigns(%{pattern: %{}, on_pattern_change: "handle_change"})
      {:ok, %{pattern: %{}, on_pattern_change: "handle_change"}}

      iex> validate_assigns(%{pattern: %{}})
      {:error, "on_pattern_change is required"}
  """
  @spec validate_assigns(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with {:ok, _} <- validate_required(assigns, [:on_pattern_change]),
         {:ok, _} <- validate_pattern(assigns.pattern),
         {:ok, _} <- validate_max_occurrences(assigns.max_occurrences) do
      {:ok, assigns}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Validates pattern data.

  ## Examples

      iex> validate_pattern(%{})
      {:ok, %{}}

      iex> validate_pattern(%{form: %{pattern_type: "daily"}})
      {:ok, %{form: %{pattern_type: "daily"}}}

      iex> validate_pattern("invalid")
      {:error, "pattern must be a map"}
  """
  @spec validate_pattern(any()) :: {:ok, map()} | {:error, String.t()}
  defp validate_pattern(pattern) when is_map(pattern) do
    {:ok, pattern}
  end

  defp validate_pattern(_pattern) do
    {:error, "pattern must be a map"}
  end

  @doc """
  Validates max occurrences.

  ## Examples

      iex> validate_max_occurrences(365)
      {:ok, 365}

      iex> validate_max_occurrences(0)
      {:error, "max_occurrences must be greater than 0"}

      iex> validate_max_occurrences(-1)
      {:error, "max_occurrences must be greater than 0"}
  """
  @spec validate_max_occurrences(integer()) :: {:ok, integer()} | {:error, String.t()}
  defp validate_max_occurrences(max_occurrences) when is_integer(max_occurrences) do
    if max_occurrences > 0 do
      {:ok, max_occurrences}
    else
      {:error, "max_occurrences must be greater than 0"}
    end
  end

  defp validate_max_occurrences(_max_occurrences) do
    {:error, "max_occurrences must be an integer"}
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
  Renders the pattern component.

  ## Examples

      iex> render_pattern(%{pattern: %{}, on_pattern_change: "handle_change"})
      %Phoenix.LiveView.Rendered{...}
  """
  @spec render_pattern(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_pattern(assigns) do
    ~H"""
    <div class={["recurrence-pattern", @class]} {@rest}>
      <div class="pattern-section">
        <h3>Recurrence Pattern</h3>

        <.select_field
          field={get_pattern_field(@pattern, :pattern_type)}
          label="Pattern Type"
          options={[
            {"Daily", "daily"},
            {"Weekly", "weekly"},
            {"Monthly", "monthly"},
            {"Custom", "custom"}
          ]}
          prompt="Select pattern type"
          phx-change={@on_pattern_change}
        />
      </div>

      <div :if={get_pattern_type(@pattern) == "daily"} class="pattern-section">
        <.form_field
          field={get_pattern_field(@pattern, :interval)}
          label="Every X days"
          type="number"
          phx-change={@on_pattern_change}
        />
      </div>

      <div :if={get_pattern_type(@pattern) == "weekly"} class="pattern-section">
        <.form_field
          field={get_pattern_field(@pattern, :interval)}
          label="Every X weeks"
          type="number"
          phx-change={@on_pattern_change}
        />

        <div class="days-of-week">
          <label>Days of the week:</label>
          <div class="day-checkboxes">
            <.form_field field={get_pattern_field(@pattern, :monday)} label="Monday" type="checkbox" phx-change={@on_pattern_change} />
            <.form_field field={get_pattern_field(@pattern, :tuesday)} label="Tuesday" type="checkbox" phx-change={@on_pattern_change} />
            <.form_field field={get_pattern_field(@pattern, :wednesday)} label="Wednesday" type="checkbox" phx-change={@on_pattern_change} />
            <.form_field field={get_pattern_field(@pattern, :thursday)} label="Thursday" type="checkbox" phx-change={@on_pattern_change} />
            <.form_field field={get_pattern_field(@pattern, :friday)} label="Friday" type="checkbox" phx-change={@on_pattern_change} />
            <.form_field field={get_pattern_field(@pattern, :saturday)} label="Saturday" type="checkbox" phx-change={@on_pattern_change} />
            <.form_field field={get_pattern_field(@pattern, :sunday)} label="Sunday" type="checkbox" phx-change={@on_pattern_change} />
          </div>
        </div>
      </div>

      <div :if={get_pattern_type(@pattern) == "monthly"} class="pattern-section">
        <.select_field
          field={get_pattern_field(@pattern, :monthly_type)}
          label="Monthly Type"
          options={[
            {"Same date each month", "date"},
            {"Same day of week", "day_of_week"}
          ]}
          phx-change={@on_pattern_change}
        />
      </div>

      <div class="pattern-section">
        <.form_field
          field={get_pattern_field(@pattern, :start_date)}
          label="Start Date"
          type="date"
          phx-change={@on_pattern_change}
        />

        <.select_field
          field={get_pattern_field(@pattern, :end_type)}
          label="End Type"
          options={[
            {"Never", "never"},
            {"After X occurrences", "count"},
            {"On specific date", "date"}
          ]}
          phx-change={@on_pattern_change}
        />

        <.form_field
          :if={get_end_type(@pattern) == "count"}
          field={get_pattern_field(@pattern, :occurrence_count)}
          label="Number of occurrences"
          type="number"
          phx-change={@on_pattern_change}
        />

        <.form_field
          :if={get_end_type(@pattern) == "date"}
          field={get_pattern_field(@pattern, :end_date)}
          label="End Date"
          type="date"
          phx-change={@on_pattern_change}
        />
      </div>

      <div :if={@on_preview} class="preview-section">
        <button phx-click={@on_preview} class="preview-button">Preview Dates</button>

        <div :if={not Enum.empty?(@preview_dates)} class="preview-dates">
          <h4>Preview (first 10 dates):</h4>
          <ul class="date-list">
            <li :for={date <- Enum.take(@preview_dates, 10)}>
              <%= format_date_for_display(date) %>
            </li>
          </ul>
          <p :if={length(@preview_dates) > 10} class="more-dates">
            ... and <%= length(@preview_dates) - 10 %> more dates
          </p>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  Gets pattern field value.

  ## Examples

      iex> get_pattern_field(%{form: %{pattern_type: "daily"}}, :pattern_type)
      "daily"

      iex> get_pattern_field(%{}, :pattern_type)
      nil

      iex> get_pattern_field(%{form: %{invalid: "data"}}, :pattern_type)
      nil
  """
  @spec get_pattern_field(map(), atom()) :: any()
  defp get_pattern_field(pattern, field) when is_map(pattern) do
    pattern
    |> get_in([:form, field])
    |> case do
      nil -> nil
      value -> value
    end
  end

  defp get_pattern_field(_pattern, _field) do
    nil
  end

  @doc """
  Gets pattern type.

  ## Examples

      iex> get_pattern_type(%{form: %{pattern_type: "daily"}})
      "daily"

      iex> get_pattern_type(%{})
      nil

      iex> get_pattern_type(%{form: %{}})
      nil
  """
  @spec get_pattern_type(map()) :: String.t() | nil
  defp get_pattern_type(pattern) when is_map(pattern) do
    get_pattern_field(pattern, :pattern_type)
  end

  defp get_pattern_type(_pattern) do
    nil
  end

  @doc """
  Gets end type.

  ## Examples

      iex> get_end_type(%{form: %{end_type: "count"}})
      "count"

      iex> get_end_type(%{})
      nil

      iex> get_end_type(%{form: %{}})
      nil
  """
  @spec get_end_type(map()) :: String.t() | nil
  defp get_end_type(pattern) when is_map(pattern) do
    get_pattern_field(pattern, :end_type)
  end

  defp get_end_type(_pattern) do
    nil
  end

  @doc """
  Formats date for display.

  ## Examples

      iex> format_date_for_display("2024-01-15")
      "January 15, 2024"

      iex> format_date_for_display("2024-12-25")
      "December 25, 2024"

      iex> format_date_for_display("invalid-date")
      "invalid-date"
  """
  @spec format_date_for_display(String.t()) :: String.t()
  defp format_date_for_display(date) when is_binary(date) do
    case String.split(date, "-") do
      [year, month, day] ->
        year = String.to_integer(year)
        month = String.to_integer(month)
        day = String.to_integer(day)

        month_name = get_month_name(month)
        "#{month_name} #{day}, #{year}"

      _ ->
        date
    end
  end

  defp format_date_for_display(date) do
    date
  end

  @doc """
  Gets month name from month number.

  ## Examples

      iex> get_month_name(1)
      "January"

      iex> get_month_name(12)
      "December"

      iex> get_month_name(13)
      "Unknown"
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
end
