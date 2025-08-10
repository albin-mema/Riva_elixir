alias RivaAshWeb.Components.Interactive, as: Interactive
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.LiveView, as: LiveView

defmodule RivaAshWeb.Components.Interactive.AvailabilityGrid do
  @moduledoc """
  Weekly availability grid editor component.

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
  <.availability_grid
    availability={@availability}
    on_slot_toggle="toggle_availability"
    editable={true}
  />

  # With bulk actions
  <.availability_grid
    availability={@availability}
    on_slot_toggle="toggle_availability"
    on_bulk_action="handle_bulk_action"
    start_hour={6}
    end_hour={22}
  />
  """
  use Phoenix.Component
  import RivaAshWeb.Components.UI.Button
  import RivaAshWeb.Components.Atoms.Toggle

  @doc """
  Renders a weekly availability grid editor.

  ## Attributes

  - `availability` (map, required): Map containing availability data
  - `on_slot_toggle` (string, required): Event handler for slot toggle
  - `on_bulk_action` (string, optional): Event handler for bulk actions
  - `time_slots` (list, optional): List of time slots
  - `start_hour` (integer, default: 8): Starting hour for grid
  - `end_hour` (integer, default: 18): Ending hour for grid
  - `slot_duration` (integer, default: 60): Duration of each slot in minutes
  - `editable` (boolean, default: true): Whether grid is editable
  - `class` (string, default: ""): Additional CSS classes
  - `rest` (global): Additional HTML attributes
  """
  @spec availability_grid(map()) :: Phoenix.LiveView.Rendered.t()
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

  @doc """
  Renders the availability grid component.

  ## Examples

      iex> availability_grid(%{
      ...>   availability: %{"monday" => %{8 => true, 9 => false}},
      ...>   on_slot_toggle: "toggle_availability",
      ...>   editable: true
      ...> })
      %Phoenix.LiveView.Rendered{...}
  """
  @spec availability_grid(map()) :: Phoenix.LiveView.Rendered.t()
  def availability_grid(assigns) do
    assigns
    |> validate_assigns()
    |> render_grid()
  end

  # Private functions

  @doc """
  Validates component assigns.

  ## Examples

      iex> validate_assigns(%{availability: %{}, on_slot_toggle: "event"})
      {:ok, %{availability: %{}, on_slot_toggle: "event"}}

      iex> validate_assigns(%{on_slot_toggle: "event"})
      {:error, "availability is required"}
  """
  @spec validate_assigns(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with {:ok, _} <- validate_required(assigns, [:availability, :on_slot_toggle]),
         {:ok, _} <- validate_availability_structure(assigns.availability),
         {:ok, _} <- validate_time_range(assigns.start_hour, assigns.end_hour) do
      {:ok, assigns}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Validates availability data structure.

  ## Examples

      iex> validate_availability_structure(%{"monday" => %{8 => true}})
      {:ok, %{"monday" => %{8 => true}}}

      iex> validate_availability_structure("invalid")
      {:error, "availability must be a map"}
  """
  @spec validate_availability_structure(any()) :: {:ok, map()} | {:error, String.t()}
  defp validate_availability_structure(availability) when is_map(availability) do
    {:ok, availability}
  end

  defp validate_availability_structure(_availability) do
    {:error, "availability must be a map"}
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
  Renders the grid component.

  ## Examples

      iex> render_grid(%{availability: %{}, on_slot_toggle: "event"})
      %Phoenix.LiveView.Rendered{...}
  """
  @spec render_grid(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_grid(assigns) do
    ~H"""
    <div class={["availability-grid", @class]} {@rest}>
      <div :if={@on_bulk_action && @editable} class="bulk-actions">
        <h3>Bulk Actions</h3>
        <.button phx-click={@on_bulk_action} phx-value-action="select_all">Select All</.button>
        <.button phx-click={@on_bulk_action} phx-value-action="clear_all">Clear All</.button>
        <.button phx-click={@on_bulk_action} phx-value-action="copy_day">Copy Day</.button>
      </div>

      <div class="grid-container">
        <!-- Header row with days -->
        <div class="grid-header">
          <div class="time-header">Time</div>
          <div :for={day <- get_days_of_week()} class="day-header">
            <%= String.capitalize(day) %>
          </div>
        </div>

        <!-- Time slot rows -->
        <div :for={hour <- @start_hour..@end_hour} class="grid-row">
          <div class="time-cell">
            <span><%= format_hour(hour) %></span>
          </div>

          <div :for={day <- get_days_of_week()} class="day-cell">
            <.toggle
              :if={@editable}
              checked={get_availability(@availability, day, hour)}
              phx-click={@on_slot_toggle}
              phx-value-day={day}
              phx-value-hour={hour}
              class="availability-toggle"
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

      <div :if={@editable} class="quick-templates">
        <h4>Quick Templates</h4>
        <.button phx-click={@on_bulk_action} phx-value-action="business_hours">Business Hours (9-5)</.button>
        <.button phx-click={@on_bulk_action} phx-value-action="weekdays_only">Weekdays Only</.button>
        <.button phx-click={@on_bulk_action} phx-value-action="weekends_only">Weekends Only</.button>
      </div>
    </div>
    """
  end

  @doc """
  Gets days of the week in order.

  ## Examples

      iex> get_days_of_week()
      ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
  """
  @spec get_days_of_week() :: list(String.t())
  defp get_days_of_week do
    ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
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

  @doc """
  Gets availability for a specific day/hour.

  ## Examples

      iex> get_availability(%{"monday" => %{8 => true}}, "monday", 8)
      true

      iex> get_availability(%{"monday" => %{8 => false}}, "monday", 8)
      false

      iex> get_availability(%{}, "monday", 8)
      false
  """
  @spec get_availability(map(), String.t(), integer()) :: boolean()
  defp get_availability(availability, day, hour) when is_map(availability) and is_binary(day) and is_integer(hour) do
    day_availability = Map.get(availability, day, %{})
    Map.get(day_availability, hour, false)
  end

  defp get_availability(_availability, _day, _hour) do
    false
  end
end
