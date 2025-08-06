defmodule RivaAshWeb.Components.Interactive.TimeSlotPicker do
  @moduledoc """
  Interactive time slot picker component.

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
  <.time_slot_picker
    available_slots={@available_slots}
    on_slot_select="select_slot"
    on_slot_deselect="deselect_slot"
  />

  # With multiple selection and duration
  <.time_slot_picker
    available_slots={@available_slots}
    selected_slots={@selected_slots}
    on_slot_select="select_slot"
    on_slot_deselect="deselect_slot"
    multiple_selection={true}
    duration_minutes={30}
    show_duration={true}
    disabled_slots={@disabled_slots}
  />
  """
  use Phoenix.Component

  @doc """
  Renders a time slot picker interface.

  ## Attributes

  - `available_slots` (list, required): List of available time slots
  - `selected_slots` (list, optional): List of selected slot IDs
  - `on_slot_select` (string, required): Event handler for slot selection
  - `on_slot_deselect` (string, required): Event handler for slot deselection
  - `multiple_selection` (boolean, default: false): Whether to allow multiple selections
  - `duration_minutes` (integer, default: 60): Duration of each slot in minutes
  - `show_duration` (boolean, default: true): Whether to show duration label
  - `disabled_slots` (list, optional): List of disabled slot IDs
  - `class` (string, default: ""): Additional CSS classes
  - `rest` (global): Additional HTML attributes
  """
  @spec time_slot_picker(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:available_slots, :list, required: true)
  attr(:selected_slots, :list, default: [])
  attr(:on_slot_select, :string, required: true)
  attr(:on_slot_deselect, :string, required: true)
  attr(:multiple_selection, :boolean, default: false)
  attr(:duration_minutes, :integer, default: 60)
  attr(:show_duration, :boolean, default: true)
  attr(:disabled_slots, :list, default: [])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @doc """
  Renders the time slot picker component.

  ## Examples

      iex> time_slot_picker(%{
      ...>   available_slots: [%{id: 1, start_time: "09:00", end_time: "10:00"}],
      ...>   on_slot_select: "select_slot",
      ...>   on_slot_deselect: "deselect_slot"
      ...> })
      %Phoenix.LiveView.Rendered{...}
  """
  @spec time_slot_picker(map()) :: Phoenix.LiveView.Rendered.t()
  def time_slot_picker(assigns) do
    assigns
    |> validate_assigns()
    |> render_picker()
  end

  # Private functions

  @doc """
  Validates component assigns.

  ## Examples

      iex> validate_assigns(%{available_slots: [], on_slot_select: "select", on_slot_deselect: "deselect"})
      {:ok, %{available_slots: [], on_slot_select: "select", on_slot_deselect: "deselect"}}

      iex> validate_assigns(%{available_slots: []})
      {:error, "on_slot_select is required"}
  """
  @spec validate_assigns(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with {:ok, _} <- validate_required(assigns, [:available_slots, :on_slot_select, :on_slot_deselect]),
         {:ok, _} <- validate_slot_data(assigns.available_slots),
         {:ok, _} <- validate_selected_slots(assigns.selected_slots),
         {:ok, _} <- validate_disabled_slots(assigns.disabled_slots),
         {:ok, _} <- validate_duration(assigns.duration_minutes) do
      {:ok, assigns}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Validates slot data.

  ## Examples

      iex> validate_slot_data([%{id: 1, start_time: "09:00", end_time: "10:00"}])
      {:ok, [%{id: 1, start_time: "09:00", end_time: "10:00"}]}

      iex> validate_slot_data([%{invalid: "data"}])
      {:error, "Invalid slot data"}

      iex> validate_slot_data("invalid")
      {:error, "available_slots must be a list"}
  """
  @spec validate_slot_data(list(map())) :: {:ok, list(map())} | {:error, String.t()}
  defp validate_slot_data(slots) when is_list(slots) do
    case Enum.find(slots, fn slot -> not valid_slot?(slot) end) do
      nil -> {:ok, slots}
      _ -> {:error, "Invalid slot data"}
    end
  end

  defp validate_slot_data(_slots) do
    {:error, "available_slots must be a list"}
  end

  @doc """
  Checks if slot data is valid.

  ## Examples

      iex> is_valid_slot(%{id: 1, start_time: "09:00", end_time: "10:00"})
      true

      iex> is_valid_slot(%{id: 1, start_time: "09:00"})
      false

      iex> is_valid_slot("invalid")
      false
  """
  @spec valid_slot?(map()) :: boolean()
  defp valid_slot?(slot) when is_map(slot) do
    Map.has_key?(slot, :id) and
      Map.has_key?(slot, :start_time) and
      Map.has_key?(slot, :end_time) and
      is_integer(slot.id) and
      slot.id > 0
  end

  defp valid_slot?(_slot) do
    false
  end

  @doc """
  Validates selected slots.

  ## Examples

      iex> validate_selected_slots([1, 2, 3])
      {:ok, [1, 2, 3]}

      iex> validate_selected_slots(["invalid"])
      {:error, "Invalid slot ID in selected_slots"}

      iex> validate_selected_slots("invalid")
      {:error, "selected_slots must be a list"}
  """
  @spec validate_selected_slots(list()) :: {:ok, list()} | {:error, String.t()}
  defp validate_selected_slots(selected_slots) when is_list(selected_slots) do
    case Enum.find(selected_slots, fn slot_id -> not valid_slot_id?(slot_id) end) do
      nil -> {:ok, selected_slots}
      invalid_id -> {:error, "Invalid slot ID in selected_slots: #{inspect(invalid_id)}"}
    end
  end

  defp validate_selected_slots(_selected_slots) do
    {:error, "selected_slots must be a list"}
  end

  @doc """
  Validates disabled slots.

  ## Examples

      iex> validate_disabled_slots([1, 2, 3])
      {:ok, [1, 2, 3]}

      iex> validate_disabled_slots(["invalid"])
      {:error, "Invalid slot ID in disabled_slots"}

      iex> validate_disabled_slots("invalid")
      {:error, "disabled_slots must be a list"}
  """
  @spec validate_disabled_slots(list()) :: {:ok, list()} | {:error, String.t()}
  defp validate_disabled_slots(disabled_slots) when is_list(disabled_slots) do
    case Enum.find(disabled_slots, fn slot_id -> not valid_slot_id?(slot_id) end) do
      nil -> {:ok, disabled_slots}
      invalid_id -> {:error, "Invalid slot ID in disabled_slots: #{inspect(invalid_id)}"}
    end
  end

  defp validate_disabled_slots(_disabled_slots) do
    {:error, "disabled_slots must be a list"}
  end

  @doc """
  Checks if slot ID is valid.

  ## Examples

      iex> is_valid_slot_id(1)
      true

      iex> is_valid_slot_id(0)
      false

      iex> is_valid_slot_id(-1)
      false

      iex> is_valid_slot_id("invalid")
      false
  """
  @spec valid_slot_id?(any()) :: boolean()
  defp valid_slot_id?(slot_id) when is_integer(slot_id) do
    slot_id > 0
  end

  defp valid_slot_id?(_slot_id) do
    false
  end

  @doc """
  Validates duration.

  ## Examples

      iex> validate_duration(60)
      {:ok, 60}

      iex> validate_duration(0)
      {:error, "duration_minutes must be greater than 0"}

      iex> validate_duration(-1)
      {:error, "duration_minutes must be greater than 0"}
  """
  @spec validate_duration(integer()) :: {:ok, integer()} | {:error, String.t()}
  defp validate_duration(duration) when is_integer(duration) do
    if duration > 0 do
      {:ok, duration}
    else
      {:error, "duration_minutes must be greater than 0"}
    end
  end

  defp validate_duration(_duration) do
    {:error, "duration_minutes must be an integer"}
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
  Renders the picker component.

  ## Examples

      iex> render_picker(%{available_slots: [], on_slot_select: "select", on_slot_deselect: "deselect"})
      %Phoenix.LiveView.Rendered{...}
  """
  @spec render_picker(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_picker(assigns) do
    ~H"""
    <div class={["time-slot-picker", @class]} {@rest}>
      <div :if={@show_duration} class="duration-label">
        <label>Duration: <%= @duration_minutes %> minutes</label>
      </div>

      <div class="slots-container">
        <div :for={slot <- @available_slots} class="slot-wrapper">
          <button
            phx-click={if slot_selected?(slot.id, @selected_slots), do: @on_slot_deselect, else: @on_slot_select}
            phx-value-slot={slot.id}
            disabled={slot_disabled?(slot.id, @disabled_slots)}
            class={[
              "time-slot",
              if(slot_selected?(slot.id, @selected_slots), do: "selected", else: ""),
              if(slot_disabled?(slot.id, @disabled_slots), do: "disabled", else: "")
            ]}
          >
            <div class="slot-time">
              <span><%= slot.start_time %> - <%= slot.end_time %></span>
              <span :if={slot.price} class="slot-price">$<%= slot.price %></span>
            </div>
            <div :if={slot.available_count} class="slot-availability">
              <%= slot.available_count %> available
            </div>
          </button>
        </div>
      </div>

      <div :if={not Enum.empty?(@selected_slots)} class="selected-slots">
        <h4>Selected Time Slots:</h4>
        <div class="selected-slots-list">
          <div :for={slot_id <- @selected_slots} class="selected-slot-item">
            <span><%= get_slot_display(slot_id, @available_slots) %></span>
            <button phx-click={@on_slot_deselect} phx-value-slot={slot_id} class="remove-button">Remove</button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  Checks if a slot is selected.

  ## Examples

      iex> is_slot_selected(1, [1, 2, 3])
      true

      iex> is_slot_selected(4, [1, 2, 3])
      false
  """
  @spec slot_selected?(integer(), list(integer())) :: boolean()
  defp slot_selected?(slot_id, selected_slots) when is_integer(slot_id) and is_list(selected_slots) do
    slot_id in selected_slots
  end

  defp slot_selected?(_slot_id, _selected_slots) do
    false
  end

  @doc """
  Checks if a slot is disabled.

  ## Examples

      iex> is_slot_disabled(1, [1, 2, 3])
      true

      iex> is_slot_disabled(4, [1, 2, 3])
      false
  """
  @spec slot_disabled?(integer(), list(integer())) :: boolean()
  defp slot_disabled?(slot_id, disabled_slots) when is_integer(slot_id) and is_list(disabled_slots) do
    slot_id in disabled_slots
  end

  defp slot_disabled?(_slot_id, _disabled_slots) do
    false
  end

  @doc """
  Gets slot display text.

  ## Examples

      iex> get_slot_display(1, [%{id: 1, start_time: "09:00", end_time: "10:00"}])
      "09:00 - 10:00"

      iex> get_slot_display(999, [%{id: 1, start_time: "09:00", end_time: "10:00"}])
      "Unknown Slot"
  """
  @spec get_slot_display(integer(), list(map())) :: String.t()
  defp get_slot_display(slot_id, available_slots) when is_integer(slot_id) and is_list(available_slots) do
    case Enum.find(available_slots, &(&1.id == slot_id)) do
      %{start_time: start_time, end_time: end_time} -> "#{start_time} - #{end_time}"
      _ -> "Unknown Slot"
    end
  end

  defp get_slot_display(_slot_id, _available_slots) do
    "Unknown Slot"
  end
end
