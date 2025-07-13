defmodule RivaAshWeb.Components.Interactive.RecurrencePattern do
  @moduledoc """
  Recurring reservation pattern setup component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Select
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Components.Atoms.Checkbox
  import RivaAshWeb.Components.Atoms.DatePicker

  @doc """
  Renders a recurrence pattern configuration interface.
  """
  attr :pattern, :map, default: %{}
  attr :on_pattern_change, :string, required: true
  attr :on_preview, :string, default: nil
  attr :preview_dates, :list, default: []
  attr :max_occurrences, :integer, default: 365
  attr :class, :string, default: ""
  attr :rest, :global

  def recurrence_pattern(assigns) do
    ~H"""
    <!-- Recurrence pattern implementation will go here -->
    <div {@rest}>
      <div>
        <h3>Recurrence Pattern</h3>
        
        <.select
          field={:pattern_type}
          options={[
            {"Daily", "daily"},
            {"Weekly", "weekly"},
            {"Monthly", "monthly"},
            {"Custom", "custom"}
          ]}
          value={@pattern[:pattern_type] || "daily"}
          phx-change={@on_pattern_change}
        />
      </div>
      
      <div :if={@pattern[:pattern_type] == "daily"}>
        <.input
          type="number"
          field={:interval}
          label="Every X days"
          value={@pattern[:interval] || 1}
          min="1"
          max="30"
          phx-change={@on_pattern_change}
        />
      </div>
      
      <div :if={@pattern[:pattern_type] == "weekly"}>
        <.input
          type="number"
          field={:interval}
          label="Every X weeks"
          value={@pattern[:interval] || 1}
          min="1"
          max="12"
          phx-change={@on_pattern_change}
        />
        
        <div>
          <label>Days of the week:</label>
          <.checkbox field={:monday} label="Monday" phx-change={@on_pattern_change} />
          <.checkbox field={:tuesday} label="Tuesday" phx-change={@on_pattern_change} />
          <.checkbox field={:wednesday} label="Wednesday" phx-change={@on_pattern_change} />
          <.checkbox field={:thursday} label="Thursday" phx-change={@on_pattern_change} />
          <.checkbox field={:friday} label="Friday" phx-change={@on_pattern_change} />
          <.checkbox field={:saturday} label="Saturday" phx-change={@on_pattern_change} />
          <.checkbox field={:sunday} label="Sunday" phx-change={@on_pattern_change} />
        </div>
      </div>
      
      <div :if={@pattern[:pattern_type] == "monthly"}>
        <.select
          field={:monthly_type}
          options={[
            {"Same date each month", "date"},
            {"Same day of week", "day_of_week"}
          ]}
          value={@pattern[:monthly_type] || "date"}
          phx-change={@on_pattern_change}
        />
      </div>
      
      <div>
        <.date_picker
          field={:start_date}
          label="Start Date"
          value={@pattern[:start_date]}
          phx-change={@on_pattern_change}
        />
        
        <.select
          field={:end_type}
          options={[
            {"Never", "never"},
            {"After X occurrences", "count"},
            {"On specific date", "date"}
          ]}
          value={@pattern[:end_type] || "never"}
          phx-change={@on_pattern_change}
        />
        
        <.input
          :if={@pattern[:end_type] == "count"}
          type="number"
          field={:occurrence_count}
          label="Number of occurrences"
          value={@pattern[:occurrence_count] || 1}
          min="1"
          max={@max_occurrences}
          phx-change={@on_pattern_change}
        />
        
        <.date_picker
          :if={@pattern[:end_type] == "date"}
          field={:end_date}
          label="End Date"
          value={@pattern[:end_date]}
          phx-change={@on_pattern_change}
        />
      </div>
      
      <div :if={@on_preview}>
        <button phx-click={@on_preview}>Preview Dates</button>
        
        <div :if={@preview_dates != []}>
          <h4>Preview (first 10 dates):</h4>
          <ul>
            <li :for={date <- Enum.take(@preview_dates, 10)}>
              <%= date %>
            </li>
          </ul>
          <p :if={length(@preview_dates) > 10}>
            ... and <%= length(@preview_dates) - 10 %> more dates
          </p>
        </div>
      </div>
    </div>
    """
  end
end
