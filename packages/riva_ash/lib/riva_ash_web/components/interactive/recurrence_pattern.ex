defmodule RivaAshWeb.Components.Interactive.RecurrencePattern do
  @moduledoc """
  Recurring reservation pattern setup component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField

  @doc """
  Renders a recurrence pattern configuration interface.
  """
  attr(:pattern, :map, default: %{})
  attr(:on_pattern_change, :string, required: true)
  attr(:on_preview, :string, default: nil)
  attr(:preview_dates, :list, default: [])
  attr(:max_occurrences, :integer, default: 365)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def recurrence_pattern(assigns) do
    ~H"""
    <!-- Recurrence pattern implementation will go here -->
    <div {@rest}>
      <div>
        <h3>Recurrence Pattern</h3>

        <.select_field
          field={@pattern[:form][:pattern_type]}
          label="Pattern Type"
          options={[
            {"Daily", "daily"},
            {"Weekly", "weekly"},
            {"Monthly", "monthly"},
            {"Custom", "custom"}
          ]}
          prompt="Select pattern type"
        />
      </div>

      <div :if={@pattern[:pattern_type] == "daily"}>
        <.form_field
          field={@pattern[:form][:interval]}
          label="Every X days"
          type="number"
        />
      </div>

      <div :if={@pattern[:pattern_type] == "weekly"}>
        <.form_field
          field={@pattern[:form][:interval]}
          label="Every X weeks"
          type="number"
        />

        <div>
          <label>Days of the week:</label>
          <.form_field field={@pattern[:form][:monday]} label="Monday" type="checkbox" />
          <.form_field field={@pattern[:form][:tuesday]} label="Tuesday" type="checkbox" />
          <.form_field field={@pattern[:form][:wednesday]} label="Wednesday" type="checkbox" />
          <.form_field field={@pattern[:form][:thursday]} label="Thursday" type="checkbox" />
          <.form_field field={@pattern[:form][:friday]} label="Friday" type="checkbox" />
          <.form_field field={@pattern[:form][:saturday]} label="Saturday" type="checkbox" />
          <.form_field field={@pattern[:form][:sunday]} label="Sunday" type="checkbox" />
        </div>
      </div>

      <div :if={@pattern[:pattern_type] == "monthly"}>
        <.select_field
          field={@pattern[:form][:monthly_type]}
          label="Monthly Type"
          options={[
            {"Same date each month", "date"},
            {"Same day of week", "day_of_week"}
          ]}
        />
      </div>

      <div>
        <.form_field field={@pattern[:form][:start_date]} label="Start Date" type="date" />

        <.select_field
          field={@pattern[:form][:end_type]}
          label="End Type"
          options={[
            {"Never", "never"},
            {"After X occurrences", "count"},
            {"On specific date", "date"}
          ]}
        />

        <.form_field
          :if={@pattern[:end_type] == "count"}
          field={@pattern[:form][:occurrence_count]}
          label="Number of occurrences"
          type="number"
        />

        <.form_field
          :if={@pattern[:end_type] == "date"}
          field={@pattern[:form][:end_date]}
          label="End Date"
          type="date"
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
