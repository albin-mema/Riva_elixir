defmodule RivaAshWeb.Components.Forms.ScheduleForm do
  @moduledoc """
  Item schedule configuration form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Select
  import RivaAshWeb.Components.Atoms.DatePicker
  import RivaAshWeb.Components.Atoms.TimePicker
  import RivaAshWeb.Components.Interactive.AvailabilityGrid

  @doc """
  Renders an item schedule configuration form.
  """
  attr :form, :map, required: true
  attr :item, :map, required: true
  attr :availability, :map, default: %{}
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_availability_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def schedule_form(assigns) do
    ~H"""
    <!-- Schedule form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <div>
        <h3>Schedule for <%= @item.name %></h3>
      </div>
      
      <div>
        <h4>General Settings</h4>
        <.form_field field={@form[:schedule_type]} label="Schedule Type" type="select" options={[
          {"Always Available", "always"},
          {"Custom Schedule", "custom"},
          {"Seasonal", "seasonal"}
        ]} required />
      </div>
      
      <div :if={@form[:schedule_type].value == "custom"}>
        <h4>Weekly Availability</h4>
        <.availability_grid
          availability={@availability}
          on_slot_toggle={@on_availability_change}
          on_bulk_action={@on_availability_change}
          start_hour={8}
          end_hour={18}
          editable={true}
        />
      </div>
      
      <div :if={@form[:schedule_type].value == "seasonal"}>
        <h4>Seasonal Settings</h4>
        <.date_picker field={@form[:season_start]} label="Season Start Date" />
        <.date_picker field={@form[:season_end]} label="Season End Date" />
      </div>
      
      <div>
        <h4>Booking Rules</h4>
        <.form_field field={@form[:advance_booking_days]} label="Advance Booking (days)" type="number" min="0" />
        <.form_field field={@form[:max_booking_duration]} label="Max Booking Duration (hours)" type="number" min="1" />
        <.form_field field={@form[:min_booking_duration]} label="Min Booking Duration (hours)" type="number" min="1" />
      </div>
      
      <div>
        <h4>Time Slots</h4>
        <.form_field field={@form[:slot_duration]} label="Slot Duration (minutes)" type="select" options={[
          {"15 minutes", "15"},
          {"30 minutes", "30"},
          {"1 hour", "60"},
          {"2 hours", "120"},
          {"4 hours", "240"}
        ]} />
        
        <.time_picker field={@form[:earliest_start_time]} label="Earliest Start Time" />
        <.time_picker field={@form[:latest_end_time]} label="Latest End Time" />
      </div>
      
      <div>
        <h4>Exceptions</h4>
        <.form_field field={@form[:holiday_schedule]} label="Holiday Schedule" type="select" options={[
          {"Follow regular schedule", "regular"},
          {"Closed on holidays", "closed"},
          {"Custom holiday hours", "custom"}
        ]} />
      </div>
      
      <div>
        <.button type="submit" loading={@loading}>Save Schedule</.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
