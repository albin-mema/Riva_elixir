defmodule RivaAshWeb.Components.Interactive.TimeSlotPicker do
  @moduledoc """
  Interactive time slot picker component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a time slot picker interface.
  """
  attr :available_slots, :list, required: true
  attr :selected_slots, :list, default: []
  attr :on_slot_select, :string, required: true
  attr :on_slot_deselect, :string, required: true
  attr :multiple_selection, :boolean, default: false
  attr :duration_minutes, :integer, default: 60
  attr :show_duration, :boolean, default: true
  attr :disabled_slots, :list, default: []
  attr :class, :string, default: ""
  attr :rest, :global

  def time_slot_picker(assigns) do
    ~H"""
    <!-- Time slot picker implementation will go here -->
    <div {@rest}>
      <div :if={@show_duration}>
        <label>Duration: <%= @duration_minutes %> minutes</label>
      </div>
      
      <div>
        <div :for={slot <- @available_slots}>
          <button 
            phx-click={if slot.id in @selected_slots, do: @on_slot_deselect, else: @on_slot_select}
            phx-value-slot={slot.id}
            disabled={slot.id in @disabled_slots}
            class={[
              "time-slot",
              if(slot.id in @selected_slots, do: "selected", else: ""),
              if(slot.id in @disabled_slots, do: "disabled", else: "")
            ]}
          >
            <div>
              <span><%= slot.start_time %> - <%= slot.end_time %></span>
              <span :if={slot.price}>$<%= slot.price %></span>
            </div>
            <div :if={slot.available_count}>
              <%= slot.available_count %> available
            </div>
          </button>
        </div>
      </div>
      
      <div :if={@selected_slots != []}>
        <h4>Selected Time Slots:</h4>
        <div :for={slot_id <- @selected_slots}>
          <span><%= slot_id %></span>
          <button phx-click={@on_slot_deselect} phx-value-slot={slot_id}>Remove</button>
        </div>
      </div>
    </div>
    """
  end
end
