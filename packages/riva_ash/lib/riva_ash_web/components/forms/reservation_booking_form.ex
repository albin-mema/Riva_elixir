defmodule RivaAshWeb.Components.Forms.ReservationBookingForm do
  @moduledoc """
  Multi-step reservation booking form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button

  import RivaAshWeb.Components.Atoms.DatePicker

  import RivaAshWeb.Components.Interactive.TimeSlotPicker

  @doc """
  Renders a multi-step reservation booking form.
  """
  attr(:form, :map, required: true)
  attr(:step, :integer, default: 1)
  attr(:total_steps, :integer, default: 4)
  attr(:clients, :list, default: [])
  attr(:items, :list, default: [])
  attr(:available_slots, :list, default: [])
  attr(:selected_slots, :list, default: [])
  attr(:pricing_info, :map, default: %{})
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_next_step, :string, required: true)
  attr(:on_prev_step, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:on_slot_select, :string, required: true)
  attr(:on_slot_deselect, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def reservation_booking_form(assigns) do
    ~H"""
    <!-- Reservation booking form implementation will go here -->
    <div {@rest}>
      <div>
        <h2>Book Reservation - Step <%= @step %> of <%= @total_steps %></h2>
        <div class="progress-bar">
          <div style={"width: #{@step / @total_steps * 100}%"}></div>
        </div>
      </div>

      <form phx-submit={@on_submit} phx-change={@on_change}>
        <!-- Step 1: Client Selection -->
        <div :if={@step == 1}>
          <h3>Select Client</h3>
          <.select_field field={@form[:client_id]} label="Client" options={@clients} required />
          <.form_field field={@form[:client_notes]} label="Special Requests" type="textarea" />
        </div>

        <!-- Step 2: Item Selection -->
        <div :if={@step == 2}>
          <h3>Select Item</h3>
          <.select_field field={@form[:item_id]} label="Item" options={@items} required />
          <div :if={@form[:item_id].value}>
            <!-- Item details display -->
            <div>Item details will be shown here</div>
          </div>
        </div>

        <!-- Step 3: Date & Time Selection -->
        <div :if={@step == 3}>
          <h3>Select Date & Time</h3>
          <.date_picker field={@form[:reservation_date]} required />

          <div :if={@form[:reservation_date].value}>
            <.time_slot_picker
              available_slots={@available_slots}
              selected_slots={@selected_slots}
              on_slot_select={@on_slot_select}
              on_slot_deselect={@on_slot_deselect}
            />
          </div>
        </div>

        <!-- Step 4: Confirmation & Payment -->
        <div :if={@step == 4}>
          <h3>Confirm Reservation</h3>

          <div>
            <h4>Reservation Summary</h4>
            <p>Client: <%= get_client_name(@clients, @form[:client_id].value) %></p>
            <p>Item: <%= get_item_name(@items, @form[:item_id].value) %></p>
            <p>Date: <%= @form[:reservation_date].value %></p>
            <p>Time Slots: <%= length(@selected_slots) %> selected</p>
          </div>

          <div :if={@pricing_info != %{}}>
            <h4>Pricing</h4>
            <p>Subtotal: $<%= @pricing_info.subtotal %></p>
            <p>Tax: $<%= @pricing_info.tax %></p>
            <p>Total: $<%= @pricing_info.total %></p>
          </div>

          <.select_field field={@form[:payment_method]} label="Payment Method" options={[
            {"Cash", "cash"},
            {"Credit Card", "credit_card"},
            {"Bank Transfer", "bank_transfer"}
          ]} required />
        </div>

        <div>
          <.button
            :if={@step > 1}
            type="button"
            variant="outline"
            phx-click={@on_prev_step}
          >
            Previous
          </.button>

          <.button
            :if={@step < @total_steps}
            type="button"
            phx-click={@on_next_step}
          >
            Next
          </.button>

          <.button
            :if={@step == @total_steps}
            type="submit"
            loading={@loading}
          >
            Confirm Reservation
          </.button>

          <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
        </div>
      </form>
    </div>
    """
  end

  # Helper functions
  defp get_client_name(clients, client_id) do
    case Enum.find(clients, fn {_, id} -> id == client_id end) do
      {name, _} -> name
      _ -> "Unknown"
    end
  end

  defp get_item_name(items, item_id) do
    case Enum.find(items, fn {_, id} -> id == item_id end) do
      {name, _} -> name
      _ -> "Unknown"
    end
  end
end
