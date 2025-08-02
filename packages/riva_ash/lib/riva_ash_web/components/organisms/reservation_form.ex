defmodule RivaAshWeb.Components.Organisms.ReservationForm do
  @moduledoc """
  Complex reservation creation form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.DatePicker
  import RivaAshWeb.Components.Atoms.TimePicker

  @doc """
  Renders a reservation form with all necessary fields.
  """
  attr(:form, :map, required: true)
  attr(:clients, :list, default: [])
  attr(:items, :list, default: [])
  attr(:employees, :list, default: [])
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:step, :integer, default: 1)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def reservation_form(assigns) do
    ~H"""
    <!-- Reservation form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <div :if={@step == 1}>
        <!-- Step 1: Basic info -->
        <.select_field field={@form[:client_id]} label="Client" options={Enum.map(@clients, &{&1.name, &1.id})} />
        <.select_field field={@form[:item_id]} label="Item" options={Enum.map(@items, &{&1.name, &1.id})} />
      </div>

      <div :if={@step == 2}>
        <!-- Step 2: Date/Time -->
        <.date_picker field={@form[:reserved_date]} placeholder="Reservation Date" />
        <.time_picker field={@form[:start_time]} placeholder="Start Time" />
        <.time_picker field={@form[:end_time]} placeholder="End Time" />
      </div>

      <div>
        <.button type="submit" variant="primary" loading={@loading}>Create Reservation</.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
