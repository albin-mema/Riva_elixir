defmodule RivaAshWeb.Components.Forms.ReservationForm do
  @moduledoc """
  Reservation form component using atomic design system.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.DatePicker
  import RivaAshWeb.Components.Atoms.TimePicker

  @doc """
  Renders a reservation form for creating or editing reservations.
  """
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:loading, :boolean, default: false)
  attr(:clients, :list, default: [])
  attr(:items, :list, default: [])
  attr(:employees, :list, default: [])
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def reservation_form(assigns) do
    ~H"""
    <.card variant="elevated" {@rest}>
      <:body>
        <form phx-submit={@on_submit} phx-change={@on_change} class="space-y-6">
          <.select_field
            field={@form[:client_id]}
            label="Client"
            options={@clients}
            required
            helper_text="Select the client for this reservation"
          />

          <.select_field
            field={@form[:item_id]}
            label="Item"
            options={@items}
            required
            helper_text="Select the item to reserve"
          />

          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <.date_picker
              field={@form[:reserved_from]}
              required
            />

            <.time_picker
              field={@form[:reserved_from]}
              required
            />
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <.date_picker
              field={@form[:reserved_until]}
              required
            />

            <.time_picker
              field={@form[:reserved_until]}
              required
            />
          </div>

          <.select_field
            field={@form[:employee_id]}
            label="Employee"
            options={@employees}
            helper_text="Select the employee handling this reservation (optional)"
          />

          <.form_field
            field={@form[:notes]}
            label="Notes"
            type="textarea"
            helper_text="Add any special notes for this reservation"
          />

          <div class="flex justify-end space-x-3 pt-4 border-t">
            <.button type="button" variant="outline" phx-click={@on_cancel} disabled={@loading}>
              Cancel
            </.button>

            <.button type="submit" loading={@loading}>
              <%= if @editing, do: "Update Reservation", else: "Create Reservation" %>
            </.button>
          </div>
        </form>
      </:body>
    </.card>
    """
  end
end
