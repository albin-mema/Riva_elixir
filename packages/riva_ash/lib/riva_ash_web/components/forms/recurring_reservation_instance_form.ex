defmodule RivaAshWeb.Components.Forms.RecurringReservationInstanceForm do
  @moduledoc """
  RecurringReservationInstanceForm organism component for creating and editing recurring reservation instances.
  Uses atomic design components to provide a consistent form experience.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Molecules.FormField

  @doc """
  Renders a recurring reservation instance form for creating or editing instances.

  ## Examples

      <.recurring_reservation_instance_form
        form={@form}
        editing={@editing_instance}
        loading={@loading}
        recurring_reservations={@recurring_reservations}
        on_submit="save_instance"
        on_change="validate_instance"
        on_cancel="cancel_form"
      />
  """
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:loading, :boolean, default: false)
  attr(:recurring_reservations, :list, default: [])
  attr(:reservations, :list, default: [])
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:class, :string, default: "")

  def recurring_reservation_instance_form(assigns) do
    ~H"""
    <.card variant="elevated" class={@class}>
      <:header>
        <.card_title>
          <%= if @editing, do: "Edit Instance", else: "Add New Instance" %>
        </.card_title>
        <.card_description>
          <%= if @editing do %>
            Update the recurring reservation instance information below.
          <% else %>
            Fill in the details to add a new recurring reservation instance.
          <% end %>
        </.card_description>
      </:header>

      <:body>
        <.form
          for={@form}
          phx-submit={@on_submit}
          phx-change={@on_change}
          class="space-y-6"
        >
          <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
            <.form_field
              field={@form[:scheduled_date]}
              label="Scheduled Date"
              type="date"
              required={true}
            />

            <.form_field
              field={@form[:sequence_number]}
              label="Sequence Number"
              type="number"
              required={true}
              helper_text="The sequence number in the recurring pattern (1, 2, 3, etc.)"
            />
          </div>

          <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
            <.form_field
              field={@form[:status]}
              label="Status"
              type="select"
              options={[
                "pending": "Pending",
                "confirmed": "Confirmed",
                "failed": "Failed",
                "skipped": "Skipped",
                "cancelled": "Cancelled"
              ]}
              prompt="Select a status"
              required={true}
            />

            <%= if @recurring_reservations != [] do %>
              <.form_field
                field={@form[:recurring_reservation_id]}
                label="Recurring Reservation"
                type="select"
                options={Enum.map(@recurring_reservations, &{"Reservation ##{&1.id}", &1.id})}
                prompt="Select a recurring reservation"
                required={true}
              />
            <% end %>
          </div>

          <.form_field
            field={@form[:notes]}
            label="Notes"
            type="textarea"
            placeholder="Additional notes about this instance..."
            helper_text="Optional notes or comments"
          />

          <.form_field
            field={@form[:skip_reason]}
            label="Skip Reason"
            type="textarea"
            placeholder="Reason for skipping this instance..."
            helper_text="Only applicable if status is 'skipped'"
          />

          <%= if @editing do %>
            <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
              <.form_field
                field={@form[:error_message]}
                label="Error Message"
                type="textarea"
                placeholder="Error message if reservation creation failed..."
                helper_text="Only applicable if status is 'failed'"
              />

              <.form_field
                field={@form[:created_at]}
                label="Created At"
                type="datetime-local"
                helper_text="When the actual reservation was created for this instance"
              />
            </div>

            <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
              <.form_field
                field={@form[:failed_at]}
                label="Failed At"
                type="datetime-local"
                helper_text="When this instance failed to create a reservation"
              />

              <%= if @reservations != [] do %>
                <.form_field
                  field={@form[:reservation_id]}
                  label="Reservation"
                  type="select"
                  options={[{"No reservation", ""} | Enum.map(@reservations, &{"Reservation ##{&1.id}", &1.id})]}
                  prompt="Select a reservation"
                />
              <% end %>
            </div>
          <% end %>

          <div class="flex justify-end space-x-3 pt-4 border-t">
            <.button
              type="button"
              variant="outline"
              phx-click={@on_cancel}
              disabled={@loading}
            >
              Cancel
            </.button>

            <.button
              type="submit"
              loading={@loading}
            >
              <%= if @editing, do: "Update Instance", else: "Add Instance" %>
            </.button>
          </div>
        </.form>
      </:body>
    </.card>
    """
  end
end
