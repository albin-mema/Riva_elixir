defmodule RivaAshWeb.Components.Forms.PaymentForm do
  @moduledoc """
  Payment processing form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Select

  @doc """
  Renders a payment form.
  """
  attr :form, :map, required: true
  attr :reservation, :map, required: true
  attr :payment_methods, :list, default: []
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def payment_form(assigns) do
    ~H"""
    <!-- Payment form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <div>
        <h3>Payment Details</h3>

        <div>
          <h4>Reservation Summary</h4>
          <p>Client: <%= @reservation.client_name %></p>
          <p>Item: <%= @reservation.item_name %></p>
          <p>Date: <%= @reservation.date %></p>
          <p>Amount: $<%= @reservation.total_amount %></p>
        </div>
      </div>

      <.form_field
        field={@form[:payment_method]}
        label="Payment Method"
        type="select"
        options={Enum.map(@payment_methods, &{Atom.to_string(&1) |> String.capitalize(), &1})}
        required
      />

      <div :if={@form[:payment_method].value == "credit_card"}>
        <h4>Credit Card Information</h4>
        <.form_field field={@form[:card_number]} label="Card Number" type="text" required />
        <.form_field field={@form[:card_holder_name]} label="Cardholder Name" type="text" required />

        <div style="display: flex; gap: 1rem;">
          <.form_field field={@form[:expiry_month]} label="Month" type="select" options={[
            {"01", "01"}, {"02", "02"}, {"03", "03"}, {"04", "04"},
            {"05", "05"}, {"06", "06"}, {"07", "07"}, {"08", "08"},
            {"09", "09"}, {"10", "10"}, {"11", "11"}, {"12", "12"}
          ]} required />
          <.form_field field={@form[:expiry_year]} label="Year" type="select" options={
            for year <- 2024..2034, do: {to_string(year), to_string(year)}
          } required />
          <.form_field field={@form[:cvv]} label="CVV" type="text" required />
        </div>
      </div>

      <div :if={@form[:payment_method].value == "bank_transfer"}>
        <h4>Bank Transfer Information</h4>
        <.form_field field={@form[:bank_account]} label="Bank Account" type="text" required />
        <.form_field field={@form[:routing_number]} label="Routing Number" type="text" required />
      </div>

      <div :if={@form[:payment_method].value == "cash"}>
        <h4>Cash Payment</h4>
        <.form_field field={@form[:amount_received]} label="Amount Received" type="number" step="0.01" required />
        <.form_field field={@form[:change_given]} label="Change Given" type="number" step="0.01" readonly />
      </div>

      <.form_field field={@form[:notes]} label="Payment Notes" type="textarea" />

      <div>
        <.button type="submit" variant="primary" loading={@loading}>Process Payment</.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
