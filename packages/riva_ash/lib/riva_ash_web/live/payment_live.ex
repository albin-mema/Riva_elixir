defmodule RivaAshWeb.PaymentLive do
  @moduledoc """
  Payment tracking and management LiveView.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Molecules.StatusIndicator
  import RivaAshWeb.Components.Molecules.FilterPanel

  alias RivaAsh.Resources.Payment

  @impl true
  def mount(_params, session, socket) do
    user = get_current_user_from_session(session)

    if user do
      socket =
        socket
        |> assign(:current_user, user)
        |> assign(:page_title, "Payment Management")
        |> assign(:payments, [])
        |> assign(:meta, %{})
        |> assign(:filters, %{})
        |> assign(:selected_payment, nil)

      {:ok, socket}
    else
      {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!-- Payment management implementation will go here -->
    <div>
      <.page_header title="Payment Management" description="Track and manage reservation payments">
        <:action>
          <button phx-click="record_payment">Record Payment</button>
        </:action>
      </.page_header>
      
      <.filter_panel
        filters={[
          %{type: "select", field: "status", label: "Status", options: [
            {"Pending", "pending"},
            {"Completed", "completed"},
            {"Failed", "failed"},
            {"Refunded", "refunded"}
          ]},
          %{type: "date", field: "date_from", label: "From Date"},
          %{type: "date", field: "date_to", label: "To Date"}
        ]}
        values={@filters}
        on_apply="apply_filters"
        on_clear="clear_filters"
      />
      
      <.data_table
        items={@payments}
        meta={@meta}
        path={~p"/payments"}
        id="payments-table"
      >
        <:col label="Reservation" field={:reservation} sortable>
          <%= item.reservation.client.first_name %> <%= item.reservation.client.last_name %> - 
          <%= item.reservation.item.name %>
        </:col>
        <:col label="Amount" field={:amount} sortable>
          $<%= :erlang.float_to_binary(item.amount, decimals: 2) %>
        </:col>
        <:col label="Method" field={:payment_method}>
          <%= item.payment_method %>
        </:col>
        <:col label="Status" field={:status}>
          <.status_indicator status={item.status} />
        </:col>
        <:col label="Date" field={:payment_date} sortable>
          <%= Calendar.strftime(item.payment_date, "%Y-%m-%d %H:%M") %>
        </:col>
        <:col label="Actions">
          <button phx-click="view_payment" phx-value-id={item.id}>View</button>
          <button :if={item.status == "pending"} phx-click="mark_paid" phx-value-id={item.id}>Mark Paid</button>
          <button :if={item.status == "completed"} phx-click="refund_payment" phx-value-id={item.id}>Refund</button>
        </:col>
      </.data_table>
      
      <div :if={@selected_payment}>
        <!-- Payment details modal will go here -->
        <div>
          <h3>Payment Details</h3>
          <p>Amount: $<%= :erlang.float_to_binary(@selected_payment.amount, decimals: 2) %></p>
          <p>Status: <%= @selected_payment.status %></p>
          <button phx-click="close_payment_details">Close</button>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("record_payment", _params, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("view_payment", %{"id" => id}, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("mark_paid", %{"id" => id}, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("refund_payment", %{"id" => id}, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("apply_filters", params, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("clear_filters", _params, socket) do
    {:noreply, assign(socket, :filters, %{})}
  end

  def handle_event("close_payment_details", _params, socket) do
    {:noreply, assign(socket, :selected_payment, nil)}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions will go here
  defp get_current_user_from_session(_session) do
    # Implementation will go here
    nil
  end
end
