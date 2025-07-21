defmodule RivaAshWeb.PaymentLive do
  @moduledoc """
  LiveView for managing Payments.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.AllAtoms

  alias RivaAsh.Resources.Payment

  @impl true
  def mount(_params, _session, socket) do
    payments = RivaAsh.read(Payment)

    socket =
      socket
      |> assign(:page_title, "Payments")
      |> assign(:payments, payments)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Payments" description="Manage payment records">
        <:action>
          <.button phx-click="new_payment" class="bg-blue-600 hover:bg-blue-700">New Payment</.button>
        </:action>
      </.page_header>

      <.data_table
        id="payments-table"
        items={@payments}
        meta={@meta}
        path="/payments"
      >
        <:col :let={payment} label="Client ID" field={:client_id} sortable>
          <%= payment.client_id %>
        </:col>
        <:col :let={payment} label="Amount">
          <%= payment.amount %>
        </:col>
        <:col :let={payment} label="Status" field={:status} sortable>
          <%= payment.status %>
        </:col>
        <:col :let={payment} label="Actions">
          <.button phx-click="edit_payment" phx-value-id={payment.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_payment" phx-value-id={payment.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_payment", _params, socket) do
    {:noreply, push_patch(socket, to: "/payments/new")}
  end

  def handle_event("edit_payment", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/payments/#{id}/edit")}
  end

  def handle_event("delete_payment", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting payment with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
