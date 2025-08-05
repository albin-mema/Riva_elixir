defmodule RivaAshWeb.PaymentLive do
  @moduledoc """
  LiveView for managing Payments.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Payment
  alias RivaAsh.Payment.PaymentService
  alias RivaAsh.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_payments_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}
          {:error, reason} ->
            Logger.error("Failed to load payments: #{inspect(reason)}")
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
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
          <%= format_currency(payment.amount) %>
        </:col>
        <:col :let={payment} label="Status" field={:status} sortable>
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case payment.status do
              :pending -> "bg-yellow-100 text-yellow-800"
              :completed -> "bg-green-100 text-green-800"
              :failed -> "bg-red-100 text-red-800"
              :refunded -> "bg-gray-100 text-gray-800"
              _ -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(payment.status)) %>
          </span>
        </:col>
        <:col :let={payment} label="Payment Date">
          <%= format_date(payment.inserted_at) %>
        </:col>
        <:col :let={payment} label="Actions">
          <%= if payment.status == :pending do %>
            <.button phx-click="process_payment" phx-value-id={payment.id} class="bg-green-600 hover:bg-green-700">Process</.button>
            <.button phx-click="cancel_payment" phx-value-id={payment.id} class="bg-yellow-600 hover:bg-yellow-700">Cancel</.button>
          <% end %>
          <.button phx-click="edit_payment" phx-value-id={payment.id} class="bg-blue-600 hover:bg-blue-700">Edit</.button>
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

  def handle_event("process_payment", %{"id" => id}, socket) do
    case PaymentService.process_payment(id, socket.assigns.current_user) do
      {:ok, _payment} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Payment processed successfully")
         |> reload_payments()}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Failed to process payment: #{format_error(reason)}")}
    end
  end

  def handle_event("cancel_payment", %{"id" => id}, socket) do
    case PaymentService.cancel_payment(id, socket.assigns.current_user) do
      {:ok, _payment} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Payment cancelled successfully")
         |> reload_payments()}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Failed to cancel payment: #{format_error(reason)}")}
    end
  end

  def handle_event("delete_payment", %{"id" => id}, socket) do
    case PaymentService.delete_payment(id, socket.assigns.current_user) do
      {:ok, _payment} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Payment deleted successfully")
         |> reload_payments()}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Failed to delete payment: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_payments_data(socket, user) do
    case PaymentService.get_user_payments(user) do
      {:ok, {payments, meta}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:payments, payments)
          |> assign(:meta, meta)
          |> assign(:loading, false)
        
        {:ok, socket}
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_payments(socket) do
    case PaymentService.get_user_payments(socket.assigns.current_user) do
      {:ok, {payments, meta}} ->
        assign(socket, :payments, payments)
        |> assign(:meta, meta)
      
      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :payments_page_title, "Payments")
  end

  defp format_currency(amount) when is_number(amount) do
    case Application.get_env(:riva_ash, :currency, "USD") do
      "USD" -> "$#{:erlang.float_to_binary(amount, decimals: 2)}"
      "EUR" -> "€#{:erlang.float_to_binary(amount, decimals: 2)}"
      "GBP" -> "£#{:erlang.float_to_binary(amount, decimals: 2)}"
      currency -> "#{currency}#{:erlang.float_to_binary(amount, decimals: 2)}"
    end
  end

  defp format_currency(amount), do: amount

  defp format_date(nil), do: "N/A"
  defp format_date(date) do
    case Calendar.strftime(date, "%Y-%m-%d %H:%M") do
      {:ok, formatted} -> formatted
      {:error, _} -> "Invalid date"
    end
  end

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} -> 
        errors |> Enum.map(&format_validation_error/1) |> Enum.join(", ")
      %Ash.Error.Forbidden{} -> "You don't have permission to perform this action"
      %Ash.Error.NotFound{} -> "Payment not found"
      _ -> "An unexpected error occurred"
    end
  end

  defp format_validation_error(error) do
    case error do
      {message, _} -> message
      message when is_binary(message) -> message
      _ -> "Invalid input"
    end
  end
end