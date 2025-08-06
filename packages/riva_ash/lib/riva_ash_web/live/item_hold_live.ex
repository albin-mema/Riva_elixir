defmodule RivaAshWeb.ItemHoldLive do
  @moduledoc """
  LiveView for managing Item Holds.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.ItemHold
  alias RivaAsh.Hold.HoldService
  alias RivaAsh.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_item_holds_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}

          {:error, reason} ->
            Logger.error("Failed to load item holds: #{inspect(reason)}")
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
      <.page_header title="Item Holds" description="Manage temporary holds on items">
        <:action>
          <.button phx-click="new_item_hold" class="bg-blue-600 hover:bg-blue-700">New Item Hold</.button>
        </:action>
      </.page_header>

      <.data_table
        id="item-holds-table"
        items={@item_holds}
        meta={@meta}
        path="/item-holds"
      >
        <:col :let={item_hold} label="Item ID" field={:item_id} sortable>
          <%= item_hold.item_id %>
        </:col>
        <:col :let={item_hold} label="Client ID" field={:client_id} sortable>
          <%= item_hold.client_id %>
        </:col>
        <:col :let={item_hold} label="Start Time">
          <%= format_datetime(item_hold.start_time) %>
        </:col>
        <:col :let={item_hold} label="End Time">
          <%= format_datetime(item_hold.end_time) %>
        </:col>
        <:col :let={item_hold} label="Status">
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case item_hold.status do
              :active -> "bg-green-100 text-green-800"
              :expired -> "bg-red-100 text-red-800"
              :cancelled -> "bg-gray-100 text-gray-800"
              _ -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(item_hold.status)) %>
          </span>
        </:col>
        <:col :let={item_hold} label="Actions">
          <%= if item_hold.status == :active do %>
            <.button phx-click="edit_item_hold" phx-value-id={item_hold.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
            <.button phx-click="cancel_item_hold" phx-value-id={item_hold.id} class="bg-yellow-600 hover:bg-yellow-700">Cancel</.button>
          <% else %>
            <.button phx-click="view_item_hold" phx-value-id={item_hold.id} variant="secondary">View</.button>
          <% end %>
          <.button phx-click="delete_item_hold" phx-value-id={item_hold.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item_hold", _params, socket) do
    {:noreply, push_patch(socket, to: "/item-holds/new")}
  end

  def handle_event("edit_item_hold", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-holds/#{id}/edit")}
  end

  def handle_event("cancel_item_hold", %{"id" => id}, socket) do
    case HoldService.cancel_hold(id, socket.assigns.current_user) do
      {:ok, _item_hold} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item hold cancelled successfully")
         |> reload_item_holds()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to cancel item hold: #{format_error(reason)}")}
    end
  end

  def handle_event("delete_item_hold", %{"id" => id}, socket) do
    case HoldService.delete_hold(id, socket.assigns.current_user) do
      {:ok, _item_hold} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item hold deleted successfully")
         |> reload_item_holds()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete item hold: #{format_error(reason)}")}
    end
  end

  def handle_event("view_item_hold", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-holds/#{id}")}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_item_holds_data(socket, user) do
    case HoldService.get_user_holds(user) do
      {:ok, {item_holds, meta}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:item_holds, item_holds)
          |> assign(:meta, meta)
          |> assign(:loading, false)

        {:ok, socket}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_item_holds(socket) do
    case HoldService.get_user_holds(socket.assigns.current_user) do
      {:ok, {item_holds, meta}} ->
        assign(socket, :item_holds, item_holds)
        |> assign(:meta, meta)

      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :item_holds_page_title, "Item Holds")
  end

  defp format_datetime(nil), do: "N/A"

  defp format_datetime(datetime) do
    case DateTime.from_naive(datetime, "Etc/UTC") do
      {:ok, datetime} ->
        Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")

      {:error, _} ->
        "Invalid date"
    end
  end

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} ->
        Enum.map_join(errors, ", ", &format_validation_error/1)

      %Ash.Error.Forbidden{} ->
        "You don't have permission to perform this action"

      %Ash.Error.NotFound{} ->
        "Item hold not found"

      _ ->
        "An unexpected error occurred"
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
