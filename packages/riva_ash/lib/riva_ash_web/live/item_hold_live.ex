defmodule RivaAshWeb.ItemHoldLive do
  @moduledoc """
  LiveView for managing Item Holds.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.ItemHold

  @impl true
  def mount(_params, _session, socket) do
    item_holds = ItemHold.read!()

    socket =
      socket
      |> assign(:page_title, "Item Holds")
      |> assign(:item_holds, item_holds)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
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
          <%= item_hold.start_time %>
        </:col>
        <:col :let={item_hold} label="End Time">
          <%= item_hold.end_time %>
        </:col>
        <:col :let={item_hold} label="Actions">
          <.button phx-click="edit_item_hold" phx-value-id={item_hold.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
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

  def handle_event("delete_item_hold", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting item hold with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
