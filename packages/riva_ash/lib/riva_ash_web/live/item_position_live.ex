defmodule RivaAshWeb.ItemPositionLive do
  @moduledoc """
  LiveView for managing Item Positions.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.AllAtoms

  alias RivaAsh.Resources.ItemPosition

  @impl true
  def mount(_params, _session, socket) do
    item_positions = RivaAsh.read(ItemPosition)

    socket =
      socket
      |> assign(:page_title, "Item Positions")
      |> assign(:item_positions, item_positions)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Item Positions" description="Manage item positions within a layout">
        <:action>
          <.button phx-click="new_item_position" class="bg-blue-600 hover:bg-blue-700">New Item Position</.button>
        </:action>
      </.page_header>

      <.data_table
        id="item-positions-table"
        items={@item_positions}
        meta={@meta}
        path="/item-positions"
      >
        <:col :let={item_position} label="Item ID" field={:item_id} sortable>
          <%= item_position.item_id %>
        </:col>
        <:col :let={item_position} label="Layout ID" field={:layout_id} sortable>
          <%= item_position.layout_id %>
        </:col>
        <:col :let={item_position} label="X Coordinate">
          <%= item_position.x_coord %>
        </:col>
        <:col :let={item_position} label="Y Coordinate">
          <%= item_position.y_coord %>
        </:col>
        <:col :let={item_position} label="Actions">
          <.button phx-click="edit_item_position" phx-value-id={item_position.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_item_position" phx-value-id={item_position.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item_position", _params, socket) do
    {:noreply, push_patch(socket, to: "/item-positions/new")}
  end

  def handle_event("edit_item_position", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-positions/#{id}/edit")}
  end

  def handle_event("delete_item_position", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting item position with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
