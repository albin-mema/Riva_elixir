defmodule RivaAshWeb.ItemTypeLive do
  @moduledoc """
  LiveView for managing Item Types.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.ItemType

  @impl true
  def mount(_params, session, socket) do
    mount_business_scoped(socket, session, ItemType, :business_id, "Item Types")
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Item Types" description="Categorize and manage types of items">
        <:action>
          <.button phx-click="new_item_type" class="bg-blue-600 hover:bg-blue-700">New Item Type</.button>
        </:action>
      </.page_header>

      <.data_table
        id="item-types-table"
        items={@item_types}
        meta={@meta}
        path="/item-types"
      >
        <:col :let={item_type} label="Name" field={:name} sortable>
          <%= item_type.name %>
        </:col>
        <:col :let={item_type} label="Description">
          <%= item_type.description %>
        </:col>
        <:col :let={item_type} label="Actions">
          <.button phx-click="edit_item_type" phx-value-id={item_type.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_item_type" phx-value-id={item_type.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item_type", _params, socket) do
    {:noreply, push_patch(socket, to: "/item-types/new")}
  end

  def handle_event("edit_item_type", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-types/#{id}/edit")}
  end

  def handle_event("delete_item_type", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting item type with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
