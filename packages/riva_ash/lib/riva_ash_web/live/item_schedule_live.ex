defmodule RivaAshWeb.ItemScheduleLive do
  @moduledoc """
  LiveView for managing Item Schedules.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.ItemSchedule

  @impl true
  def mount(_params, session, socket) do
    mount_business_scoped(socket, session, ItemSchedule, [:item, :business_id], "Item Schedules")
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Item Schedules" description="Manage item scheduling">
        <:action>
          <.button phx-click="new_item_schedule" class="bg-blue-600 hover:bg-blue-700">New Item Schedule</.button>
        </:action>
      </.page_header>

      <.data_table
        id="item-schedules-table"
        items={@item_schedules}
        meta={@meta}
        path="/item-schedules"
      >
        <:col :let={item_schedule} label="Item ID" field={:item_id} sortable>
          <%= item_schedule.item_id %>
        </:col>
        <:col :let={item_schedule} label="Start Time">
          <%= item_schedule.start_time %>
        </:col>
        <:col :let={item_schedule} label="End Time">
          <%= item_schedule.end_time %>
        </:col>
        <:col :let={item_schedule} label="Day of Week">
          <%= item_schedule.day_of_week %>
        </:col>
        <:col :let={item_schedule} label="Actions">
          <.button phx-click="edit_item_schedule" phx-value-id={item_schedule.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_item_schedule" phx-value-id={item_schedule.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item_schedule", _params, socket) do
    {:noreply, push_patch(socket, to: "/item-schedules/new")}
  end

  def handle_event("edit_item_schedule", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-schedules/#{id}/edit")}
  end

  def handle_event("delete_item_schedule", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting item schedule with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
