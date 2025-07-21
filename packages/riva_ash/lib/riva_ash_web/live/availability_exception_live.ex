defmodule RivaAshWeb.AvailabilityExceptionLive do
  @moduledoc """
  LiveView for managing Availability Exceptions.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.AllAtoms

  alias RivaAsh.Resources.AvailabilityException

  @impl true
  def mount(_params, _session, socket) do
    exceptions = RivaAsh.read(AvailabilityException)

    socket =
      socket
      |> assign(:page_title, "Availability Exceptions")
      |> assign(:exceptions, exceptions)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Availability Exceptions" description="Manage exceptions for item availability">
        <:action>
          <.button phx-click="new_exception" class="bg-blue-600 hover:bg-blue-700">New Exception</.button>
        </:action>
      </.page_header>

      <.data_table
        id="availability-exceptions-table"
        items={@exceptions}
        meta={@meta}
        path="/availability-exceptions"
      >
        <:col :let={exception} label="Item ID" field={:item_id} sortable>
          <%= exception.item_id %>
        </:col>
        <:col :let={exception} label="Start Date">
          <%= exception.start_date %>
        </:col>
        <:col :let={exception} label="End Date">
          <%= exception.end_date %>
        </:col>
        <:col :let={exception} label="Reason">
          <%= exception.reason %>
        </:col>
        <:col :let={exception} label="Actions">
          <.button phx-click="edit_exception" phx-value-id={exception.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_exception" phx-value-id={exception.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_exception", _params, socket) do
    {:noreply, push_patch(socket, to: "/availability-exceptions/new")}
  end

  def handle_event("edit_exception", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/availability-exceptions/#{id}/edit")}
  end

  def handle_event("delete_exception", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting availability exception with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
