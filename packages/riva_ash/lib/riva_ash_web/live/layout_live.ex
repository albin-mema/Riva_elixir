defmodule RivaAshWeb.LayoutLive do
  @moduledoc """
  LiveView for managing Layouts.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Layout

  @impl true
  def mount(_params, _session, socket) do
    layouts = Layout.read!()

    socket =
      socket
      |> assign(:page_title, "Layouts")
      |> assign(:layouts, layouts)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Layouts" description="Manage various layout configurations">
        <:action>
          <.button phx-click="new_layout" class="bg-blue-600 hover:bg-blue-700">New Layout</.button>
        </:action>
      </.page_header>

      <.data_table
        id="layouts-table"
        items={@layouts}
        meta={@meta}
        path="/layouts"
      >
        <:col :let={layout} label="Name" field={:name} sortable>
          <%= layout.name %>
        </:col>
        <:col :let={layout} label="Description">
          <%= layout.description %>
        </:col>
        <:col :let={layout} label="Width">
          <%= layout.width %>
        </:col>
        <:col :let={layout} label="Height">
          <%= layout.height %>
        </:col>
        <:col :let={layout} label="Actions">
          <.button phx-click="edit_layout" phx-value-id={layout.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_layout" phx-value-id={layout.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_layout", _params, socket) do
    {:noreply, push_patch(socket, to: "/layouts/new")}
  end

  def handle_event("edit_layout", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/layouts/#{id}/edit")}
  end

  def handle_event("delete_layout", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting layout with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
