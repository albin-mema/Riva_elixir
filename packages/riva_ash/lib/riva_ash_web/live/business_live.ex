defmodule RivaAshWeb.BusinessLive do
  @moduledoc """
  LiveView for managing Businesses.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.AllAtoms

  alias RivaAsh.Resources.Business

  @impl true
  def mount(_params, _session, socket) do
    businesses = RivaAsh.read(Business)

    socket =
      socket
      |> assign(:page_title, "Businesses")
      |> assign(:businesses, businesses)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Businesses" description="Manage business information">
        <:action>
          <.button phx-click="new_business" class="bg-blue-600 hover:bg-blue-700">New Business</.button>
        </:action>
      </.page_header>

      <.data_table
        id="businesses-table"
        items={@businesses}
        meta={@meta}
        path="/businesses"
      >
        <:col :let={business} label="Name" field={:name} sortable>
          <%= business.name %>
        </:col>
        <:col :let={business} label="Address">
          <%= business.address %>
        </:col>
        <:col :let={business} label="Phone">
          <%= business.phone %>
        </:col>
        <:col :let={business} label="Email">
          <%= business.email %>
        </:col>
        <:col :let={business} label="Actions">
          <.button phx-click="edit_business" phx-value-id={business.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_business" phx-value-id={business.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_business", _params, socket) do
    {:noreply, push_patch(socket, to: "/businesses/new")}
  end

  def handle_event("edit_business", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/businesses/#{id}/edit")}
  end

  def handle_event("delete_business", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting business with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
