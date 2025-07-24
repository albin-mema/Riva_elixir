defmodule RivaAshWeb.SectionLive do
  @moduledoc """
  LiveView for managing Sections.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Section

  @impl true
  def mount(_params, _session, socket) do
    sections = Section.read!()

    socket =
      socket
      |> assign(:page_title, "Sections")
      |> assign(:sections, sections)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Sections" description="Organize items into various sections">
        <:action>
          <.button phx-click="new_section" class="bg-blue-600 hover:bg-blue-700">New Section</.button>
        </:action>
      </.page_header>

      <.data_table
        id="sections-table"
        items={@sections}
        meta={@meta}
        path="/sections"
      >
        <:col :let={section} label="Name" field={:name} sortable>
          <%= section.name %>
        </:col>
        <:col :let={section} label="Description">
          <%= section.description %>
        </:col>
        <:col :let={section} label="Actions">
          <.button phx-click="edit_section" phx-value-id={section.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_section" phx-value-id={section.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_section", _params, socket) do
    {:noreply, push_patch(socket, to: "/sections/new")}
  end

  def handle_event("edit_section", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/sections/#{id}/edit")}
  end

  def handle_event("delete_section", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting section with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
