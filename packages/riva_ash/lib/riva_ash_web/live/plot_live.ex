defmodule RivaAshWeb.PlotLive do
  @moduledoc """
  LiveView for managing Plots.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Plot

  @impl true
  def mount(_params, _session, socket) do
    plots = Plot.read!()

    socket =
      socket
      |> assign(:page_title, "Plots")
      |> assign(:plots, plots)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Plots" description="Manage plots and their attributes">
        <:action>
          <.button phx-click="new_plot" class="bg-blue-600 hover:bg-blue-700">New Plot</.button>
        </:action>
      </.page_header>

      <.data_table
        id="plots-table"
        items={@plots}
        meta={@meta}
        path="/plots"
      >
        <:col :let={plot} label="Name" field={:name} sortable>
          <%= plot.name %>
        </:col>
        <:col :let={plot} label="Capacity">
          <%= plot.capacity %>
        </:col>
        <:col :let={plot} label="Description">
          <%= plot.description %>
        </:col>
        <:col :let={plot} label="Actions">
          <.button phx-click="edit_plot" phx-value-id={plot.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_plot" phx-value-id={plot.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_plot", _params, socket) do
    {:noreply, push_patch(socket, to: "/plots/new")}
  end

  def handle_event("edit_plot", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/plots/#{id}/edit")}
  end

  def handle_event("delete_plot", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting plot with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
