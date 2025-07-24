defmodule RivaAshWeb.PricingLive do
  @moduledoc """
  LiveView for managing Pricing plans.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Pricing

  @impl true
  def mount(_params, _session, socket) do
    pricings = Pricing.read!()

    socket =
      socket
      |> assign(:page_title, "Pricing")
      |> assign(:pricings, pricings)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Pricing" description="Define and manage pricing rules and plans">
        <:action>
          <.button phx-click="new_pricing" class="bg-blue-600 hover:bg-blue-700">New Pricing</.button>
        </:action>
      </.page_header>

      <.data_table
        id="pricings-table"
        items={@pricings}
        meta={@meta}
        path="/pricings"
      >
        <:col :let={pricing} label="Name" field={:name} sortable>
          <%= pricing.name %>
        </:col>
        <:col :let={pricing} label="Amount">
          <%= pricing.amount %>
        </:col>
        <:col :let={pricing} label="Duration">
          <%= pricing.duration %>
        </:col>
        <:col :let={pricing} label="Actions">
          <.button phx-click="edit_pricing" phx-value-id={pricing.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_pricing" phx-value-id={pricing.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_pricing", _params, socket) do
    {:noreply, push_patch(socket, to: "/pricings/new")}
  end

  def handle_event("edit_pricing", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/pricings/#{id}/edit")}
  end

  def handle_event("delete_pricing", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting pricing with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
