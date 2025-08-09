
defmodule RivaAshWeb.BusinessLive do
  @moduledoc """
  LiveView for managing Businesses.

  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Uses AuthHelpers for authentication and business scoping
  - Delegates business logic to Business context
  - Handles UI state and user interactions
  - Uses proper Ash error handling
  - Implements CRUD operations through Ash actions
  """

  use RivaAshWeb, :live_view
  import RivaAshWeb.Components.UI.Badge

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.Business
  alias RivaAsh.Businesses

  @impl true
  def mount(_params, session, socket) do
    case mount_business_scoped(
           socket,
           session,
           Business,
           [:owner_id],
           "Businesses"
         ) do
      {:ok, socket} ->
        {:ok, assign(socket, loading: false)}

      {:error, _unmatched} = error ->
        {:ok, error}
    end
  end

  @impl true
  def handle_params(params, _url, socket) do
    # Handle pagination, sorting, and filtering through business logic
    case Businesses.list_businesses(socket.assigns.current_user, params) do
      {businesses, meta} ->
        {:noreply, assign(socket, businesses: businesses, meta: meta)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load businesses: #{reason}")
         |> assign(loading: false)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Businesses" description="Manage business information">
        <:action>
          <.button phx-click="new_business" variant="primary">
            + New Business
          </.button>
        </:action>
      </.page_header>

      <%= if @loading do %>
        <div class="flex justify-center items-center py-12">
          <div class="border-b-2 border-blue-600 rounded-full w-8 h-8 animate-spin"></div>
        </div>
      <% else %>
        <.data_table
          id="businesses-table"
          items={@businesses}
          meta={@meta}
          path={~p"/businesses"}
        >
          <:col :let={business} label="Name" field={:name} sortable>
            <%= business.name %>
          </:col>
          <:col :let={business} label="Description">
            <%= business.description || "No description" %>
          </:col>
          <:col :let={business} label="Owner">
            <%= business.owner.name %>
          </:col>
          <:col :let={business} label="Status">
            <.badge variant={status_variant(business.status)}>
              <%= String.capitalize(to_string(business.status)) %>
            </.badge>
          </:col>
          <:col :let={business} label="Created">
            <%= Calendar.strftime(business.inserted_at, "%Y-%m-%d") %>
          </:col>
          <:col :let={business} label="Actions">
            <div class="flex space-x-2">
              <.button phx-click="edit_business" phx-value-id={business.id} variant="outline" size="sm">
                Edit
              </.button>
              <.button phx-click="delete_business" phx-value-id={business.id} variant="destructive" size="sm">
                Delete
              </.button>
            </div>
          </:col>
        </.data_table>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("new_business", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/businesses/new")}
  end

  def handle_event("edit_business", %{"id" => id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/businesses/#{id}/edit")}
  end

  def handle_event("delete_business", %{"id" => id}, socket) do
    case Businesses.delete_business(socket.assigns.current_user, id) do
      {:ok, _business} ->
        {:noreply,
         socket
         |> put_flash(:info, "Business deleted successfully")
         |> push_patch(to: ~p"/businesses")}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to delete this business")
         |> push_patch(to: ~p"/businesses")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete business: #{reason}")
         |> push_patch(to: ~p"/businesses")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  @doc """
  Determines the badge variant based on business status.
  """
  defp status_variant(:active), do: "default"
  defp status_variant(:inactive), do: "secondary"
  defp status_variant(:suspended), do: "destructive"
end
