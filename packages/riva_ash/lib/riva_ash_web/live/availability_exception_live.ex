alias RivaAshWeb.Components.UI, as: UI
alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Live, as: Live
alias RivaAsh.Resources, as: Resources
alias Ash.Error, as: Error

defmodule RivaAshWeb.AvailabilityExceptionLive do
  @moduledoc """
  LiveView for managing Availability Exceptions.

  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Uses AuthHelpers for authentication and business scoping
  - Delegates business logic to AvailabilityException context
  - Handles UI state and user interactions
  - Uses proper Ash error handling
  - Implements CRUD operations through Ash actions
  """

  use RivaAshWeb, :live_view
  import RivaAshWeb.Components.UI.Badge

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.AvailabilityException
  alias RivaAsh.AvailabilityExceptions
  alias RivaAsh.ErrorHelpers

  @impl true
  def mount(_params, session, socket) do
    case mount_business_scoped(
           socket,
           session,
           AvailabilityException,
           [:item, :business_id],
           "Availability Exceptions"
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
    case AvailabilityExceptions.list_exceptions(socket.assigns.current_user, params) do
      {exceptions, meta} ->
        {:noreply, assign(socket, exceptions: exceptions, meta: meta)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load availability exceptions: #{reason}")
         |> assign(loading: false)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Availability Exceptions" description="Manage exceptions for item availability">
        <:action>
          <.button phx-click="new_exception" variant="primary">
            + New Exception
          </.button>
        </:action>
      </.page_header>

      <%= if @loading do %>
        <div class="flex justify-center items-center py-12">
          <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
        </div>
      <% else %>
        <.data_table
          id="availability-exceptions-table"
          items={@exceptions}
          meta={@meta}
          path={~p"/availability-exceptions"}
        >
          <:col :let={exception} label="Item" field={:item_id} sortable>
            <%= exception.item.name %>
          </:col>
          <:col :let={exception} label="Start Date" field={:start_date} sortable>
            <%= Calendar.strftime(exception.start_date, "%Y-%m-%d") %>
          </:col>
          <:col :let={exception} label="End Date" field={:end_date} sortable>
            <%= if exception.end_date, do: Calendar.strftime(exception.end_date, "%Y-%m-%d"), else: "Ongoing" %>
          </:col>
          <:col :let={exception} label="Reason">
            <%= exception.reason || "No reason provided" %>
          </:col>
          <:col :let={exception} label="Status">
            <.badge variant={status_variant(exception.status)}>
              <%= String.capitalize(to_string(exception.status)) %>
            </.badge>
          </:col>
          <:col :let={exception} label="Actions">
            <div class="flex space-x-2">
              <.button phx-click="edit_exception" phx-value-id={exception.id} variant="outline" size="sm">
                Edit
              </.button>
              <.button phx-click="delete_exception" phx-value-id={exception.id} variant="destructive" size="sm">
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
  def handle_event("new_exception", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/availability-exceptions/new")}
  end

  def handle_event("edit_exception", %{"id" => id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/availability-exceptions/#{id}/edit")}
  end

  def handle_event("delete_exception", %{"id" => id}, socket) do
    case AvailabilityExceptions.delete_exception(socket.assigns.current_user, id) do
      {:ok, _exception} ->
        {:noreply,
         socket
         |> put_flash(:info, "Availability exception deleted successfully")
         |> push_patch(to: ~p"/availability-exceptions")}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to delete this exception")
         |> push_patch(to: ~p"/availability-exceptions")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete exception: #{reason}")
         |> push_patch(to: ~p"/availability-exceptions")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  @doc """
  Determines the badge variant based on exception status.
  """
  defp status_variant(:active), do: "destructive"
  defp status_variant(:pending), do: "secondary"
  defp status_variant(:expired), do: "outline"
  defp status_unmatchedvariant(_unmatched), do: "secondary"
end
