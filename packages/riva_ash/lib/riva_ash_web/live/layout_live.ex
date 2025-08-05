defmodule RivaAshWeb.LayoutLive do
  @moduledoc """
  LiveView for managing Layouts.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Layout
  alias RivaAsh.Layout.LayoutService
  alias RivaAsh.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_layouts_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}
          {:error, reason} ->
            Logger.error("Failed to load layouts: #{inspect(reason)}")
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
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
          <%= truncate_text(layout.description, 100) %>
        </:col>
        <:col :let={layout} label="Business">
          <%= if layout.business do %>
            <%= layout.business.name %>
          <% else %>
            <span class="text-gray-500">Unknown Business</span>
          <% end %>
        </:col>
        <:col :let={layout} label="Dimensions">
          <%= layout.width %> × <%= layout.height %>
        </:col>
        <:col :let={layout} label="Status">
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case layout.status do
              :active -> "bg-green-100 text-green-800"
              :inactive -> "bg-gray-100 text-gray-800"
              _ -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(layout.status)) %>
          </span>
        </:col>
        <:col :let={layout} label="Actions">
          <%= if layout.status == :active do %>
            <.button phx-click="edit_layout" phx-value-id={layout.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
            <.button phx-click="deactivate_layout" phx-value-id={layout.id} class="bg-yellow-600 hover:bg-yellow-700">Deactivate</.button>
          <% else %>
            <.button phx-click="activate_layout" phx-value-id={layout.id} class="bg-green-600 hover:bg-green-700">Activate</.button>
            <.button phx-click="view_layout" phx-value-id={layout.id} variant="secondary">View</.button>
          <% end %>
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

  def handle_event("view_layout", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/layouts/#{id}")}
  end

  def handle_event("activate_layout", %{"id" => id}, socket) do
    case LayoutService.activate_layout(id, socket.assigns.current_user) do
      {:ok, _layout} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Layout activated successfully")
         |> reload_layouts()}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Failed to activate layout: #{format_error(reason)}")}
    end
  end

  def handle_event("deactivate_layout", %{"id" => id}, socket) do
    case LayoutService.deactivate_layout(id, socket.assigns.current_user) do
      {:ok, _layout} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Layout deactivated successfully")
         |> reload_layouts()}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Failed to deactivate layout: #{format_error(reason)}")}
    end
  end

  def handle_event("delete_layout", %{"id" => id}, socket) do
    case LayoutService.delete_layout(id, socket.assigns.current_user) do
      {:ok, _layout} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Layout deleted successfully")
         |> reload_layouts()}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Failed to delete layout: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_layouts_data(socket, user) do
    case LayoutService.get_user_layouts(user) do
      {:ok, {layouts, meta}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:layouts, layouts)
          |> assign(:meta, meta)
          |> assign(:loading, false)
        
        {:ok, socket}
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_layouts(socket) do
    case LayoutService.get_user_layouts(socket.assigns.current_user) do
      {:ok, {layouts, meta}} ->
        assign(socket, :layouts, layouts)
        |> assign(:meta, meta)
      
      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :layouts_page_title, "Layouts")
  end

  defp truncate_text(nil, _length), do: "N/A"
  defp truncate_text(text, length) when byte_size(text) <= length, do: text
  defp truncate_text(text, length) do
    String.slice(text, 0, length) <> "..."
  end

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} -> 
        errors |> Enum.map(&format_validation_error/1) |> Enum.join(", ")
      %Ash.Error.Forbidden{} -> "You don't have permission to perform this action"
      %Ash.Error.NotFound{} -> "Layout not found"
      _ -> "An unexpected error occurred"
    end
  end

  defp format_validation_error(error) do
    case error do
      {message, _} -> message
      message when is_binary(message) -> message
      _ -> "Invalid input"
    end
  end
end