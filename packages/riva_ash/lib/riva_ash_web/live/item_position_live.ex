defmodule RivaAshWeb.ItemPositionLive do
  @moduledoc """
  LiveView for managing Item Positions.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.ItemPosition
  alias RivaAsh.Position.PositionService
  alias RivaAsh.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_item_positions_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}

          {:error, reason} ->
            Logger.error("Failed to load item positions: #{inspect(reason)}")
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
      <.page_header title="Item Positions" description="Manage item positions within a layout">
        <:action>
          <.button phx-click="new_item_position" class="bg-blue-600 hover:bg-blue-700">New Item Position</.button>
        </:action>
      </.page_header>

      <.data_table
        id="item-positions-table"
        items={@item_positions}
        meta={@meta}
        path="/item-positions"
      >
        <:col :let={item_position} label="Item" field={:item_id} sortable>
          <%= if item_position.item do %>
            <%= item_position.item.name %>
          <% else %>
            <span class="text-gray-500">Unknown Item</span>
          <% end %>
        </:col>
        <:col :let={item_position} label="Layout" field={:layout_id} sortable>
          <%= if item_position.layout do %>
            <%= item_position.layout.name %>
          <% else %>
            <span class="text-gray-500">Unknown Layout</span>
          <% end %>
        </:col>
        <:col :let={item_position} label="X Coordinate">
          <%= item_position.x_coord %>
        </:col>
        <:col :let={item_position} label="Y Coordinate">
          <%= item_position.y_coord %>
        </:col>
        <:col :let={item_position} label="Width">
          <%= item_position.width || "Auto" %>
        </:col>
        <:col :let={item_position} label="Height">
          <%= item_position.height || "Auto" %>
        </:col>
        <:col :let={item_position} label="Rotation">
          <%= item_position.rotation || "0°" %>
        </:col>
        <:col :let={item_position} label="Status">
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case item_position.status do
              :active -> "bg-green-100 text-green-800"
              :inactive -> "bg-gray-100 text-gray-800"
              _ -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(item_position.status)) %>
          </span>
        </:col>
        <:col :let={item_position} label="Actions">
          <%= if item_position.status == :active do %>
            <.button phx-click="edit_item_position" phx-value-id={item_position.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
            <.button phx-click="deactivate_item_position" phx-value-id={item_position.id} class="bg-yellow-600 hover:bg-yellow-700">Deactivate</.button>
          <% else %>
            <.button phx-click="activate_item_position" phx-value-id={item_position.id} class="bg-green-600 hover:bg-green-700">Activate</.button>
            <.button phx-click="view_item_position" phx-value-id={item_position.id} variant="secondary">View</.button>
          <% end %>
          <.button phx-click="delete_item_position" phx-value-id={item_position.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item_position", _params, socket) do
    {:noreply, push_patch(socket, to: "/item-positions/new")}
  end

  def handle_event("edit_item_position", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-positions/#{id}/edit")}
  end

  def handle_event("view_item_position", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-positions/#{id}")}
  end

  def handle_event("activate_item_position", %{"id" => id}, socket) do
    case PositionService.activate_position(id, socket.assigns.current_user) do
      {:ok, _item_position} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item position activated successfully")
         |> reload_item_positions()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to activate item position: #{format_error(reason)}")}
    end
  end

  def handle_event("deactivate_item_position", %{"id" => id}, socket) do
    case PositionService.deactivate_position(id, socket.assigns.current_user) do
      {:ok, _item_position} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item position deactivated successfully")
         |> reload_item_positions()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to deactivate item position: #{format_error(reason)}")}
    end
  end

  def handle_event("delete_item_position", %{"id" => id}, socket) do
    case PositionService.delete_position(id, socket.assigns.current_user) do
      {:ok, _item_position} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item position deleted successfully")
         |> reload_item_positions()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete item position: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_item_positions_data(socket, user) do
    case PositionService.get_user_positions(user) do
      {:ok, {item_positions, meta}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:item_positions, item_positions)
          |> assign(:meta, meta)
          |> assign(:loading, false)

        {:ok, socket}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_item_positions(socket) do
    case PositionService.get_user_positions(socket.assigns.current_user) do
      {:ok, {item_positions, meta}} ->
        assign(socket, :item_positions, item_positions)
        |> assign(:meta, meta)

      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :item_positions_page_title, "Item Positions")
  end

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} ->
        Enum.map_join(errors, ", ", &format_validation_error/1)

      %Ash.Error.Forbidden{} ->
        "You don't have permission to perform this action"

      %Ash.Error.NotFound{} ->
        "Item position not found"

      _ ->
        "An unexpected error occurred"
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
