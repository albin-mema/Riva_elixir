defmodule RivaAshWeb.ItemLive do
  @moduledoc """
  LiveView for managing Items.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Item
  alias RivaAsh.Resources.Business
  alias RivaAsh.Item.ItemService
  alias RivaAsh.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_items_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}

          {:error, reason} ->
            Logger.error("Failed to load items: #{inspect(reason)}")
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
      <.page_header title="Items" description="Manage items available for booking">
        <:action>
          <.button phx-click="new_item" class="bg-blue-600 hover:bg-blue-700">New Item</.button>
        </:action>
      </.page_header>

      <.data_table
        id="items-table"
        items={@items}
        meta={@meta}
        path="/items"
      >
        <:col :let={item} label="Name" field={:name} sortable>
          <%= item.name %>
        </:col>
        <:col :let={item} label="Description">
          <%= truncate_text(item.description, 100) %>
        </:col>
        <:col :let={item} label="Type">
          <%= item.item_type.name %>
        </:col>
        <:col :let={item} label="Status">
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case ItemService.get_item_status(item) do
              :available -> "bg-green-100 text-green-800"
              :occupied -> "bg-yellow-100 text-yellow-800"
              :maintenance -> "bg-red-100 text-red-800"
              _ -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(ItemService.get_item_status(item))) %>
          </span>
        </:col>
        <:col :let={item} label="Price">
          <%= format_price(item.price) %>
        </:col>
        <:col :let={item} label="Capacity">
          <%= item.capacity || "N/A" %>
        </:col>
        <:col :let={item} label="Actions">
          <.button phx-click="edit_item" phx-value-id={item.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="view_item" phx-value-id={item.id} variant="secondary">View</.button>
          <.button phx-click="delete_item" phx-value-id={item.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item", _params, socket) do
    {:noreply, push_patch(socket, to: "/items/new")}
  end

  def handle_event("edit_item", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/items/#{id}/edit")}
  end

  def handle_event("view_item", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/items/#{id}")}
  end

  def handle_event("delete_item", %{"id" => id}, socket) do
    case ItemService.delete_item(id, socket.assigns.current_user) do
      {:ok, _item} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item deleted successfully")
         |> reload_items()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete item: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_items_data(socket, user) do
    case ItemService.get_user_items(user) do
      {:ok, {items, meta}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:items, items)
          |> assign(:meta, meta)
          |> assign(:loading, false)

        {:ok, socket}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_items(socket) do
    case ItemService.get_user_items(socket.assigns.current_user) do
      {:ok, {items, meta}} ->
        assign(socket, :items, items)
        |> assign(:meta, meta)

      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :items_page_title, "Items")
  end

  defp truncate_text(nil, _length), do: "N/A"
  defp truncate_text(text, length) when byte_size(text) <= length, do: text

  defp truncate_text(text, length) do
    String.slice(text, 0, length) <> "..."
  end

  defp format_price(nil), do: "N/A"

  defp format_price(price) when is_number(price) do
    case Money.new(price, :USD) do
      {:ok, money} -> Money.to_string(money)
      {:error, _} -> "$#{:erlang.float_to_binary(price, decimals: 2)}"
    end
  end

  defp format_price(_price), do: "N/A"

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} ->
        Enum.map_join(errors, ", ", &format_validation_error/1)

      %Ash.Error.Forbidden{} ->
        "You don't have permission to perform this action"

      %Ash.Error.NotFound{} ->
        "Item not found"

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
