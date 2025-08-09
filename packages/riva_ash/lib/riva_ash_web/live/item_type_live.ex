alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAsh.Resources, as: Resources
alias RivaAsh.ItemType, as: ItemType
alias RivaAsh.Live, as: Live
alias Ash.Error, as: Error

defmodule RivaAshWeb.ItemTypeLive do
  @moduledoc """
  LiveView for managing Item Types.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.ItemType
  alias RivaAsh.ItemType.TypeService
  alias RivaAsh.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_item_types_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}

          {:error, reason} ->
            Logger.error("Failed to load item types: #{inspect(reason)}")
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _unmatched} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Item Types" description="Categorize and manage types of items">
        <:action>
          <.button phx-click="new_item_type" class="bg-blue-600 hover:bg-blue-700">New Item Type</.button>
        </:action>
      </.page_header>

      <.data_table
        id="item-types-table"
        items={@item_types}
        meta={@meta}
        path="/item-types"
      >
        <:col :let={item_type} label="Name" field={:name} sortable>
          <%= item_type.name %>
        </:col>
        <:col :let={item_type} label="Description">
          <%= truncate_text(item_type.description, 100) %>
        </:col>
        <:col :let={item_type} label="Business">
          <%= if item_type.business do %>
            <%= item_type.business.name %>
          <% else %>
            <span class="text-gray-500">Unknown Business</span>
          <% end %>
        </:col>
        <:col :let={item_type} label="Item Count">
          <%= item_type.item_count || 0 %>
        </:col>
        <:col :let={item_type} label="Status">
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case item_type.status do
              :active -> "bg-green-100 text-green-800"
              :inactive -> "bg-gray-100 text-gray-800"
              _unmatchedunmatched -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(item_type.status)) %>
          </span>
        </:col>
        <:col :let={item_type} label="Actions">
          <%= if item_type.status == :active do %>
            <.button phx-click="edit_item_type" phx-value-id={item_type.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
            <.button phx-click="deactivate_item_type" phx-value-id={item_type.id} class="bg-yellow-600 hover:bg-yellow-700">Deactivate</.button>
          <% else %>
            <.button phx-click="activate_item_type" phx-value-id={item_type.id} class="bg-green-600 hover:bg-green-700">Activate</.button>
            <.button phx-click="view_item_type" phx-value-id={item_type.id} variant="secondary">View</.button>
          <% end %>
          <.button phx-click="delete_item_type" phx-value-id={item_type.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item_type", _params, socket) do
    {:noreply, push_patch(socket, to: "/item-types/new")}
  end

  def handle_event("edit_item_type", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-types/#{id}/edit")}
  end

  def handle_event("view_item_type", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-types/#{id}")}
  end

  def handle_event("activate_item_type", %{"id" => id}, socket) do
    case TypeService.activate_type(id, socket.assigns.current_user) do
      {:ok, _item_type} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item type activated successfully")
         |> reload_item_types()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to activate item type: #{format_error(reason)}")}
    end
  end

  def handle_event("deactivate_item_type", %{"id" => id}, socket) do
    case TypeService.deactivate_type(id, socket.assigns.current_user) do
      {:ok, _item_type} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item type deactivated successfully")
         |> reload_item_types()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to deactivate item type: #{format_error(reason)}")}
    end
  end

  def handle_event("delete_item_type", %{"id" => id}, socket) do
    case TypeService.delete_type(id, socket.assigns.current_user) do
      {:ok, _item_type} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item type deleted successfully")
         |> reload_item_types()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete item type: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_item_types_data(socket, user) do
    case TypeService.get_user_types(user) do
      {:ok, {item_types, meta}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:item_types, item_types)
          |> assign(:meta, meta)
          |> assign(:loading, false)

        {:ok, socket}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_item_types(socket) do
    case TypeService.get_user_types(socket.assigns.current_user) do
      {:ok, {item_types, meta}} ->
        assign(socket, :item_types, item_types)
        |> assign(:meta, meta)

      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :item_types_page_title, "Item Types")
  end

  defp truncate_text(nil, _length), do: "N/A"
  defp truncate_text(text, length) when byte_size(text) <= length, do: text

  defp truncate_text(text, length) do
    String.slice(text, 0, length) <> "..."
  end

  defp format_error(reason) do
    case RivaAsh.ErrorHelpers.format_error(reason) do
      %{message: message} -> message
      message when is_binary(message) -> message
      _ -> "An unexpected error occurred"
    end
  end

  defp format_validation_error(error) do
    case error do
      {message, _unmatched} -> message
      message when is_binary(message) -> message
      _unmatchedunmatched -> "Invalid input"
    end
  end
end
