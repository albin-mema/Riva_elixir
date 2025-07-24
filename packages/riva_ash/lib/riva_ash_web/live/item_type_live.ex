defmodule RivaAshWeb.ItemTypeLive do
  @moduledoc """
  LiveView for managing Item Types.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.ItemType

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        item_types = ItemType.read!(actor: user)

        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, "Item Types")
          |> assign(:item_types, item_types)
          |> assign(:meta, %{}) # Placeholder for pagination/metadata

        {:ok, socket}
      {:error, _} ->
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
          <%= item_type.description %>
        </:col>
        <:col :let={item_type} label="Actions">
          <.button phx-click="edit_item_type" phx-value-id={item_type.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
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

  def handle_event("delete_item_type", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting item type with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions
  defp get_current_user_from_session(session) do
    user_token = session["user_token"]

    if user_token do
      with {:ok, user_id} <- Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", user_token, max_age: 86_400) |> RivaAsh.ErrorHelpers.to_result(),
           {:ok, user} <- Ash.get(RivaAsh.Accounts.User, user_id, domain: RivaAsh.Accounts) |> RivaAsh.ErrorHelpers.to_result() do
        RivaAsh.ErrorHelpers.success(user)
      else
        _ -> RivaAsh.ErrorHelpers.failure(:not_authenticated)
      end
    else
      RivaAsh.ErrorHelpers.failure(:not_authenticated)
    end
  end
end
