defmodule RivaAshWeb.ItemLive do
  @moduledoc """
  LiveView for managing Items.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Item

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        items = Item.read!(actor: user)

        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, "Items")
          |> assign(:items, items)
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
          <%= item.description %>
        </:col>
        <:col :let={item} label="Price">
          <%= item.price %>
        </:col>
        <:col :let={item} label="Actions">
          <.button phx-click="edit_item" phx-value-id={item.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
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

  def handle_event("delete_item", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting item with ID: #{id}")
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
