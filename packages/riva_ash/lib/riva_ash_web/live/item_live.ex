defmodule RivaAshWeb.ItemLive do
  @moduledoc """
  Item management LiveView with positioning support.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Organisms.ItemForm
  import RivaAshWeb.Components.Molecules.EmptyState

  alias RivaAsh.Resources.Item

  @impl true
  def mount(_params, session, socket) do
    user = get_current_user_from_session(session)

    if user do
      socket =
        socket
        |> assign(:current_user, user)
        |> assign(:page_title, "Item Management")
        |> assign(:items, [])
        |> assign(:meta, %{})
        |> assign(:show_form, false)
        |> assign(:editing_item, nil)
        |> assign(:form, nil)
        |> assign(:sections, [])
        |> assign(:item_types, [])

      {:ok, socket}
    else
      {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!-- Item management implementation will go here -->
    <div>
      <.page_header title="Item Management" description="Manage your reservable items and their positions">
        <:action>
          <button phx-click="new_item">Add Item</button>
        </:action>
      </.page_header>

      <div :if={@show_form}>
        <.item_form
          form={@form}
          sections={@sections}
          item_types={@item_types}
          editing={@editing_item != nil}
          on_submit="save_item"
          on_change="validate_item"
          on_cancel="cancel_form"
        />
      </div>

      <div :if={@items == [] && !@show_form}>
        <.empty_state
          icon={:cube}
          title="No items found"
          description="Create your first reservable item to start taking bookings"
        />
      </div>

      <.data_table
        :if={@items != [] && !@show_form}
        items={@items}
        meta={@meta}
        path="/items"
        id="items-table"
      >
        <:col :let={item} label="Name" field={:name} sortable>
          <%= item.name %>
        </:col>
        <:col :let={item} label="Section" field={:section} sortable>
          <%= item.section.name %>
        </:col>
        <:col :let={item} label="Type" field={:item_type} sortable>
          <%= item.item_type.name %>
        </:col>
        <:col :let={item} label="Position">
          Row <%= item.grid_row %>, Col <%= item.grid_column %>
        </:col>
        <:col :let={item} label="Status">
          <%= if item.is_active, do: "Active", else: "Inactive" %>
        </:col>
        <:col :let={item} label="Actions">
          <button phx-click="edit_item" phx-value-id={item.id}>Edit</button>
          <button phx-click="delete_item" phx-value-id={item.id}>Delete</button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item", _params, socket) do
    {:noreply, assign(socket, :show_form, true)}
  end

  def handle_event("edit_item", %{"id" => id}, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("delete_item", %{"id" => id}, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("save_item", _params, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("validate_item", _params, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("cancel_form", _params, socket) do
    {:noreply, assign(socket, :show_form, false)}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions will go here
  defp get_current_user_from_session(_session) do
    # Implementation will go here
    nil
  end
end
