defmodule RivaAshWeb.ItemLive do
  @moduledoc """
  Item management LiveView with positioning support.
  """
  use RivaAshWeb, :live_view
  import OK, only: [success: 1, failure: 1, ~>>: 2, for: 1, required: 2]

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Organisms.ItemForm
  import RivaAshWeb.Components.Molecules.EmptyState

  alias RivaAsh.Resources.Item

  @impl true
  def mount(_params, session, socket) do
    get_current_user_from_session(session)
    |> OK.required(:user_not_authenticated)
    ~>> fn user ->
      socket
      |> assign(:current_user, user)
      |> assign(:page_title, "Item Management")
      |> assign(:items, load_items(user))
      |> assign(:meta, %{})
      |> assign(:show_form, false)
      |> assign(:editing_item, nil)
      |> assign(:form, nil)
      |> assign(:sections, load_sections(user))
      |> assign(:item_types, load_item_types(user))
    end
    |> case do
      {:ok, socket} -> success(socket)
      {:error, :user_not_authenticated} -> success(redirect(socket, to: "/sign-in"))
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
    form = AshPhoenix.Form.for_create(Item, :create, actor: socket.assigns.current_user) |> to_form()

    socket
    |> assign(:show_form, true)
    |> assign(:form, form)
    |> then(&{:noreply, &1})
  end

  def handle_event("edit_item", %{"id" => id}, socket) do
    OK.for do
      item <- Item.by_id(id)
      form <- OK.wrap(AshPhoenix.Form.for_update(item, :update, actor: socket.assigns.current_user))
    after
      socket
      |> assign(:editing_item, item)
      |> assign(:form, form |> to_form())
      |> assign(:show_form, true)
    else
      _ ->
        socket
        |> put_flash(:error, "Failed to load item for editing")
    end
    |> then(&{:noreply, &1})
  end

  def handle_event("delete_item", %{"id" => id}, socket) do
    Item.by_id(id)
    ~>> fn item ->
      Item.destroy(item, actor: socket.assigns.current_user)
    end
    |> case do
      {:ok, _} ->
        socket
        |> put_flash(:info, "Item deleted successfully")
        |> assign(:items, load_items(socket.assigns.current_user))
      {:error, error} ->
        socket
        |> put_flash(:error, "Failed to delete item: #{inspect(error)}")
    end
    |> then(&{:noreply, &1})
  end

  def handle_event("save_item", %{"form" => params}, socket) do
    AshPhoenix.Form.submit(socket.assigns.form, params: params, actor: socket.assigns.current_user)
    ~>> fn item ->
      action_text = if socket.assigns.editing_item, do: "updated", else: "created"
      
      socket
      |> assign(:items, load_items(socket.assigns.current_user))
      |> assign(:show_form, false)
      |> assign(:editing_item, nil)
      |> assign(:form, nil)
      |> put_flash(:info, "Item #{action_text} successfully")
    end
    |> case do
      {:ok, socket} -> {:noreply, socket}
      {:error, form} ->
        socket =
          socket
          |> assign(:form, form |> to_form())
          |> put_flash(:error, "Please fix the errors below")
        {:noreply, socket}
    end
  end

  def handle_event("validate_item", %{"form" => params}, socket) do
    form = AshPhoenix.Form.validate(socket.assigns.form, params) |> to_form()
    socket = assign(socket, :form, form)
    {:noreply, socket}
  end

  def handle_event("cancel_form", _params, socket) do
    socket
    |> assign(:show_form, false)
    |> assign(:editing_item, nil)
    |> assign(:form, nil)
    |> then(&{:noreply, &1})
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  defp get_current_user_from_session(session) do
    # Implementation will be added when auth system is integrated
    case Map.get(session, "user_token") do
      nil -> nil
      _token -> %{id: "user-1", role: :user} # Mock user for now
    end
  end

  defp load_items(user) do
    Item
    |> Ash.Query.load([:section, :item_type])
    |> Ash.read(actor: user)
    |> case do
      {:ok, items} -> items
      {:error, _} -> []
    end
  end

  defp load_sections(user) do
    RivaAsh.Resources.Section
    |> Ash.read(actor: user)
    |> case do
      {:ok, sections} -> sections
      {:error, _} -> []
    end
  end

  defp load_item_types(user) do
    RivaAsh.Resources.ItemType
    |> Ash.read(actor: user)
    |> case do
      {:ok, types} -> types
      {:error, _} -> []
    end
  end
end
end
