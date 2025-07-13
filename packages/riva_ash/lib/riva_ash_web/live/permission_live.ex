defmodule RivaAshWeb.PermissionLive do
  @moduledoc """
  Permission and role management LiveView.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Organisms.PermissionMatrix
  import RivaAshWeb.Components.Molecules.TabNavigation

  alias RivaAsh.Resources.{Permission, Employee, EmployeePermission}

  @impl true
  def mount(_params, session, socket) do
    user = get_current_user_from_session(session)

    if user do
      socket =
        socket
        |> assign(:current_user, user)
        |> assign(:page_title, "Permission Management")
        |> assign(:active_tab, "employees")
        |> assign(:employees, [])
        |> assign(:permissions, [])
        |> assign(:selected_employee, nil)
        |> assign(:current_permissions, [])
        |> assign(:meta, %{})

      {:ok, socket}
    else
      {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!-- Permission management implementation will go here -->
    <div>
      <.page_header title="Permission Management" description="Manage employee roles and permissions">
        <:action>
          <button phx-click="create_permission">Create Permission</button>
        </:action>
      </.page_header>
      
      <.tab_navigation
        tabs={[
          %{id: "employees", label: "Employee Permissions"},
          %{id: "permissions", label: "Permission List"}
        ]}
        active_tab={@active_tab}
        on_tab_change="change_tab"
      />
      
      <div :if={@active_tab == "employees"}>
        <div :if={@selected_employee}>
          <.permission_matrix
            employee={@selected_employee}
            permissions={@permissions}
            current_permissions={@current_permissions}
            on_permission_change="toggle_permission"
            on_save="save_permissions"
            on_cancel="cancel_permission_edit"
          />
        </div>
        
        <.data_table
          :if={!@selected_employee}
          items={@employees}
          meta={@meta}
          path={~p"/permissions"}
          id="employees-permissions-table"
        >
          <:col label="Employee" field={:name} sortable>
            <%= item.first_name %> <%= item.last_name %>
          </:col>
          <:col label="Role" field={:role}>
            <%= item.role %>
          </:col>
          <:col label="Permissions Count">
            <%= length(item.permissions || []) %>
          </:col>
          <:col label="Actions">
            <button phx-click="manage_permissions" phx-value-id={item.id}>Manage Permissions</button>
          </:col>
        </.data_table>
      </div>
      
      <div :if={@active_tab == "permissions"}>
        <.data_table
          items={@permissions}
          meta={@meta}
          path={~p"/permissions"}
          id="permissions-table"
        >
          <:col label="Name" field={:name} sortable>
            <%= item.name %>
          </:col>
          <:col label="Description">
            <%= item.description %>
          </:col>
          <:col label="Actions">
            <button phx-click="edit_permission" phx-value-id={item.id}>Edit</button>
            <button phx-click="delete_permission" phx-value-id={item.id}>Delete</button>
          </:col>
        </.data_table>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("change_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, :active_tab, tab)}
  end

  def handle_event("manage_permissions", %{"id" => employee_id}, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("toggle_permission", %{"permission" => permission_id}, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("save_permissions", _params, socket) do
    # Implementation will go here
    {:noreply, assign(socket, :selected_employee, nil)}
  end

  def handle_event("cancel_permission_edit", _params, socket) do
    {:noreply, assign(socket, :selected_employee, nil)}
  end

  def handle_event("create_permission", _params, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("edit_permission", %{"id" => id}, socket) do
    # Implementation will go here
    {:noreply, socket}
  end

  def handle_event("delete_permission", %{"id" => id}, socket) do
    # Implementation will go here
    {:noreply, socket}
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
