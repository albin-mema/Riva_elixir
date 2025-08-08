alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAsh.Resources, as: Resources
alias RivaAsh.Permission, as: Permission
alias RivaAsh.Live, as: Live
alias Ash.Error, as: Error

defmodule RivaAshWeb.PermissionLive do
  @moduledoc """
  Permission and role management LiveView.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Organisms.PermissionMatrix
  import RivaAshWeb.Components.Molecules.TabNavigation
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.{Permission, Employee, EmployeePermission}
  alias RivaAsh.Permission.PermissionService
  alias RivaAsh.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_permissions_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}

          {:error, reason} ->
            Logger.error("Failed to load permissions data: #{inspect(reason)}")
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
      <.page_header title="Permission Management" description="Manage employee roles and permissions">
        <:action>
          <.button phx-click="create_permission" class="bg-blue-600 hover:bg-blue-700">Create Permission</.button>
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
          path="/permissions"
          id="employees-permissions-table"
        >
          <:col :let={item} label="Employee" field={:name} sortable>
            <%= item.first_name %> <%= item.last_name %>
          </:col>
          <:col :let={item} label="Role" field={:role}>
            <%= item.role %>
          </:col>
          <:col :let={item} label="Permissions Count">
            <%= length(item.permissions || []) %>
          </:col>
          <:col :let={item} label="Status">
            <span class={[
              "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
              case item.status do
                :active -> "bg-green-100 text-green-800"
                :inactive -> "bg-gray-100 text-gray-800"
                _unmatchedunmatched -> "bg-gray-100 text-gray-800"
              end
            ]}>
              <%= String.capitalize(to_string(item.status)) %>
            </span>
          </:col>
          <:col :let={item} label="Actions">
            <.button phx-click="manage_permissions" phx-value-id={item.id} class="bg-indigo-600 hover:bg-indigo-700">Manage Permissions</.button>
            <.button phx-click="edit_employee" phx-value-id={item.id} variant="secondary" size="sm">Edit</.button>
            <.button phx-click="delete_employee" phx-value-id={item.id} variant="danger" size="sm">Delete</.button>
          </:col>
        </.data_table>
      </div>

      <div :if={@active_tab == "permissions"}>
        <.data_table
          items={@permissions}
          meta={@meta}
          path="/permissions"
          id="permissions-table"
        >
          <:col :let={item} label="Name" field={:name} sortable>
            <%= item.name %>
          </:col>
          <:col :let={item} label="Description">
            <%= truncate_text(item.description, 100) %>
          </:col>
          <:col :let={item} label="Module">
            <%= item.module || "N/A" %>
          </:col>
          <:col :let={item} label="Action">
            <%= item.action || "N/A" %>
          </:col>
          <:col :let={item} label="Status">
            <span class={[
              "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
              case item.status do
                :active -> "bg-green-100 text-green-800"
                :inactive -> "bg-gray-100 text-gray-800"
                _unmatchedunmatched -> "bg-gray-100 text-gray-800"
              end
            ]}>
              <%= String.capitalize(to_string(item.status)) %>
            </span>
          </:col>
          <:col :let={item} label="Actions">
            <.button phx-click="edit_permission" phx-value-id={item.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
            <.button phx-click="delete_permission" phx-value-id={item.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
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
    case PermissionService.get_employee_permissions(employee_id, socket.assigns.current_user) do
      {:ok, {employee, permissions, current_permissions}} ->
        {:noreply,
         socket
         |> assign(:selected_employee, employee)
         |> assign(:permissions, permissions)
         |> assign(:current_permissions, current_permissions)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load employee permissions: #{format_error(reason)}")}
    end
  end

  def handle_event("toggle_permission", %{"permission" => permission_id}, socket) do
    # Toggle permission in current_permissions list
    current_permissions = socket.assigns.current_permissions

    if permission_id in current_permissions do
      new_permissions = List.delete(current_permissions, permission_id)
      {:noreply, assign(socket, :current_permissions, new_permissions)}
    else
      new_permissions = [permission_id | current_permissions]
      {:noreply, assign(socket, :current_permissions, new_permissions)}
    end
  end

  def handle_event("save_permissions", _params, socket) do
    case PermissionService.save_employee_permissions(
           socket.assigns.selected_employee.id,
           socket.assigns.current_permissions,
           socket.assigns.current_user
         ) do
      {:ok, _employee} ->
        {:noreply,
         socket
         |> put_flash(:info, "Permissions saved successfully")
         |> assign(:selected_employee, nil)
         |> reload_permissions_data()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to save permissions: #{format_error(reason)}")}
    end
  end

  def handle_event("cancel_permission_edit", _params, socket) do
    {:noreply, assign(socket, :selected_employee, nil)}
  end

  def handle_event("create_permission", _params, socket) do
    {:noreply, push_patch(socket, to: "/permissions/new")}
  end

  def handle_event("edit_permission", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/permissions/#{id}/edit")}
  end

  def handle_event("delete_permission", %{"id" => id}, socket) do
    case PermissionService.delete_permission(id, socket.assigns.current_user) do
      {:ok, _permission} ->
        {:noreply,
         socket
         |> put_flash(:info, "Permission deleted successfully")
         |> reload_permissions_data()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete permission: #{format_error(reason)}")}
    end
  end

  def handle_event("edit_employee", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/employees/#{id}/edit")}
  end

  def handle_event("delete_employee", %{"id" => id}, socket) do
    case PermissionService.delete_employee(id, socket.assigns.current_user) do
      {:ok, _employee} ->
        {:noreply,
         socket
         |> put_flash(:info, "Employee deleted successfully")
         |> reload_permissions_data()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete employee: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_permissions_data(socket, user) do
    case PermissionService.get_user_permissions_data(user) do
      {:ok, {employees, permissions, meta}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:employees, employees)
          |> assign(:permissions, permissions)
          |> assign(:active_tab, "employees")
          |> assign(:selected_employee, nil)
          |> assign(:current_permissions, [])
          |> assign(:meta, meta)
          |> assign(:loading, false)

        {:ok, socket}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_permissions_data(socket) do
    case PermissionService.get_user_permissions_data(socket.assigns.current_user) do
      {:ok, {employees, permissions, meta}} ->
        socket
        |> assign(:employees, employees)
        |> assign(:permissions, permissions)
        |> assign(:meta, meta)

      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Permission Management"
  end

  defp truncate_text(nil, _length), do: "N/A"
  defp truncate_text(text, length) when byte_size(text) <= length, do: text

  defp truncate_text(text, length) do
    String.slice(text, 0, length) <> "..."
  end

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} ->
        Enum.map_join(errors, ", ", &format_validation_error/1)

      %Ash.Error.Forbidden{} ->
        "You don't have permission to perform this action"

      %Ash.Error.NotFound{} ->
        "Resource not found"

      _unmatchedunmatched ->
        "An unexpected error occurred"
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
