alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Live, as: Live
alias RivaAsh.Resources, as: Resources
alias AshPhoenix.Form, as: Form
alias Flop.Phoenix, as: Phoenix
alias Ash.Error, as: Error

defmodule RivaAshWeb.EmployeeLive do
  @moduledoc """
  LiveView for managing Employees.

  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Uses AuthHelpers for authentication and business scoping
  - Delegates business logic to Employee context
  - Handles UI state and user interactions
  - Uses proper Ash error handling
  - Implements CRUD operations through Ash actions
  """

  use RivaAshWeb, :live_view

  alias RivaAsh.Employees
  alias RivaAsh.ErrorHelpers

  require Ash.Query

  # Import atomic design components
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Badge
  import RivaAshWeb.Components.Molecules.EmptyState
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.EmployeeForm
  import RivaAshWeb.Components.Molecules.ConfirmDialog
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.Employee
  alias RivaAsh.Resources.Business

  @impl true
  def mount(_params, session, socket) do
    case mount_business_scoped(
           socket,
           session,
           Employee,
           [:business_id],
           "Employee Management"
         ) do
      {:ok, socket} ->
        {:ok,
         socket
         |> assign(
           :form,
           AshPhoenix.Form.for_create(Employee, :create, actor: socket.assigns.current_user) |> to_form()
         )
         |> assign(:show_form, false)
         |> assign(:editing_employee, nil)
         |> assign(:loading, false)
         |> assign(:is_admin, socket.assigns.current_user.role == :admin)
         |> assign(:show_confirm_delete_modal, false)
         |> assign(:employee_to_delete, nil)}

      {:error, _unmatched} = error ->
        {:ok, error}
    end
  end

  @impl true
  def handle_params(params, _uri, socket) do
    user = socket.assigns.current_user

    # Use Flop to handle pagination, sorting, and filtering through business logic
    case Employees.list_employees(user, params) do
      {employees, meta} ->
        {:noreply,
         socket
         |> assign(:employees, employees)
         |> assign(:meta, meta)}

      {:error, reason} ->
        {:noreply,
         socket
         |> assign(:employees, [])
         |> assign(:meta, %{})
         |> put_flash(:error, "Failed to load employees: #{reason}")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <!-- Page Header -->
      <.page_header
        title="Employee Management"
        description="Manage your business employees and their roles"
      >
        <:action>
          <.button
            variant={if @show_form, do: "outline", else: "primary"}
            phx-click="toggle_form"
          >
            <%= if @show_form, do: "Cancel", else: "Add Employee" %>
          </.button>
        </:action>
      </.page_header>

      <!-- Add/Edit Employee Form -->
      <%= if @show_form do %>
        <.employee_form
          form={@form}
          editing={@editing_employee != nil}
          loading={@loading}
          businesses={@businesses}
          on_submit="save_employee"
          on_change="validate_employee"
          on_cancel="cancel_form"
        />
      <% end %>

      <!-- Employee Table -->
      <div class="bg-white shadow rounded-lg">
        <div class="sm:p-6 px-4 py-5">
          <%= if @employees == [] do %>
            <.empty_state
              icon={:user_group}
              title="No employees found"
              description="Get started by adding your first employee to the business."
            >
              <:action>
                <.button variant="primary" phx-click="toggle_form">
                  Add Employee
                </.button>
              </:action>
            </.empty_state>
          <% else %>
            <Flop.Phoenix.table
              items={@employees}
              meta={@meta}
              path={~p"/employees"}
              id="employees-table"
              opts={[
                container: true,
                container_attrs: [class: "overflow-hidden"],
                table_attrs: [class: "min-w-full divide-y divide-gray-200"],
                thead_attrs: [class: "bg-gray-50"],
                tbody_attrs: [class: "bg-white divide-y divide-gray-200"],
                th_wrapper_attrs: [class: "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"],
                tbody_td_attrs: [class: "px-6 py-4 whitespace-nowrap text-sm text-gray-900"]
              ]}
            >
              <:col :let={employee} label="Name" field={:first_name}>
                <div class="flex items-center">
                  <div class="flex-shrink-0 w-10 h-10">
                    <div class="flex justify-center items-center bg-gray-300 rounded-full w-10 h-10">
                      <span class="font-medium text-gray-700 text-sm">
                        <%= String.first(employee.first_name) <> String.first(employee.last_name) %>
                      </span>
                    </div>
                  </div>
                  <div class="ml-4">
                    <div class="font-medium text-gray-900 text-sm">
                      <%= employee.first_name %> <%= employee.last_name %>
                    </div>
                    <div class="text-gray-500 text-sm">
                      <%= employee.email %>
                    </div>
                  </div>
                </div>
              </:col>

              <:col :let={employee} label="Role" field={:role}>
                <.badge variant={role_variant(employee.role)}>
                  <%= String.capitalize(to_string(employee.role)) %>
                </.badge>
              </:col>

              <:col :let={employee} label="Phone">
                <%= employee.phone || "—" %>
              </:col>

              <:col :let={employee} label="Status">
                <.badge variant={if employee.is_active, do: "default", else: "secondary"}>
                  <%= if employee.is_active, do: "Active", else: "Inactive" %>
                </.badge>
              </:col>

              <:col :let={employee} label="Hire Date" field={:hire_date}>
                <%= if employee.hire_date do %>
                  <%= Calendar.strftime(employee.hire_date, "%b %d, %Y") %>
                <% else %>
                  —
                <% end %>
              </:col>

              <:col :let={employee} label="Actions">
                <div class="flex space-x-2">
                  <.button
                    variant="outline"
                    size="sm"
                    phx-click="edit_employee"
                    phx-value-id={employee.id}
                  >
                    Edit
                  </.button>
                  <.button
                    variant="destructive"
                    size="sm"
                    phx-click={JS.push("prepare_delete_employee", value: %{id: employee.id})}
                  >
                    Delete
                  </.button>
                  <.confirm_dialog
                    id="delete-employee-modal"
                    show={@show_confirm_delete_modal}
                    title="Delete Employee"
                    message={"Are you sure you want to delete employee \"#{@employee_to_delete.first_name} #{@employee_to_delete.last_name}\"?"}
                    on_confirm={JS.push("confirm_delete_employee", value: %{id: @employee_to_delete.id})}
                    on_cancel={JS.hide("delete-employee-modal")}
                  />
                </div>
              </:col>
            </Flop.Phoenix.table>

            <!-- Pagination -->
            <div class="mt-6">
              <Flop.Phoenix.pagination
                meta={@meta}
                path={~p"/employees"}
                class="flex justify-between items-center"
                page_link_attrs={[class: "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"]}
                current_page_link_attrs={[class: "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-500 bg-gray-100 cursor-default"]}
                disabled_link_attrs={[class: "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-400 bg-gray-100 cursor-not-allowed"]}
              />
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("toggle_form", _params, socket) do
    socket =
      socket
      |> assign(:show_form, !socket.assigns.show_form)
      |> assign(:editing_employee, nil)
      |> assign(
        :form,
        AshPhoenix.Form.for_create(Employee, :create, actor: socket.assigns.current_user)
        |> to_form()
      )

    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_form", _params, socket) do
    socket =
      socket
      |> assign(:show_form, false)
      |> assign(:editing_employee, nil)
      |> assign(
        :form,
        AshPhoenix.Form.for_create(Employee, :create, actor: socket.assigns.current_user)
        |> to_form()
      )

    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_employee", %{"form" => params}, socket) do
    form =
      if socket.assigns.editing_employee do
        AshPhoenix.Form.for_update(socket.assigns.editing_employee, :update, actor: socket.assigns.current_user)
      else
        AshPhoenix.Form.for_create(Employee, :create, actor: socket.assigns.current_user)
      end
      |> AshPhoenix.Form.validate(params)
      |> to_form()

    {:noreply, assign(socket, :form, form)}
  end

  @impl true
  def handle_event("save_employee", %{"form" => params}, socket) do
    user = socket.assigns.current_user

    socket = assign(socket, :loading, true)

    # Submit the form with the correct parameters
    result = AshPhoenix.Form.submit(socket.assigns.form, params: params, actor: user)

    case result do
      {:ok, employee} ->
        action_text = if socket.assigns.editing_employee, do: "updated", else: "created"

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:show_form, false)
          |> assign(:editing_employee, nil)
          |> assign(
            :form,
            AshPhoenix.Form.for_create(Employee, :create, actor: user) |> to_form()
          )
          |> put_flash(:info, "Employee #{action_text} successfully!")
          |> push_patch(to: ~p"/employees")

        {:noreply, socket}

      {:error, form} ->
        socket =
          socket
          |> assign(:loading, false)
          |> assign(:form, to_form(form))

        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("edit_employee", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    case Employees.get_employee(user, id) do
      {:ok, employee} ->
        form =
          AshPhoenix.Form.for_update(employee, :update, actor: user)
          |> to_form()

        socket =
          socket
          |> assign(:editing_employee, employee)
          |> assign(:form, form)
          |> assign(:show_form, true)

        {:noreply, socket}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to edit this employee")
         |> push_patch(to: ~p"/employees")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load employee: #{reason}")
         |> push_patch(to: ~p"/employees")}
    end
  end

  @impl true
  def handle_event("prepare_delete_employee", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    case Employees.get_employee(user, id) do
      {:ok, employee} ->
        socket =
          socket
          |> assign(:show_confirm_delete_modal, true)
          |> assign(:employee_to_delete, employee)
          |> JS.show("delete-employee-modal")

        {:noreply, socket}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to delete this employee")
         |> push_patch(to: ~p"/employees")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load employee: #{reason}")
         |> push_patch(to: ~p"/employees")}
    end
  end

  @impl true
  def handle_event("confirm_delete_employee", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    # Hide modal immediately
    socket = assign(socket, :show_confirm_delete_modal, false)

    case Employees.delete_employee(user, id) do
      {:ok, employee} ->
        {:noreply,
         socket
         |> put_flash(
           :info,
           "Employee \"#{employee.first_name} #{employee.last_name}\" deleted successfully!"
         )
         |> push_patch(to: ~p"/employees")}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to delete this employee")
         |> push_patch(to: ~p"/employees")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete employee: #{reason}")
         |> push_patch(to: ~p"/employees")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper Functions

  # Helper function for badge variants based on role
  defp role_variant(:admin), do: "destructive"
  defp role_variant(:manager), do: "default"
  defp role_variant(:staff), do: "secondary"
  defp role_unmatchedvariant(_unmatched), do: "secondary"
end
