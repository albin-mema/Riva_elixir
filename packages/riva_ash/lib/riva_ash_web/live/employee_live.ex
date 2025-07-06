defmodule RivaAshWeb.EmployeeLive do
  use RivaAshWeb, :live_view

  import RivaAshWeb.CoreComponents
  import Phoenix.Controller, only: [get_csrf_token: 0]

  alias RivaAsh.Resources.Employee
  alias RivaAsh.Domain

  @impl true
  def mount(_params, _session, socket) do
    {:ok, load_employees(socket)}
  end

  @impl true
  def handle_params(_params, _uri, socket) do
    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta name="csrf-token" content={get_csrf_token()} />
        <title>Employee Management - RivaAsh</title>
        <link phx-track-static rel="stylesheet" href="/assets/app.css" />
        <script defer phx-track-static type="text/javascript" src="/assets/app.js"></script>
        <%= Application.get_env(:live_debugger, :live_debugger_tags) %>
      </head>
      <body class="bg-gray-50 antialiased">
        <header class="bg-white shadow">
          <div class="mx-auto px-4 sm:px-6 lg:px-8 py-6 max-w-7xl">
            <h1 class="font-bold text-gray-900 text-3xl">
              RivaAsh - Employee Management
            </h1>
          </div>
        </header>
        <main class="mx-auto sm:px-6 lg:px-8 py-6 max-w-7xl">
          <div class="px-4 sm:px-0 py-6">
            <div class="p-8 border-4 border-gray-200 border-dashed rounded-lg">
              <.live_react_component
                module="EmployeeManager"
                props={%{employees: @employees}}
                id="employee-manager"
              />
            </div>
          </div>
        </main>
      </body>
    </html>
    """
  end

  @impl true
  def handle_event("create_employee", params, socket) do
    case create_employee(params) do
      {:ok, _employee} ->
        socket = load_employees(socket)
        {:noreply, socket}

      {:error, _changeset} ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("delete_employee", %{"id" => id}, socket) do
    case delete_employee(id) do
      :ok ->
        socket = load_employees(socket)
        {:noreply, socket}

      {:error, _reason} ->
        {:noreply, socket}
    end
  end

  # Private functions

  defp load_employees(socket) do
    case Employee.read(authorize?: false) do
      {:ok, employees} ->
        # Convert employees to a format suitable for React
        employees_data = Enum.map(employees, &employee_to_map/1)
        assign(socket, :employees, employees_data)

      {:error, _reason} ->
        assign(socket, :employees, [])
    end
  end

  defp create_employee(params) do
    # We need to create a business first since employees require a business_id
    # For this demo, we'll create a default business or use an existing one
    _business_params = %{name: "Demo Business", description: "Default business for demo"}

    case ensure_demo_business() do
      {:ok, business} ->
        employee_params = Map.put(params, "business_id", business.id)
        Employee.create(employee_params, authorize?: false)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp delete_employee(id) do
    case Employee.by_id(id, authorize?: false) do
      {:ok, employee} ->
        Employee.destroy(employee, authorize?: false)

      {:error, _reason} ->
        {:error, :not_found}
    end
  end

  defp ensure_demo_business do
    alias RivaAsh.Resources.Business

    # Try to find existing demo business
    case Business.read(authorize?: false) do
      {:ok, [business | _]} ->
        {:ok, business}

      {:ok, []} ->
        # Create demo business
        Business.create(%{name: "Demo Business", description: "Default business for demo"}, authorize?: false)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp employee_to_map(employee) do
    %{
      id: employee.id,
      first_name: employee.first_name,
      last_name: employee.last_name,
      email: employee.email,
      phone: employee.phone,
      role: employee.role,
      full_name: employee.full_name,
      inserted_at: employee.inserted_at,
      updated_at: employee.updated_at
    }
  end

  defp format_errors(changeset) do
    changeset.errors
    |> Enum.map(fn {field, {message, _}} -> "#{field}: #{message}" end)
    |> Enum.join(", ")
  end
end
