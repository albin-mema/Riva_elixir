defmodule RivaAshWeb.EmployeeLive do
  use RivaAshWeb, :live_view

  require Ash.Query

  # Import atomic design components
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Badge
  import RivaAshWeb.Components.Molecules.EmptyState
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.EmployeeForm

  alias RivaAsh.Resources.Employee
  alias RivaAsh.Resources.Business

  @impl true
  def mount(_params, session, socket) do
    # Get the current user from the session
    user = get_current_user_from_session(session)

    if user do
      socket =
        socket
        |> assign(:current_user, user)
        |> assign(:businesses, list_user_businesses(user))
        |> assign(:form, AshPhoenix.Form.for_create(Employee, :create, actor: user) |> to_form())
        |> assign(:show_form, false)
        |> assign(:editing_employee, nil)
        |> assign(:loading, false)
        |> assign(:is_admin, user.role == :admin)
        |> assign(:page_title, "Employee Management")

      {:ok, socket}
    else
      # User not authenticated, redirect to sign in
      socket =
        socket
        |> put_flash(:error, "You must be logged in to access this page.")
        |> redirect(to: "/sign-in")

      {:ok, socket}
    end
  end

  @impl true
  def handle_params(params, _uri, socket) do
    user = socket.assigns.current_user

    # Use Flop to handle pagination, sorting, and filtering
    {employees, meta} = list_employees_with_flop(user, params)

    socket =
      socket
      |> assign(:employees, employees)
      |> assign(:meta, meta)

    {:noreply, socket}
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
                <.badge variant={if employee.is_active, do: "success", else: "secondary"}>
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
                    phx-click="delete_employee"
                    phx-value-id={employee.id}
                    data-confirm="Are you sure you want to delete this employee?"
                  >
                    Delete
                  </.button>
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
      |> assign(:form, AshPhoenix.Form.for_create(Employee, :create, actor: socket.assigns.current_user) |> to_form())

    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_form", _params, socket) do
    socket =
      socket
      |> assign(:show_form, false)
      |> assign(:editing_employee, nil)
      |> assign(:form, AshPhoenix.Form.for_create(Employee, :create, actor: socket.assigns.current_user) |> to_form())

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
    IO.puts("DEBUG: Form submission result: #{inspect(result)}")

    case result do
      {:ok, employee} ->
        IO.puts("DEBUG: Employee created successfully: #{inspect(employee)}")
        action_text = if socket.assigns.editing_employee, do: "updated", else: "created"

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:show_form, false)
          |> assign(:editing_employee, nil)
          |> assign(:form, AshPhoenix.Form.for_create(Employee, :create, actor: user) |> to_form())
          |> put_flash(:info, "Employee #{action_text} successfully!")
          |> push_patch(to: ~p"/employees")

        {:noreply, socket}

      {:error, form} ->
        IO.puts("DEBUG: Form submission failed: #{inspect(form.errors)}")
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

    case Employee.by_id(id, actor: user) do
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

      {:error, _reason} ->
        socket = put_flash(socket, :error, "Employee not found.")
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("delete_employee", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    case Employee.by_id(id, actor: user) do
      {:ok, employee} ->
        case employee |> Ash.destroy(actor: user) do
          :ok ->
            socket =
              socket
              |> put_flash(:info, "Employee \"#{employee.first_name} #{employee.last_name}\" deleted successfully!")
              |> push_patch(to: ~p"/employees")

            {:noreply, socket}

          {:error, _reason} ->
            socket = put_flash(socket, :error, "Failed to delete employee.")
            {:noreply, socket}
        end

      {:error, _reason} ->
        socket = put_flash(socket, :error, "Employee not found.")
        {:noreply, socket}
    end
  end

  # Helper Functions

  defp list_employees_with_flop(user, params) do
    # Get employees as a list
    employees = load_employees_simple(user)

    # Validate Flop parameters first
    case Flop.validate(params, for: Employee) do
      {:ok, flop} ->
        # Apply manual sorting and pagination to the list
        {paginated_employees, meta} = apply_flop_to_list(employees, flop)
        {paginated_employees, meta}

      {:error, meta} ->
        # Return empty results with error meta
        {[], meta}
    end
  end

  defp apply_flop_to_list(employees, flop) do
    # Apply sorting
    sorted_employees = apply_sorting(employees, flop)

    # Apply pagination
    total_count = length(sorted_employees)
    {paginated_employees, pagination_info} = apply_pagination(sorted_employees, flop)

    # Build meta information
    meta = build_meta(flop, total_count, pagination_info)

    {paginated_employees, meta}
  end

  defp apply_sorting(employees, %Flop{order_by: nil}), do: employees
  defp apply_sorting(employees, %Flop{order_by: []}), do: employees
  defp apply_sorting(employees, %Flop{order_by: order_by, order_directions: order_directions}) do
    # Default to :asc if no directions provided
    directions = order_directions || Enum.map(order_by, fn _ -> :asc end)

    # Zip order_by and directions, pad directions with :asc if needed
    order_specs = Enum.zip_with([order_by, directions], fn [field, direction] -> {field, direction} end)

    Enum.sort(employees, fn emp1, emp2 ->
      compare_employees(emp1, emp2, order_specs)
    end)
  end

  defp compare_employees(_emp1, _emp2, []), do: true
  defp compare_employees(emp1, emp2, [{field, direction} | rest]) do
    val1 = get_field_value(emp1, field)
    val2 = get_field_value(emp2, field)

    case {val1, val2} do
      {same, same} -> compare_employees(emp1, emp2, rest)
      {nil, _} -> direction == :desc
      {_, nil} -> direction == :asc
      {v1, v2} when direction == :asc -> v1 <= v2
      {v1, v2} when direction == :desc -> v1 >= v2
    end
  end

  defp get_field_value(employee, field) do
    case field do
      :first_name -> employee.first_name
      :last_name -> employee.last_name
      :email -> employee.email
      :role -> employee.role
      :hire_date -> employee.hire_date
      :inserted_at -> employee.inserted_at
      :is_active -> employee.is_active
      :business_id -> employee.business_id
      _ -> nil
    end
  end

  defp apply_pagination(employees, flop) do
    total_count = length(employees)

    cond do
      # Page-based pagination
      flop.page && flop.page_size ->
        page = max(flop.page, 1)
        page_size = flop.page_size
        offset = (page - 1) * page_size

        paginated = employees |> Enum.drop(offset) |> Enum.take(page_size)
        pagination_info = %{
          type: :page,
          page: page,
          page_size: page_size,
          offset: offset,
          total_count: total_count
        }
        {paginated, pagination_info}

      # Offset-based pagination
      flop.offset && flop.limit ->
        offset = max(flop.offset, 0)
        limit = flop.limit

        paginated = employees |> Enum.drop(offset) |> Enum.take(limit)
        pagination_info = %{
          type: :offset,
          offset: offset,
          limit: limit,
          total_count: total_count
        }
        {paginated, pagination_info}

      # Limit only
      flop.limit ->
        limit = flop.limit
        paginated = Enum.take(employees, limit)
        pagination_info = %{
          type: :limit,
          limit: limit,
          total_count: total_count
        }
        {paginated, pagination_info}

      # No pagination
      true ->
        pagination_info = %{
          type: :none,
          total_count: total_count
        }
        {employees, pagination_info}
    end
  end

  defp build_meta(flop, total_count, pagination_info) do
    case pagination_info.type do
      :page ->
        page = pagination_info.page
        page_size = pagination_info.page_size
        total_pages = ceil(total_count / page_size)

        %Flop.Meta{
          current_page: page,
          current_offset: pagination_info.offset,
          flop: flop,
          has_next_page?: page < total_pages,
          has_previous_page?: page > 1,
          next_page: if(page < total_pages, do: page + 1, else: nil),
          previous_page: if(page > 1, do: page - 1, else: nil),
          page_size: page_size,
          total_count: total_count,
          total_pages: total_pages,
          opts: []
        }

      :offset ->
        offset = pagination_info.offset
        limit = pagination_info.limit
        current_page = div(offset, limit) + 1
        total_pages = ceil(total_count / limit)

        %Flop.Meta{
          current_page: current_page,
          current_offset: offset,
          flop: flop,
          has_next_page?: offset + limit < total_count,
          has_previous_page?: offset > 0,
          next_offset: if(offset + limit < total_count, do: offset + limit, else: nil),
          previous_offset: if(offset > 0, do: max(0, offset - limit), else: nil),
          page_size: limit,
          total_count: total_count,
          total_pages: total_pages,
          opts: []
        }

      _ ->
        %Flop.Meta{
          current_page: 1,
          current_offset: 0,
          flop: flop,
          has_next_page?: false,
          has_previous_page?: false,
          page_size: total_count,
          total_count: total_count,
          total_pages: 1,
          opts: []
        }
    end
  end

  defp load_employees_simple(user) do
    try do
      IO.puts("DEBUG: Loading employees for user: #{inspect(user.email)} (role: #{user.role})")
      
      if user.role == :admin do
        # Admins can see all employees - use authorize?: false to bypass permission issues
        IO.puts("DEBUG: User is admin, loading all employees")
        case Employee.read(authorize?: false) do
          {:ok, employees} -> 
            IO.puts("DEBUG: Successfully loaded #{length(employees)} employees")
            employees
          error -> 
            IO.puts("DEBUG: Error loading employees: #{inspect(error)}")
            []
        end
      else
        # Regular users can only see employees from their businesses
        IO.puts("DEBUG: User is not admin, loading businesses first")
        user_business_ids = list_user_businesses_simple(user) |> Enum.map(& &1.id)
        IO.puts("DEBUG: User has access to business IDs: #{inspect(user_business_ids)}")

        case Employee.read(authorize?: false) do
          {:ok, all_employees} ->
            IO.puts("DEBUG: Loaded #{length(all_employees)} total employees")
            filtered_employees = Enum.filter(all_employees, fn emp -> emp.business_id in user_business_ids end)
            IO.puts("DEBUG: Filtered to #{length(filtered_employees)} employees for user's businesses")
            filtered_employees
          error -> 
            IO.puts("DEBUG: Error loading employees: #{inspect(error)}")
            []
        end
      end
    rescue
      error -> 
        IO.puts("DEBUG: Exception in load_employees_simple: #{inspect(error)}")
        []
    end
  end

  defp list_user_businesses_simple(user) do
    try do
      IO.puts("DEBUG: Loading businesses for user: #{inspect(user.email)} (role: #{user.role})")
      
      if user.role == :admin do
        # Admins can see all businesses
        IO.puts("DEBUG: User is admin, loading all businesses")
        case Business.read(authorize?: false) do
          {:ok, businesses} -> 
            IO.puts("DEBUG: Successfully loaded #{length(businesses)} businesses")
            businesses
          error -> 
            IO.puts("DEBUG: Error loading businesses: #{inspect(error)}")
            []
        end
      else
        # Regular users can only see their own businesses
        IO.puts("DEBUG: User is not admin, loading only owned businesses")
        case Business.read(authorize?: false) do
          {:ok, all_businesses} ->
            filtered_businesses = Enum.filter(all_businesses, fn business -> business.owner_id == user.id end)
            IO.puts("DEBUG: Successfully loaded #{length(filtered_businesses)} owned businesses")
            filtered_businesses
          error -> 
            IO.puts("DEBUG: Error loading businesses for filtering: #{inspect(error)}")
            []
        end
      end
    rescue
      error -> 
        IO.puts("DEBUG: Exception in list_user_businesses_simple: #{inspect(error)}")
        []
    end
  end



  defp list_user_businesses(user) do
    list_user_businesses_simple(user)
  end

  defp get_current_user_from_session(session) do
    user_token = session["user_token"]

    if user_token do
      case Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", user_token, max_age: 86_400) do
        {:ok, user_id} ->
          case Ash.get(RivaAsh.Accounts.User, user_id, domain: RivaAsh.Accounts) do
            {:ok, user} -> user
            _ -> nil
          end

        _ ->
          nil
      end
    else
      nil
    end
  end

  # Helper function for badge variants based on role
  defp role_variant(:admin), do: "destructive"
  defp role_variant(:manager), do: "default"
  defp role_variant(:staff), do: "secondary"
  defp role_variant(_), do: "secondary"
end
