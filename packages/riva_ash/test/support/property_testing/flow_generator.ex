defmodule RivaAsh.PropertyTesting.FlowGenerator do
  @moduledoc """
  Property-based generators for creating random user navigation flows.

  This module uses StreamData to generate realistic user interaction sequences
  that can be executed in browser tests to find edge cases and regressions.
  """

  use ExUnitProperties
  import StreamData

  alias RivaAsh.PropertyTesting.{StateMachine, RouteEnumerator}

  # Conditionally alias Factory if available
  if Code.ensure_loaded?(RivaAsh.Factory) do
    alias RivaAsh.Factory
  end

  @type flow_step :: {atom(), map()}
  @type user_flow :: [flow_step()]

  @doc """
  Generate a complete user flow from anonymous to logout.
  """
  def user_flow_generator(opts \\ []) do
    max_steps = Keyword.get(opts, :max_steps, 10)
    min_steps = Keyword.get(opts, :min_steps, 3)

    gen all(steps <- integer(min_steps..max_steps)) do
      generate_flow_steps(steps, StateMachine.initial_state(), [])
    end
  end

  @doc """
  Generate a flow starting from a specific user state.
  """
  def flow_from_state_generator(initial_state, opts \\ []) do
    max_steps = Keyword.get(opts, :max_steps, 8)
    min_steps = Keyword.get(opts, :min_steps, 2)

    gen all(steps <- integer(min_steps..max_steps)) do
      generate_flow_steps(steps, initial_state, [])
    end
  end

  @doc """
  Generate authentication flows (register -> login or just login).
  """
  def auth_flow_generator do
    one_of([
      registration_flow_generator(),
      login_flow_generator()
    ])
  end

  @doc """
  Generate a registration flow.
  """
  def registration_flow_generator do
    gen all(user_data <- user_data_generator()) do
      [
        {:visit, %{path: "/register"}},
        {:register, user_data},
        {:visit, %{path: "/sign-in"}},
        {:login, %{email: user_data.email, password: user_data.password}}
      ]
    end
  end

  @doc """
  Generate a login flow with existing user.
  """
  def login_flow_generator do
    gen all(user_data <- existing_user_data_generator()) do
      [
        {:visit, %{path: "/sign-in"}},
        {:login, user_data}
      ]
    end
  end

  @doc """
  Generate CRUD operation flows.
  """
  def crud_flow_generator(resource_type) do
    one_of([
      create_flow_generator(resource_type),
      read_flow_generator(resource_type),
      update_flow_generator(resource_type),
      delete_flow_generator(resource_type)
    ])
  end

  @doc """
  Generate navigation-only flows (no data modification).
  """
  def navigation_flow_generator(user_state \\ :authenticated) do
    gen all(
          steps <- integer(3..8),
          routes <- list_of(route_for_state_generator(user_state), length: steps)
        ) do
      Enum.map(routes, fn route -> {:visit, %{path: route.path}} end)
    end
  end

  @doc """
  Generate error recovery flows.
  """
  def error_recovery_flow_generator do
    one_of([
      session_timeout_recovery_generator(),
      permission_error_recovery_generator(),
      not_found_recovery_generator()
    ])
  end

  # Private generators

  defp generate_flow_steps(0, _current_state, acc) do
    flow = Enum.reverse(acc)
    IO.puts("🎲 Generated flow complete with #{length(flow)} steps")
    flow
  end

  defp generate_flow_steps(remaining_steps, current_state, acc) do
    # Get available actions for current state
    available_actions = StateMachine.available_actions(current_state)

    # Select a random action based on weights
    action = StateMachine.random_action(current_state)

    # Generate step data based on action
    step_data = generate_step_data(action, current_state)
    step = {action, step_data}

    # Log the step generation
    step_num = length(acc) + 1
    IO.puts("   Step #{step_num}: #{current_state} → #{action}")

    # Get next state
    case StateMachine.next_state(current_state, action) do
      {:ok, next_state} ->
        IO.puts("      → Next state: #{next_state}")
        generate_flow_steps(remaining_steps - 1, next_state, [step | acc])

      {:error, reason} ->
        IO.puts("      ⚠️  Invalid transition (#{reason}), ending flow")
        # If transition is invalid, try a different action or end flow
        Enum.reverse([step | acc])
    end
  end

  defp generate_step_data(:register, _state) do
    data = user_data_generator() |> Enum.take(1) |> hd()
    IO.puts("      📝 Generated registration data: #{data.name} <#{data.email}>")
    data
  end

  defp generate_step_data(:login, _state) do
    data = existing_user_data_generator() |> Enum.take(1) |> hd()
    IO.puts("      🔑 Using login credentials: #{data.email}")
    data
  end

  defp generate_step_data(:visit_public_page, _state) do
    route = RouteEnumerator.random_route(:public)
    IO.puts("      🌐 Selected public route: #{route.path}")
    %{path: route.path}
  end

  defp generate_step_data(:visit_protected_page, _state) do
    route = RouteEnumerator.random_route(:authenticated)
    IO.puts("      🔒 Selected protected route: #{route.path}")
    %{path: route.path}
  end

  defp generate_step_data(:visit_admin_page, _state) do
    route = RouteEnumerator.random_route(:admin)
    IO.puts("      👑 Selected admin route: #{route.path}")
    %{path: route.path}
  end

  defp generate_step_data(:create_resource, _state) do
    resource_type = Enum.random([:business, :client, :item, :employee])
    resource_data = generate_resource_data(resource_type)
    IO.puts("      ➕ Will create #{resource_type} with data: #{inspect(resource_data, limit: 2)}")
    %{resource_type: resource_type, data: resource_data}
  end

  defp generate_step_data(:update_resource, _state) do
    resource_type = Enum.random([:business, :client, :item, :employee])
    resource_data = generate_resource_data(resource_type)
    resource_id = Enum.random(1..100)
    IO.puts("      ✏️  Will update #{resource_type} ##{resource_id}")
    %{resource_type: resource_type, id: resource_id, data: resource_data}
  end

  defp generate_step_data(:delete_resource, _state) do
    resource_type = Enum.random([:business, :client, :item, :employee])
    resource_id = Enum.random(1..100)
    IO.puts("      🗑️  Will delete #{resource_type} ##{resource_id}")
    %{resource_type: resource_type, id: resource_id}
  end

  defp generate_step_data(_action, _state) do
    %{}
  end

  defp user_data_generator do
    gen all(
          name <- string(:alphanumeric, min_length: 2, max_length: 50),
          email <- email_generator(),
          password <- string(:alphanumeric, min_length: 8, max_length: 20)
        ) do
      %{
        name: name,
        email: email,
        password: password,
        password_confirmation: password
      }
    end
  end

  defp existing_user_data_generator do
    # Generate data for users that should already exist in test database
    gen all(
          email <- member_of(["test@example.com", "admin@example.com", "user@example.com"]),
          password <- constant("password123")
        ) do
      %{email: email, password: password}
    end
  end

  defp email_generator do
    gen all(
          username <- string(:alphanumeric, min_length: 3, max_length: 10),
          domain <- member_of(["example.com", "test.com", "demo.org"])
        ) do
      "#{username}@#{domain}"
    end
  end

  defp route_for_state_generator(user_state) do
    category =
      case user_state do
        :anonymous -> :public
        :authenticated -> :authenticated
        :admin -> :admin
        _ -> :public
      end

    routes = RouteEnumerator.parameterless_routes(category)
    member_of(routes)
  end

  defp generate_resource_data(:business) do
    if Code.ensure_loaded?(RivaAsh.Factory) do
      RivaAsh.Factory.business_attrs() |> Enum.take(1) |> hd()
    else
      %{name: "Test Business", description: "Test Description"}
    end
  end

  defp generate_resource_data(:client) do
    if Code.ensure_loaded?(RivaAsh.Factory) do
      RivaAsh.Factory.client_attrs() |> Enum.take(1) |> hd()
    else
      %{name: "Test Client", email: "test@example.com"}
    end
  end

  defp generate_resource_data(:item) do
    if Code.ensure_loaded?(RivaAsh.Factory) do
      RivaAsh.Factory.item_attrs() |> Enum.take(1) |> hd()
    else
      %{name: "Test Item", price: 100}
    end
  end

  defp generate_resource_data(:employee) do
    if Code.ensure_loaded?(RivaAsh.Factory) do
      RivaAsh.Factory.employee_attrs() |> Enum.take(1) |> hd()
    else
      %{name: "Test Employee", email: "employee@example.com"}
    end
  end

  defp create_flow_generator(resource_type) do
    gen all(resource_data <- resource_data_generator(resource_type)) do
      [
        {:visit, %{path: "/#{resource_type}s"}},
        {:visit, %{path: "/#{resource_type}s/new"}},
        {:create_resource, %{resource_type: resource_type, data: resource_data}},
        {:visit, %{path: "/#{resource_type}s"}}
      ]
    end
  end

  defp read_flow_generator(resource_type) do
    gen all(id <- integer(1..100)) do
      [
        {:visit, %{path: "/#{resource_type}s"}},
        {:visit, %{path: "/#{resource_type}s/#{id}"}}
      ]
    end
  end

  defp update_flow_generator(resource_type) do
    gen all(
          id <- integer(1..100),
          resource_data <- resource_data_generator(resource_type)
        ) do
      [
        {:visit, %{path: "/#{resource_type}s"}},
        {:visit, %{path: "/#{resource_type}s/#{id}/edit"}},
        {:update_resource, %{resource_type: resource_type, id: id, data: resource_data}},
        {:visit, %{path: "/#{resource_type}s"}}
      ]
    end
  end

  defp delete_flow_generator(resource_type) do
    gen all(id <- integer(1..100)) do
      [
        {:visit, %{path: "/#{resource_type}s"}},
        {:delete_resource, %{resource_type: resource_type, id: id}},
        {:visit, %{path: "/#{resource_type}s"}}
      ]
    end
  end

  defp resource_data_generator(resource_type) do
    case resource_type do
      :business -> map_of(atom(:alphanumeric), string(:alphanumeric))
      :client -> map_of(atom(:alphanumeric), string(:alphanumeric))
      :item -> map_of(atom(:alphanumeric), string(:alphanumeric))
      :employee -> map_of(atom(:alphanumeric), string(:alphanumeric))
    end
  end

  defp session_timeout_recovery_generator do
    gen all(user_data <- existing_user_data_generator()) do
      [
        {:session_expire, %{}},
        {:visit, %{path: "/sign-in"}},
        {:login, user_data}
      ]
    end
  end

  defp permission_error_recovery_generator do
    constant([
      # This should fail for non-admin
      {:visit, %{path: "/admin"}},
      {:visit, %{path: "/access-denied"}},
      {:visit, %{path: "/dashboard"}}
    ])
  end

  defp not_found_recovery_generator do
    constant([
      {:visit, %{path: "/nonexistent-page"}},
      {:visit, %{path: "/404"}},
      {:visit, %{path: "/"}}
    ])
  end

  @doc """
  Generate flows that test specific scenarios.
  """
  def scenario_flow_generator(scenario) do
    case scenario do
      :happy_path ->
        constant([
          {:visit, %{path: "/"}},
          {:visit, %{path: "/register"}},
          {:register,
           %{
             name: "Test User",
             email: "test@example.com",
             password: "password123",
             password_confirmation: "password123"
           }},
          {:visit, %{path: "/sign-in"}},
          {:login, %{email: "test@example.com", password: "password123"}},
          {:visit, %{path: "/dashboard"}},
          {:logout, %{}}
        ])

      :admin_workflow ->
        constant([
          {:visit, %{path: "/sign-in"}},
          {:admin_login, %{email: "admin@example.com", password: "admin123"}},
          {:visit, %{path: "/admin"}},
          {:visit, %{path: "/dashboard"}},
          {:logout, %{}}
        ])

      :error_prone ->
        one_of([
          constant([{:visit, %{path: "/nonexistent"}}]),
          # Without auth
          constant([{:visit, %{path: "/admin"}}]),
          constant([{:login, %{email: "invalid", password: "wrong"}}])
        ])
    end
  end

  @doc """
  Validate that a generated flow is executable.
  """
  def validate_flow(flow) do
    actions = Enum.map(flow, fn {action, _data} -> action end)
    StateMachine.validate_flow(actions)
  end
end
