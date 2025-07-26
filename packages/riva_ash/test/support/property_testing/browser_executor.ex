defmodule RivaAsh.PropertyTesting.BrowserExecutor do
  @moduledoc """
  Executes property-based generated user flows using Playwright browser automation.

  This module takes generated user flows and executes them in real browsers,
  handling authentication, navigation, form filling, and error recovery.
  """

  import PhoenixTest
  alias RivaAsh.PropertyTesting.{StateMachine, DataManager}

  @type execution_result :: {:ok, map()} | {:error, term()}
  @type flow_step :: {atom(), map()}
  @type execution_context :: %{
    conn: term(),
    current_state: atom(),
    user_data: map(),
    created_resources: list(),
    screenshots: list()
  }

  @doc """
  Execute a complete user flow in the browser.
  """
  def execute_flow(flow, opts \\ []) do
    context = initialize_context(opts)

    IO.puts("\n🎬 Starting Browser Flow Execution")
    IO.puts("   Flow length: #{length(flow)} steps")
    IO.puts("   Flow sequence: #{inspect(Enum.map(flow, fn {action, _} -> action end))}")
    IO.puts("   Options: #{inspect(opts)}")

    try do
      result = execute_flow_steps(flow, context)
      cleanup_context(context)

      IO.puts("✅ Flow execution completed successfully!")
      IO.puts("   Final state: #{result.final_state}")
      IO.puts("   Resources created: #{length(result.created_resources)}")
      IO.puts("   Screenshots taken: #{length(result.screenshots)}")

      result
    rescue
      error ->
        IO.puts("❌ Flow execution failed with error: #{inspect(error)}")
        cleanup_context(context)
        {:error, {error, __STACKTRACE__}}
    end
  end

  @doc """
  Execute a single flow step.
  """
  def execute_step({action, data}, context) do
    case action do
      :visit -> execute_visit(data, context)
      :register -> execute_register(data, context)
      :login -> execute_login(data, context)
      :logout -> execute_logout(data, context)
      :create_resource -> execute_create_resource(data, context)
      :update_resource -> execute_update_resource(data, context)
      :delete_resource -> execute_delete_resource(data, context)
      :session_expire -> execute_session_expire(data, context)
      :clear_error -> execute_clear_error(data, context)
      _ -> execute_generic_action(action, data, context)
    end
  end

  # Private functions

  defp initialize_context(opts) do
    headless = Keyword.get(opts, :headless, false)
    screenshots = Keyword.get(opts, :screenshots, true)

    %{
      conn: Phoenix.ConnTest.build_conn(),
      current_state: StateMachine.initial_state(),
      user_data: %{},
      created_resources: [],
      screenshots: [],
      options: %{headless: headless, screenshots: screenshots}
    }
  end

  defp cleanup_context(context) do
    # Clean up any created resources
    Enum.each(context.created_resources, &DataManager.cleanup_resource/1)

    # Clean up screenshots if needed
    if not context.options.screenshots do
      Enum.each(context.screenshots, &File.rm/1)
    end
  end

  defp execute_flow_steps([], context) do
    IO.puts("🏁 All flow steps completed")
    {:ok, %{
      final_state: context.current_state,
      created_resources: context.created_resources,
      screenshots: context.screenshots
    }}
  end

  defp execute_flow_steps([step | remaining_steps], context) do
    {action, data} = step
    step_number = length(context.created_resources) + 1

    IO.puts("\n🔄 Step #{step_number}: #{action}")
    IO.puts("   Current state: #{context.current_state}")
    IO.puts("   Data: #{inspect(data, limit: 3)}")

    case execute_step(step, context) do
      {:ok, updated_context} ->
        IO.puts("   ✅ Step completed successfully")
        IO.puts("   New state: #{updated_context.current_state}")
        execute_flow_steps(remaining_steps, updated_context)
      {:error, reason} ->
        IO.puts("   ❌ Step failed: #{inspect(reason)}")
        {:error, {reason, step, context.current_state}}
    end
  end

  defp execute_visit(%{path: path}, context) do
    IO.puts("   🌐 Visiting: #{path}")

    try do
      updated_conn = context.conn |> visit(path)

      # Take screenshot if enabled
      context = maybe_take_screenshot(context, "visit_#{sanitize_path(path)}")

      # Update state based on the visit result
      new_state = determine_state_after_visit(path, context.current_state)

      IO.puts("   📍 Successfully navigated to: #{path}")
      if new_state != context.current_state do
        IO.puts("   🔄 State changed: #{context.current_state} → #{new_state}")
      end

      {:ok, %{context | conn: updated_conn, current_state: new_state}}
    rescue
      error ->
        IO.puts("   ❌ Navigation failed to #{path}: #{inspect(error)}")
        {:error, {:visit_failed, path, error}}
    end
  end

  defp execute_register(user_data, context) do
    IO.puts("   👤 Registering user:")
    IO.puts("      Name: #{user_data.name}")
    IO.puts("      Email: #{user_data.email}")
    IO.puts("      Password: [HIDDEN]")

    try do
      updated_conn = context.conn
      |> visit("/register")
      |> fill_in("Name", with: user_data.name)
      |> fill_in("Email", with: user_data.email)
      |> fill_in("Password", with: user_data.password)
      |> fill_in("Confirm Password", with: user_data.password_confirmation)
      |> click_button("Create Account")

      # Verify registration succeeded
      updated_conn = updated_conn |> assert_path("/sign-in")

      context = maybe_take_screenshot(context, "register_success")

      IO.puts("   ✅ Registration successful, redirected to sign-in")

      {:ok, %{context |
        conn: updated_conn,
        current_state: :anonymous,
        user_data: Map.merge(context.user_data, user_data)
      }}
    rescue
      error ->
        IO.puts("   ❌ Registration failed: #{inspect(error)}")
        context = maybe_take_screenshot(context, "register_error")
        {:error, {:register_failed, user_data, error}}
    end
  end

  defp execute_login(login_data, context) do
    IO.puts("   🔐 Logging in user:")
    IO.puts("      Email: #{login_data.email}")
    IO.puts("      Password: [HIDDEN]")

    try do
      updated_conn = context.conn
      |> visit("/sign-in")
      |> fill_in("Email", with: login_data.email)
      |> fill_in("Password", with: login_data.password)
      |> click_button("Sign In")

      # Check if login succeeded (should redirect to dashboard or businesses)
      current_path = get_current_path(updated_conn)

      new_state = if current_path in ["/dashboard", "/businesses"] do
        :authenticated
      else
        :error
      end

      context = maybe_take_screenshot(context, "login_#{new_state}")

      if new_state == :authenticated do
        IO.puts("   ✅ Login successful, redirected to: #{current_path}")
      else
        IO.puts("   ⚠️  Login result unclear, current path: #{current_path}")
      end

      {:ok, %{context |
        conn: updated_conn,
        current_state: new_state,
        user_data: Map.merge(context.user_data, login_data)
      }}
    rescue
      error ->
        IO.puts("   ❌ Login failed: #{inspect(error)}")
        context = maybe_take_screenshot(context, "login_error")
        {:error, {:login_failed, login_data, error}}
    end
  end

  defp execute_logout(_data, context) do
    try do
      updated_conn = context.conn
      |> visit("/dashboard")  # Ensure we're on an authenticated page
      |> click_button("Sign Out")
      |> assert_path("/sign-in")

      context = maybe_take_screenshot(context, "logout_success")

      {:ok, %{context |
        conn: updated_conn,
        current_state: :anonymous,
        user_data: %{}
      }}
    rescue
      error ->
        context = maybe_take_screenshot(context, "logout_error")
        {:error, {:logout_failed, error}}
    end
  end

  defp execute_create_resource(%{resource_type: resource_type, data: data}, context) do
    IO.puts("   ➕ Creating #{resource_type}:")
    IO.puts("      Path: /#{resource_type}s/new")
    IO.puts("      Data: #{inspect(data, limit: 2)}")

    try do
      path = "/#{resource_type}s/new"

      updated_conn = context.conn
      |> visit(path)
      |> fill_resource_form(resource_type, data)
      |> click_button("Create")

      # Verify creation succeeded
      updated_conn = updated_conn |> assert_path("/#{resource_type}s")

      # Track created resource for cleanup
      resource_id = extract_created_resource_id(updated_conn, resource_type)
      created_resource = {resource_type, resource_id}

      context = maybe_take_screenshot(context, "create_#{resource_type}_success")

      IO.puts("   ✅ #{String.capitalize(to_string(resource_type))} created successfully (ID: #{resource_id})")

      {:ok, %{context |
        conn: updated_conn,
        created_resources: [created_resource | context.created_resources]
      }}
    rescue
      error ->
        IO.puts("   ❌ Failed to create #{resource_type}: #{inspect(error)}")
        context = maybe_take_screenshot(context, "create_#{resource_type}_error")
        {:error, {:create_resource_failed, resource_type, data, error}}
    end
  end

  defp execute_update_resource(%{resource_type: resource_type, id: id, data: data}, context) do
    try do
      path = "/#{resource_type}s/#{id}/edit"

      updated_conn = context.conn
      |> visit(path)
      |> fill_resource_form(resource_type, data)
      |> click_button("Update")

      context = maybe_take_screenshot(context, "update_#{resource_type}_success")

      {:ok, %{context | conn: updated_conn}}
    rescue
      error ->
        context = maybe_take_screenshot(context, "update_#{resource_type}_error")
        {:error, {:update_resource_failed, resource_type, id, data, error}}
    end
  end

  defp execute_delete_resource(%{resource_type: resource_type, id: id}, context) do
    try do
      updated_conn = context.conn
      |> visit("/#{resource_type}s")
      |> click_link("Delete", href: "/#{resource_type}s/#{id}")
      |> assert_path("/#{resource_type}s")

      context = maybe_take_screenshot(context, "delete_#{resource_type}_success")

      {:ok, %{context | conn: updated_conn}}
    rescue
      error ->
        context = maybe_take_screenshot(context, "delete_#{resource_type}_error")
        {:error, {:delete_resource_failed, resource_type, id, error}}
    end
  end

  defp execute_session_expire(_data, context) do
    # Simulate session expiration by clearing session
    updated_conn = Phoenix.ConnTest.build_conn()

    {:ok, %{context |
      conn: updated_conn,
      current_state: :error,
      user_data: %{}
    }}
  end

  defp execute_clear_error(_data, context) do
    updated_conn = context.conn |> visit("/")

    {:ok, %{context |
      conn: updated_conn,
      current_state: :anonymous
    }}
  end

  defp execute_generic_action(action, _data, context) do
    # For actions we don't specifically handle, just continue
    {:ok, context}
  end

  defp determine_state_after_visit(path, current_state) do
    cond do
      path in ["/sign-in", "/register", "/"] -> :anonymous
      path == "/access-denied" -> :error
      path == "/404" -> :error
      String.starts_with?(path, "/admin") and current_state != :admin -> :error
      String.starts_with?(path, "/admin") -> :admin
      current_state in [:authenticated, :admin] -> current_state
      true -> :anonymous
    end
  end

  defp fill_resource_form(conn, resource_type, data) do
    # This would need to be customized based on your actual form fields
    case resource_type do
      :business ->
        conn
        |> fill_in("Name", with: Map.get(data, :name, "Test Business"))
        |> fill_in("Description", with: Map.get(data, :description, "Test Description"))

      :client ->
        conn
        |> fill_in("Name", with: Map.get(data, :name, "Test Client"))
        |> fill_in("Email", with: Map.get(data, :email, "client@example.com"))

      :item ->
        conn
        |> fill_in("Name", with: Map.get(data, :name, "Test Item"))
        |> fill_in("Price", with: Map.get(data, :price, "100.00"))

      _ ->
        conn
    end
  end

  defp extract_created_resource_id(_conn, _resource_type) do
    # This would extract the ID from the response or URL
    # For now, return a placeholder
    :rand.uniform(1000)
  end

  defp get_current_path(conn) do
    # Extract current path from conn
    conn.request_path || "/"
  end

  defp maybe_take_screenshot(context, name) do
    if context.options.screenshots do
      screenshot_path = "tmp/screenshots/#{name}_#{:os.system_time(:millisecond)}.png"

      try do
        # Take screenshot using Playwright
        context.conn |> screenshot(screenshot_path)
        %{context | screenshots: [screenshot_path | context.screenshots]}
      rescue
        _ -> context  # Continue if screenshot fails
      end
    else
      context
    end
  end

  defp sanitize_path(path) do
    path
    |> String.replace("/", "_")
    |> String.replace(":", "_")
    |> String.replace("?", "_")
  end
end
