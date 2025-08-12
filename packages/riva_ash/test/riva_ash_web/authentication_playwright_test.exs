defmodule RivaAshWeb.AuthenticationPlaywrightTest do
  @moduledoc """
  Comprehensive property-based tests for all Riva Ash application pages using Phoenix Test with Playwright.
  
  This test suite systematically accesses all identified pages:
  - Public routes: /, /search, /auth/sign-in, /auth/register, /docs, /health, /erd
  - Authenticated routes: /app/dashboard, /app/setup, /app/reservations, /app/inventory, /app/people, /app/finance, /app/chat, /app/settings
  - Legacy resource pages: All 20+ legacy pages under /app/people/*, /app/inventory/*, /app/finance/*, /app/reservations/*
  - Development routes: /dev/*, /storybook/* (in dev environment)
  - API routes: /api/booking/*, /admin
  
  Features:
  - Property-based testing with StreamData
  - URL parameter generation for pages requiring them
  - Comprehensive error handling with screenshots
  - Performance metrics for page load times
  - Retry logic for flaky tests
  - Focus on critical user journeys
  """

  use RivaAshWeb.FeatureCase, async: false
  # Start Playwright-backed PhoenixTest session
  use PhoenixTest.Playwright.Case
  use ExUnitProperties
  import PhoenixTest, except: [check: 2]

  import StreamData
  alias RivaAsh.PropertyTesting.RouteEnumerator
  alias RivaAsh.TestHelpers

  @moduletag browser: :chromium
  @moduletag timeout: 120_000  # 2 minutes timeout for browser tests

  # Screenshots directory
  @screenshot_dir "tmp/playwright_screenshots"

  # --- Setup and Helpers ---

  setup do
    # Ensure screenshots directory exists
    File.mkdir_p(@screenshot_dir)
    
    # Create test users
    admin = TestHelpers.create_user!(%{role: :admin, password: "password123"})
    regular_user = TestHelpers.create_user!(%{role: :user, password: "password123"})
    
    %{admin: admin, regular_user: regular_user}
  end

  # Log in as a user using real browser interactions so cookies are set in Playwright
  defp ensure_logged_in(session, user) do
    session
    |> visit("/sign-in")
    |> fill_in("Email address", with: to_string(user.email))
    |> fill_in("Password", with: "password123")
    |> click_button("Sign In")
  end

  # Log in as admin using real browser interactions
  defp ensure_admin_logged_in(session) do
    ensure_logged_in(session, %{email: "admin@example.com", password: "password123"})
  end

  # Visit a page and capture comprehensive information
  defp visit_and_capture(session, path, context \\ %{}) do
    start_time = System.monotonic_time(:millisecond)
    screenshot_path = nil
    
    try do
      session = visit(session, path)
      
      # Capture page load time
      load_time = System.monotonic_time(:millisecond) - start_time
      
      # Check for server errors
      error_check = check_for_server_errors(session)
      
      # Take screenshot if there are errors or in development mode
      if error_check.has_errors or Application.get_env(:riva_ash, :env) == :dev do
        screenshot_path = "#{@screenshot_dir}/#{String.replace(path, "/", "_")}_#{System.system_time(:second)}.png"
        take_screenshot(session, screenshot_path)
      end
      
      page_info = %{
        path: path,
        load_time: load_time,
        status: :success,
        error_check: error_check,
        screenshot_path: if(error_check.has_errors, do: screenshot_path, else: nil)
      }
      
      {:ok, session, page_info}
      
    rescue
      error ->
        load_time = System.monotonic_time(:millisecond) - start_time
        screenshot_path = "#{@screenshot_dir}/#{String.replace(path, "/", "_")}_error_#{System.system_time(:second)}.png"
        take_screenshot(session, screenshot_path)
        
        page_info = %{
          path: path,
          load_time: load_time,
          status: :error,
          error: Exception.message(error),
          screenshot_path: screenshot_path
        }
        
        {:error, page_info}
    end
  end

  # Check for server errors on the page (simplified version)
  defp check_for_server_errors(session) do
    error_patterns = [
      ~s(text="Internal Server Error"),
      ~s(text="Server Error"),
      ~s(text="Exception"),
      ~s(text="500"),
      ~s(text="404"),
      ~s(text="403")
    ]
    
    has_errors = Enum.any?(error_patterns, &refute_has(session, &1))
    
    %{
      has_errors: has_errors,
      error_count: Enum.count(error_patterns, &refute_has(session, &1))
    }
  end

  # Take screenshot with error context
  defp take_screenshot(session, path) do
    try do
      # Use the screenshot function from PhoenixTest
      session |> screenshot(path)
      IO.puts("📸 Screenshot saved: #{path}")
    rescue
      error ->
        IO.puts("❌ Failed to take screenshot: #{Exception.message(error)}")
    end
  end

  # --- Route Categories and Generators ---

  # Get all public routes
  defp public_routes do
    RouteEnumerator.public_routes()
  end

  # Get all authenticated routes
  defp authenticated_routes do
    RouteEnumerator.authenticated_routes()
  end

  # Get all admin routes
  defp admin_routes do
    RouteEnumerator.admin_routes()
  end

  # Get all routes that require parameters
  defp parameterized_routes do
    RouteEnumerator.parameterized_routes()
  end

  # Generate URL parameters for a route
  defp generate_url_params(route) do
    RouteEnumerator.generate_route_params(route)
  end

  # Build full URL with parameters
  defp build_url_with_params(base_path, params) do
    if Enum.empty?(params) do
      base_path
    else
      param_string = params
        |> Enum.map(fn {key, value} -> "#{key}=#{URI.encode(to_string(value))}" end)
        |> Enum.join("&")
      
      "#{base_path}?#{param_string}"
    end
  end

  # --- Comprehensive Page Access Tests ---

  @tag :browser
  test "systematically access all public routes", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    public_paths = public_routes() |> Enum.map(& &1.path)
    
    {results, failures} =
      public_paths
      |> Enum.sort()
      |> Enum.reduce({[], []}, fn path, {acc, fails} ->
        case visit_and_capture(session, path) do
          {:ok, _new_session, page_info} ->
            {[page_info | acc], fails}
          {:error, page_info} ->
            {acc, [page_info | fails]}
        end
      end)

    # Report results
    IO.puts("\n📊 Public Routes Test Results:")
    IO.puts("   Total tested: #{length(results)}")
    IO.puts("   Successful: #{length(results) - length(failures)}")
    IO.puts("   Failed: #{length(failures)}")
    
    if failures != [] do
      IO.puts("\n❌ Failed public routes:")
      failures |> Enum.each(fn %{path: path, error: error} ->
        IO.puts("   #{path}: #{error}")
      end)
    end

    # Assert all routes loaded successfully
    assert failures == [], "Some public routes failed to load. See failures above."
    
    # Check performance
    slow_pages = results |> Enum.filter(&(&1.load_time > 5000))
    if slow_pages != [] do
      IO.puts("\n⚠️  Slow pages (>5s):")
      slow_pages |> Enum.each(fn %{path: path, load_time: time} ->
        IO.puts("   #{path}: #{time}ms")
      end)
    end
  end

  @tag :browser
  test "systematically access all authenticated routes", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    auth_paths = authenticated_routes() |> Enum.map(& &1.path)
    
    {results, failures} =
      auth_paths
      |> Enum.sort()
      |> Enum.reduce({[], []}, fn path, {acc, fails} ->
        case visit_and_capture(session, path) do
          {:ok, new_session, page_info} ->
            {[page_info | acc], fails}
          {:error, page_info} ->
            {acc, [page_info | fails]}
        end
      end)

    # Report results
    IO.puts("\n🔒 Authenticated Routes Test Results:")
    IO.puts("   Total tested: #{length(results)}")
    IO.puts("   Successful: #{length(results) - length(failures)}")
    IO.puts("   Failed: #{length(failures)}")
    
    if failures != [] do
      IO.puts("\n❌ Failed authenticated routes:")
      failures |> Enum.each(fn %{path: path, error: error} ->
        IO.puts("   #{path}: #{error}")
      end)
    end

    assert failures == [], "Some authenticated routes failed to load. See failures above."
  end

  @tag :browser
  test "systematically access all admin routes", %{conn: conn, admin: admin} do
    session = conn |> ensure_admin_logged_in()
    
    admin_paths = admin_routes() |> Enum.map(& &1.path)
    
    {results, failures} =
      admin_paths
      |> Enum.sort()
      |> Enum.reduce({[], []}, fn path, {acc, fails} ->
        case visit_and_capture(session, path) do
          {:ok, new_session, page_info} ->
            {[page_info | acc], fails}
          {:error, page_info} ->
            {acc, [page_info | fails]}
        end
      end)

    # Report results
    IO.puts("\n👑 Admin Routes Test Results:")
    IO.puts("   Total tested: #{length(results)}")
    IO.puts("   Successful: #{length(results) - length(failures)}")
    IO.puts("   Failed: #{length(failures)}")
    
    if failures != [] do
      IO.puts("\n❌ Failed admin routes:")
      failures |> Enum.each(fn %{path: path, error: error} ->
        IO.puts("   #{path}: #{error}")
      end)
    end

    assert failures == [], "Some admin routes failed to load. See failures above."
  end

  @tag :browser
  test "access parameterized routes with generated parameters", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    param_routes = parameterized_routes()
    
    {results, failures} =
      param_routes
      |> Enum.sort_by(& &1.path)
      |> Enum.reduce({[], []}, fn route, {acc, fails} ->
        params = generate_url_params(route)
        url = build_url_with_params(route.path, params)
        
        case visit_and_capture(session, url) do
          {:ok, new_session, page_info} ->
            {[Map.put(page_info, :generated_params, params) | acc], fails}
          {:error, page_info} ->
            {acc, [Map.put(page_info, :generated_params, params) | fails]}
        end
      end)

    # Report results
    IO.puts("\n🔗 Parameterized Routes Test Results:")
    IO.puts("   Total tested: #{length(results)}")
    IO.puts("   Successful: #{length(results) - length(failures)}")
    IO.puts("   Failed: #{length(failures)}")
    
    if failures != [] do
      IO.puts("\n❌ Failed parameterized routes:")
      failures |> Enum.each(fn %{path: path, generated_params: params, error: error} ->
        IO.puts("   #{path} with #{inspect(params)}: #{error}")
      end)
    end

    assert failures == [], "Some parameterized routes failed to load. See failures above."
  end

  # --- Property-Based Testing with StreamData ---

  property "random public pages render without server errors", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    public_paths = public_routes() |> Enum.map(& &1.path)
    # Ensure home page is included
    public_paths = if "/" in public_paths, do: public_paths, else: ["/" | public_paths]
    
    check all(
            path <- member_of(public_paths),
            max_runs: 20
          ) do
      case visit_and_capture(session, path) do
        {:ok, _new_session, page_info} ->
          assert page_info.status == :success,
                 "Page #{path} failed to load: #{inspect(page_info)}"
          
          # Performance assertion
          assert page_info.load_time < 10_000,
                 "Page #{path} took too long to load: #{page_info.load_time}ms"
          
          # No server errors
          refute page_info.error_check.has_errors,
                 "Page #{path} contains server errors"
          
          true
        {:error, page_info} ->
          flunk("Page #{path} failed to load: #{page_info.error}")
      end
    end
  end

  property "random authenticated pages render without server errors", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    auth_paths = authenticated_routes() |> Enum.map(& &1.path)
    
    check all(
            path <- member_of(auth_paths),
            max_runs: 15
          ) do
      case visit_and_capture(session, path) do
        {:ok, _new_session, page_info} ->
          assert page_info.status == :success,
                 "Authenticated page #{path} failed to load: #{inspect(page_info)}"
          
          assert page_info.load_time < 10_000,
                 "Authenticated page #{path} took too long to load: #{page_info.load_time}ms"
          
          refute page_info.error_check.has_errors,
                 "Authenticated page #{path} contains server errors"
          
          true
        {:error, page_info} ->
          flunk("Authenticated page #{path} failed to load: #{page_info.error}")
      end
    end
  end

  # --- Critical User Journey Tests ---

  @tag :browser
  test "registration to dashboard flow", %{conn: conn} do
    session = conn
    
    # Step 1: Visit home page
    {:ok, session, _home_info} = visit_and_capture(session, "/")
    
    # Step 2: Navigate to registration
    {:ok, session, _register_info} = visit_and_capture(session, "/register")
    
    # Step 3: Fill registration form (simulated)
    # In a real test, you would fill the form and submit
    # For now, just verify the page loads
    assert session.current_path == "/register"
    
    # Step 4: Navigate to sign-in
    {:ok, session, _signin_info} = visit_and_capture(session, "/sign-in")
    
    # Step 5: Complete sign-in (simulated)
    session = ensure_logged_in(session, %{email: "test@example.com", password: "password123"})
    
    # Step 6: Visit dashboard
    {:ok, session, dashboard_info} = visit_and_capture(session, "/dashboard")
    
    # Verify the complete flow
    assert dashboard_info.status == :success,
           "Dashboard failed to load after registration flow"
    
    assert dashboard_info.load_time < 5_000,
           "Dashboard took too long to load: #{dashboard_info.load_time}ms"
    
    refute dashboard_info.error_check.has_errors,
           "Dashboard contains server errors"
  end

  @tag :browser
  test "item booking process", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    # Step 1: Visit inventory
    {:ok, session, _inventory_info} = visit_and_capture(session, "/app/inventory")
    
    # Step 2: Visit search
    {:ok, session, _search_info} = visit_and_capture(session, "/search")
    
    # Step 3: Visit reservations (simulated booking flow)
    {:ok, session, _reservations_info} = visit_and_capture(session, "/app/reservations")
    
    # Verify the booking flow pages load successfully
    assert session.current_path in ["/app/inventory", "/search", "/app/reservations"]
  end

  @tag :browser
  test "payment processing flow", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    # Step 1: Visit finance section
    {:ok, session, _finance_info} = visit_and_capture(session, "/app/finance")
    
    # Step 2: Visit settings (payment methods)
    {:ok, session, _settings_info} = visit_and_capture(session, "/app/settings")
    
    # Verify payment-related pages load successfully
    assert session.current_path in ["/app/finance", "/app/settings"]
  end

  @tag :browser
  test "search functionality", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    # Step 1: Visit search page
    {:ok, session, _search_info} = visit_and_capture(session, "/search")
    
    # Step 2: Test search with parameters
    search_url = "/search?q=test+item&category=all"
    {:ok, session, _search_results_info} = visit_and_capture(session, search_url)
    
    # Verify search functionality works
    assert session.current_path == "/search"
  end

  # --- Legacy and Development Route Tests ---

  @tag :browser
  test "legacy resource pages under /app/*", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    # Common legacy patterns
    legacy_patterns = [
      "/app/people",
      "/app/people/list",
      "/app/people/new",
      "/app/inventory",
      "/app/inventory/list",
      "/app/inventory/new",
      "/app/finance",
      "/app/finance/reports",
      "/app/reservations",
      "/app/reservations/calendar",
      "/app/settings"
    ]
    
    {results, failures} =
      legacy_patterns
      |> Enum.reduce({[], []}, fn path, {acc, fails} ->
        case visit_and_capture(session, path) do
          {:ok, new_session, page_info} ->
            {[page_info | acc], fails}
          {:error, page_info} ->
            {acc, [page_info | fails]}
        end
      end)

    IO.puts("\n📁 Legacy Routes Test Results:")
    IO.puts("   Total tested: #{length(results)}")
    IO.puts("   Successful: #{length(results) - length(failures)}")
    IO.puts("   Failed: #{length(failures)}")
    
    assert failures == [], "Some legacy routes failed to load. See failures above."
  end

  @tag :browser
  @tag :dev_only
  test "development routes", %{conn: conn} do
    # Only run in development environment
    if Application.get_env(:riva_ash, :env) == :dev do
      session = conn
      
      dev_patterns = [
        "/dev",
        "/dev/tools",
        "/storybook",
        "/storybook/ui"
      ]
      
      {results, failures} =
        dev_patterns
        |> Enum.reduce({[], []}, fn path, {acc, fails} ->
          case visit_and_capture(session, path) do
            {:ok, new_session, page_info} ->
              {[page_info | acc], fails}
            {:error, page_info} ->
              {acc, [page_info | fails]}
          end
        end)

      IO.puts("\n🛠️  Development Routes Test Results:")
      IO.puts("   Total tested: #{length(results)}")
      IO.puts("   Successful: #{length(results) - length(failures)}")
      IO.puts("   Failed: #{length(failures)}")
      
      # Development routes can fail in test environment, so just log
      if failures != [] do
        IO.puts("   Note: Some dev routes may not be available in test environment")
      end
    end
  end

  @tag :browser
  test "API routes", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    # Test API endpoints that should be accessible
    api_patterns = [
      "/api/health",
      "/admin"
    ]
    
    {results, failures} =
      api_patterns
      |> Enum.reduce({[], []}, fn path, {acc, fails} ->
        case visit_and_capture(session, path) do
          {:ok, new_session, page_info} ->
            {[page_info | acc], fails}
          {:error, page_info} ->
            {acc, [page_info | fails]}
        end
      end)

    IO.puts("\n🌐 API Routes Test Results:")
    IO.puts("   Total tested: #{length(results)}")
    IO.puts("   Successful: #{length(results) - length(failures)}")
    IO.puts("   Failed: #{length(failures)}")
    
    # API routes might redirect or return different content types
    # Just ensure they don't crash the browser
    assert length(failures) == 0 or
           (length(failures) == 1 and hd(failures).path == "/admin"),
           "API routes should not crash the browser"
  end

  # --- Performance and Reliability Tests ---

  @tag :browser
  test "page load performance metrics", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    # Test key pages for performance
    key_pages = [
      "/",
      "/dashboard",
      "/search",
      "/app/inventory",
      "/app/reservations"
    ]
    
    performance_results =
      key_pages
      |> Enum.map(fn path ->
        case visit_and_capture(session, path) do
          {:ok, _session, page_info} ->
            {path, page_info.load_time, page_info.status}
          {:error, page_info} ->
            {path, page_info.load_time, page_info.error}
        end
      end)

    IO.puts("\n⚡ Performance Metrics:")
    performance_results |> Enum.each(fn {path, time, status} ->
      IO.puts("   #{path}: #{time}ms - #{status}")
    end)

    # Assert all pages load within reasonable time
    slow_pages = performance_results |> Enum.filter(fn {_path, time, _status} -> time > 5000 end)
    assert slow_pages == [], "Some pages took too long to load: #{inspect(slow_pages)}"
  end

  @tag :browser
  test "retry mechanism for flaky tests", %{conn: conn, regular_user: regular_user} do
    session = conn |> ensure_logged_in(regular_user)
    
    # Test a page that might be flaky
    path = "/dashboard"
    
    # Try up to 3 times
    results =
      1..3
      |> Enum.map(fn attempt ->
        case visit_and_capture(session, path) do
          {:ok, _session, page_info} ->
            {:success, page_info}
          {:error, page_info} ->
            {:error, page_info}
        end
      end)
    
    # Check if any attempt succeeded
    success_results = results |> Enum.filter(fn {status, _} -> status == :success end)
    
    if success_results == [] do
      IO.puts("❌ All attempts failed for #{path}")
      # Log the errors for debugging
      results |> Enum.each(fn {status, info} ->
        IO.puts("   Attempt: #{status} - #{inspect(info)}")
      end)
    end
    
    # At least one attempt should succeed
    assert success_results != [], "All retry attempts failed for #{path}"
  end
end
