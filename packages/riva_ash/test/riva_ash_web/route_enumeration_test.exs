defmodule RivaAshWeb.RouteEnumerationTest do
  @moduledoc """
  Comprehensive route enumeration and testing to catch crashes and errors.
  This test systematically checks all routes in the application.
  """

  use RivaAshWeb.ConnCase
  import RivaAsh.TestHelpers
  import Phoenix.ConnTest

  @endpoint RivaAshWeb.Endpoint

  describe "Route enumeration and error checking" do
    setup do
      # Create test data for authenticated routes
      user = create_user()
      business = create_business(user.id)

      # Create additional test data that routes might need
      client = create_client(business.id)
      item_type = create_item_type(business.id)
      item = create_item(business.id, item_type.id)

      %{
        user: user,
        business: business,
        client: client,
        item_type: item_type,
        item: item
      }
    end

    test "enumerate and test all routes", %{
      user: user,
      business: business,
      client: client,
      item: item
    } do
      # Get all routes from the router
      routes = get_all_routes()

      IO.puts("\n=== ROUTE ENUMERATION TEST ===")
      IO.puts("Found #{length(routes)} routes to test")

      # Group routes by type for better testing strategy
      {public_routes, auth_routes, api_routes, admin_routes} = categorize_routes(routes)

      # Test public routes (no authentication needed)
      test_public_routes(public_routes)

      # Test authenticated routes
      test_authenticated_routes(auth_routes, user)

      # Test API routes
      test_api_routes(api_routes)

      # Test admin routes
      test_admin_routes(admin_routes, user)

      IO.puts("=== ROUTE ENUMERATION COMPLETE ===\n")
    end

    test "test specific route patterns with parameters", %{
      user: user,
      business: business,
      client: client,
      item: item
    } do
      # Test routes that require specific parameters
      test_routes_with_params([
        {"/businesses/:id/edit", [id: business.id], user},
        {"/clients/:id/edit", [id: client.id], user},
        {"/items/:id/edit", [id: item.id], user},
        {"/api/booking/availability/:item_id", [item_id: item.id], nil},
        {"/api/booking/client/:email", [email: "test@example.com"], nil}
      ])
    end

    test "test error routes and edge cases" do
      # Test known error routes
      test_error_routes([
        "/404",
        "/access-denied",
        "/nonexistent-route",
        "/api/nonexistent",
        "/admin/nonexistent"
      ])
    end
  end

  # Helper functions

  defp get_all_routes do
    # Get all routes from Phoenix router
    RivaAshWeb.Router.__routes__()
  end

  defp categorize_routes(routes) do
    public_routes = Enum.filter(routes, &public_route?/1)
    auth_routes = Enum.filter(routes, &authenticated_route?/1)
    api_routes = Enum.filter(routes, &api_route?/1)
    admin_routes = Enum.filter(routes, &admin_route?/1)

    {public_routes, auth_routes, api_routes, admin_routes}
  end

  defp public_route?(%{path: path, pipe_through: pipes}) do
    String.starts_with?(path, "/") and
      not String.starts_with?(path, "/api") and
      not String.starts_with?(path, "/admin") and
      not Enum.member?(pipes, :require_authenticated_user)
  end

  defp authenticated_route?(%{pipe_through: pipes}) do
    Enum.member?(pipes, :require_authenticated_user)
  end

  defp api_route?(%{path: path}) do
    String.starts_with?(path, "/api") or String.starts_with?(path, "/graphql")
  end

  defp admin_route?(%{path: path}) do
    String.starts_with?(path, "/admin")
  end

  defp test_public_routes(routes) do
    IO.puts("\n--- Testing #{length(routes)} public routes ---")

    Enum.each(routes, fn route ->
      test_single_route(route, nil)
    end)
  end

  defp test_authenticated_routes(routes, user) do
    IO.puts("\n--- Testing #{length(routes)} authenticated routes ---")

    Enum.each(routes, fn route ->
      test_single_route(route, user)
    end)
  end

  defp test_api_routes(routes) do
    IO.puts("\n--- Testing #{length(routes)} API routes ---")

    Enum.each(routes, fn route ->
      test_single_route(route, nil, :api)
    end)
  end

  defp test_admin_routes(routes, user) do
    IO.puts("\n--- Testing #{length(routes)} admin routes ---")

    Enum.each(routes, fn route ->
      test_single_route(route, user, :admin)
    end)
  end

  defp test_single_route(route, user, type \\ :web) do
    path = route.path
    verb = route.verb

    # Skip routes with parameters for now (they need special handling)
    if String.contains?(path, ":") do
      IO.puts("  SKIP #{verb} #{path} (has parameters)")
      :ok
    end

    try do
      conn = build_conn()

      # Authenticate if user provided
      conn = if user, do: log_in_user(conn, user), else: conn

      # Set appropriate headers based on route type
      conn =
        case type do
          :api -> put_req_header(conn, "accept", "application/json")
          _ -> put_req_header(conn, "accept", "text/html")
        end

      # Make the request
      response =
        case verb do
          :get -> get(conn, path)
          :post -> post(conn, path, %{})
          :put -> put(conn, path, %{})
          :patch -> patch(conn, path, %{})
          :delete -> delete(conn, path)
          # Default to GET
          _ -> get(conn, path)
        end

      status = response.status

      case status do
        200 ->
          IO.puts("  ✓ #{verb} #{path} -> #{status}")

        201 ->
          IO.puts("  ✓ #{verb} #{path} -> #{status}")

        302 ->
          IO.puts("  ↗ #{verb} #{path} -> #{status} (redirect)")

        401 ->
          IO.puts("  🔒 #{verb} #{path} -> #{status} (unauthorized)")

        403 ->
          IO.puts("  🚫 #{verb} #{path} -> #{status} (forbidden)")

        404 ->
          IO.puts("  ❓ #{verb} #{path} -> #{status} (not found)")

        422 ->
          IO.puts("  ⚠ #{verb} #{path} -> #{status} (unprocessable)")

        500 ->
          IO.puts("  ❌ #{verb} #{path} -> #{status} (SERVER ERROR)")

        _ ->
          IO.puts("  ? #{verb} #{path} -> #{status}")
      end

      # Assert no 500 errors (server crashes)
      assert status != 500,
             "Route #{verb} #{path} returned 500 error: #{inspect(response.resp_body)}"
    rescue
      error ->
        IO.puts("  💥 #{verb} #{path} -> EXCEPTION: #{inspect(error)}")
        reraise error, __STACKTRACE__
    end
  end

  defp test_routes_with_params(route_params_list) do
    IO.puts("\n--- Testing routes with parameters ---")

    Enum.each(route_params_list, fn {path_template, params, user} ->
      # Replace parameters in path
      actual_path =
        Enum.reduce(params, path_template, fn {key, value}, acc ->
          String.replace(acc, ":#{key}", to_string(value))
        end)

      try do
        conn = build_conn()
        conn = if user, do: log_in_user(conn, user), else: conn

        # Determine if it's an API route
        conn =
          if String.starts_with?(actual_path, "/api") do
            put_req_header(conn, "accept", "application/json")
          else
            put_req_header(conn, "accept", "text/html")
          end

        response = get(conn, actual_path)
        status = response.status

        case status do
          200 ->
            IO.puts("  ✓ GET #{actual_path} -> #{status}")

          302 ->
            IO.puts("  ↗ GET #{actual_path} -> #{status} (redirect)")

          401 ->
            IO.puts("  🔒 GET #{actual_path} -> #{status} (unauthorized)")

          403 ->
            IO.puts("  🚫 GET #{actual_path} -> #{status} (forbidden)")

          404 ->
            IO.puts("  ❓ GET #{actual_path} -> #{status} (not found)")

          500 ->
            IO.puts("  ❌ GET #{actual_path} -> #{status} (SERVER ERROR)")

          _ ->
            IO.puts("  ? GET #{actual_path} -> #{status}")
        end

        assert status != 500,
               "Route GET #{actual_path} returned 500 error: #{inspect(response.resp_body)}"
      rescue
        error ->
          IO.puts("  💥 GET #{actual_path} -> EXCEPTION: #{inspect(error)}")
          reraise error, __STACKTRACE__
      end
    end)
  end

  defp test_error_routes(paths) do
    IO.puts("\n--- Testing error routes ---")

    Enum.each(paths, fn path ->
      try do
        conn = build_conn()
        response = get(conn, path)
        status = response.status

        case status do
          404 ->
            IO.puts("  ✓ GET #{path} -> #{status} (expected)")

          200 ->
            IO.puts("  ? GET #{path} -> #{status} (unexpected success)")

          302 ->
            IO.puts("  ↗ GET #{path} -> #{status} (redirect)")

          500 ->
            IO.puts("  ❌ GET #{path} -> #{status} (SERVER ERROR)")

          _ ->
            IO.puts("  ? GET #{path} -> #{status}")
        end

        # For error routes, we mainly care about not getting 500s
        assert status != 500,
               "Error route GET #{path} returned 500 error: #{inspect(response.resp_body)}"
      rescue
        error ->
          IO.puts("  💥 GET #{path} -> EXCEPTION: #{inspect(error)}")
          reraise error, __STACKTRACE__
      end
    end)
  end

  # Helper to authenticate a user in tests
  defp log_in_user(conn, user) do
    conn
    |> init_test_session(%{})
    |> put_session(:user_id, user.id)
  end
end
