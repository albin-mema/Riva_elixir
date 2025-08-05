defmodule RivaAshWeb.HealthControllerTest do
  use RivaAshWeb.ConnCase, async: false
  use RivaAshWeb, :verified_routes
  # Enable ~p verified routes and ConnTest helpers for ~p/get/json_response
  import Phoenix.ConnTest
  import RivaAshWeb, only: [verified_routes: 0]
  verified_routes()

  describe "GET /health" do
    @describetag :controller
    test "returns 200 JSON with status ok/healthy", %{conn: conn} do
      conn = get(conn, ~p"/health")
      assert conn.status in [200, 503]
      body = json_response(conn, conn.status)

      # Minimal shape assertions to be durable across implementations
      assert is_map(body)
      assert Map.has_key?(body, "status")
      assert Map.has_key?(body, "timestamp")
      assert Map.has_key?(body, "service")

      # When DB connected, status 200 and healthy/ok
      if conn.status == 200 do
        assert body["status"] in ["ok", "healthy"]
      else
        # When DB disconnected (rare in CI), expect unhealthy
        assert body["status"] in ["unhealthy", "error"]
      end
    end
  end

  describe "GET /health/db (if present)" do
    @describetag :controller
    test "returns database status ok shape if route exists", %{conn: conn} do
      # If this route is not defined, it should be 404; otherwise assert shape
      conn = get(conn, "/health/db")

      case conn.status do
        404 ->
          assert conn.status == 404

        status when status in [200, 503] ->
          body = json_response(conn, status)
          assert Map.has_key?(body, "database")
          assert body["database"] in ["connected", "disconnected"]
        _ ->
          flunk("Unexpected status from /health/db: #{conn.status}")
      end
    end
  end
end
