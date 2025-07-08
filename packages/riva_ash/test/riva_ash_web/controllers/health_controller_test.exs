defmodule RivaAshWeb.HealthControllerTest do
  use RivaAshWeb.FeatureCase, async: true

  describe "GET /health" do
    test "returns 200 and healthy status when database is available", %{conn: conn} do
      response =
        conn
        |> visit("/health")
        |> unwrap(fn conn ->
          json_response(conn, 200)
        end)

      assert response["status"] == "healthy"
      assert response["database"] == "connected"
      assert response["service"] == "riva_ash_api"
      assert is_binary(response["timestamp"])
    end

    # Note: Testing the database failure case would require more complex setup
    # with a test-specific configuration. For now, we'll focus on the happy path.
  end
end
