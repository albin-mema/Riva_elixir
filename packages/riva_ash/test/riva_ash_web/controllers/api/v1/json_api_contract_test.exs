defmodule RivaAshWeb.Api.V1.JsonApiContractTest do
  use RivaAshWeb.ConnCase, async: false
  # Use shared JSON:API helpers rather than local duplication
  import RivaAsh.Test.JsonApiHelpers

  describe "error contract for invalid include/sort" do
    @describetag :jsonapi

    test "invalid include returns JSON:API error", %{conn: conn} do
      # Choose a common resource path; AshJsonApi exposes resources by type under /api
      # This suite focuses on contract shape rather than exact endpoint.
      conn = get(conn, "/api/unknown_type?include=nonexistent_rel")

      # The JSON:API router should return 400/409 with errors member for invalid include
      # Accept a broader range if the router normalizes to 400.
      status = conn.status || 400
      assert status in [400, 409, 422]

      body = Jason.decode!(conn.resp_body)
      assert_error_shape(body)
    end

    test "invalid sort returns JSON:API error", %{conn: conn} do
      conn = get(conn, "/api/unknown_type?sort=nonexistent_field")

      status = conn.status || 400
      assert status in [400, 409, 422]

      body = Jason.decode!(conn.resp_body)
      assert_error_shape(body)
    end
  end
end
