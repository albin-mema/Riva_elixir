defmodule RivaAshWeb.SwaggerControllerTest do
  use RivaAshWeb.ConnCase, async: true

  # Enable ~p sigil via verified routes; ConnTest/Plug.Conn for helpers
  import Phoenix.ConnTest
  import Plug.Conn
  import RivaAshWeb, only: [verified_routes: 0]
  verified_routes()

  test "GET /swagger.json includes info, paths, components", %{conn: conn} do
    conn = get(conn, ~p"/swagger.json")
    assert conn.status == 200
    assert get_resp_header(conn, "content-type") |> Enum.any?(&String.contains?(&1, "application/json"))
    {:ok, json} = Jason.decode(response(conn, 200))
    assert Map.has_key?(json, "openapi")
    assert Map.has_key?(json, "info")
    assert Map.has_key?(json, "paths")
    assert Map.has_key?(json, "components")
  end

  test "returns 404 for missing spec chunk", %{conn: conn} do
    conn = get(conn, ~p"/swagger/unknown.json")
    assert conn.status in [404, 400]
  end
end