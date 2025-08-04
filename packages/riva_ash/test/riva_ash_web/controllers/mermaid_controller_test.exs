defmodule RivaAshWeb.MermaidControllerTest do
  use RivaAshWeb.ConnCase, async: true

  # Enable ~p sigil via verified routes; ConnTest gives get/2, response/2
  import Phoenix.ConnTest
  import RivaAshWeb, only: [verified_routes: 0]
  verified_routes()

  test "GET /mermaid/domain returns mermaid syntax", %{conn: conn} do
    conn = get(conn, ~p"/mermaid/domain")
    assert get_resp_header(conn, "content-type") |> Enum.any?(&String.contains?(&1, "text/html"))
    body = response(conn, 200)
    assert body =~ "graph" or body =~ "erDiagram"
  end

  test "invalid params return 400", %{conn: conn} do
    conn = get(conn, ~p"/mermaid/domain?type=unknown")
    assert conn.status == 400
  end
end