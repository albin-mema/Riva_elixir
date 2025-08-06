defmodule RivaAshWeb.MermaidControllerTest do
  use RivaAshWeb.ConnCase, async: true
  use RivaAshWeb, :verified_routes

  # Enable ~p sigil via verified routes; ConnTest gives get/2, response/2
  import Phoenix.ConnTest

  @spec test_get_mermaid_domain_returns_mermaid_syntax :: :ok
  test "GET /mermaid/domain returns mermaid syntax", %{conn: conn} do
    conn = get(conn, ~p"/mermaid/domain")
    assert get_resp_header(conn, "content-type") |> Enum.any?(&String.contains?(&1, "text/html"))
    body = response(conn, 200)
    assert body =~ "graph" or body =~ "erDiagram"
  end

  @spec test_invalid_params_return_400 :: :ok
  test "invalid params return 400", %{conn: conn} do
    conn = get(conn, ~p"/mermaid/domain?type=unknown")
    assert conn.status == 400
  end
end
