defmodule RivaAshWeb.Api.V1.JsonApiTest do
  use RivaAshWeb.ConnCase, async: true
  import Ash.Query

  describe "JSON:API compliance" do
    test "GET /api responds with JSON:API error format on invalid request", %{conn: conn} do
      conn = get(conn, "/api/invalid")
      response = json_response(conn, 404)

      assert response["errors"] != nil
      assert response["errors"] |> hd() |> Map.has_key?("status")
      assert response["errors"] |> hd() |> Map.has_key?("title")
    end

    test "Pagination follows JSON:API spec", %{conn: conn} do
      # Create test data
      1..25 |> Enum.each(fn _ -> insert(:reservation) end)

      conn = get(conn, "/api/reservations?page[size]=10")
      response = json_response(conn, 200)

      assert response["meta"]["total-count"] >= 25
      assert length(response["data"]) == 10
      assert response["links"]["next"] != nil
    end

    test "Filtering uses Ash DB-level filters", %{conn: conn} do
      insert(:reservation, client_email: "test@example.com")
      insert(:reservation, client_email: "other@example.com")

      conn = get(conn, "/api/reservations?filter[client_email]=test@example.com")
      response = json_response(conn, 200)

      assert length(response["data"]) == 1
      assert response["data"] |> hd() |> get_in(["attributes", "client_email"]) == "test@example.com"
    end

    test "Sorting rejects unknown fields", %{conn: conn} do
      conn = get(conn, "/api/reservations?sort=invalid_field")
      response = json_response(conn, 400)

      assert response["errors"] |> hd() |> get_in(["detail"]) =~ "Invalid sort field"
    end

    test "Error format follows JSON:API spec for 422", %{conn: conn} do
      conn = post(conn, "/api/reservations", %{})
      response = json_response(conn, 422)

      assert response["errors"] != nil
      assert response["errors"] |> hd() |> Map.has_key?("status")
      assert response["errors"] |> hd() |> Map.has_key?("title")
      assert response["errors"] |> hd() |> Map.has_key?("detail")
    end
  end

  describe "OpenAPI documentation" do
    test "GET /docs responds with HTML containing correct OpenAPI URL", %{conn: conn} do
      conn = get(conn, "/docs")
      response = html_response(conn, 200)

      assert response =~ ~s|url: "/api/open_api"|
      assert response =~ ~s|<title>API Documentation</title>|
    end

    test "GET /api/open_api returns valid OpenAPI JSON", %{conn: conn} do
      conn = get(conn, "/api/open_api")
      response = json_response(conn, 200)

      assert response["openapi"] =~ ~r/^3\./
      assert response["info"]["title"] == "Riva Ash API"
      assert response["servers"] |> hd() |> Map.get("url") == "/api"
    end

    test "GET /api/open_api has correct content type", %{conn: conn} do
      conn = get(conn, "/api/open_api")
      assert get_resp_header(conn, "content-type") == ["application/json; charset=utf-8"]
    end
  end

  describe "API root endpoint" do
    test "GET /api returns 404 with JSON:API error format", %{conn: conn} do
      conn = get(conn, "/api")
      response = json_response(conn, 404)

      assert response["errors"] != nil
      assert response["errors"] |> hd() |> get_in(["status"]) == "404"
      assert response["errors"] |> hd() |> get_in(["title"]) == "Not Found"
      assert response["errors"] |> hd() |> get_in(["detail"]) =~ "No route found for GET /api"
    end
  end

  describe "error handling" do
    test "returns 400 for invalid filter parameters", %{conn: conn} do
      conn = get(conn, "/api/reservations?filter[invalid_field]=test")
      response = json_response(conn, 400)

      assert response["errors"] |> hd() |> get_in(["status"]) == "400"
      assert response["errors"] |> hd() |> get_in(["title"]) == "Invalid Filter"
      assert response["errors"] |> hd() |> get_in(["detail"]) =~ "Unknown filter field"
    end

    test "returns 400 for invalid sort parameters", %{conn: conn} do
      conn = get(conn, "/api/reservations?sort=invalid_field")
      response = json_response(conn, 400)

      assert response["errors"] |> hd() |> get_in(["status"]) == "400"
      assert response["errors"] |> hd() |> get_in(["title"]) == "Invalid Sort"
      assert response["errors"] |> hd() |> get_in(["detail"]) =~ "Unknown sort field"
    end

    test "returns 404 for non-existent resource", %{conn: conn} do
      conn = get(conn, "/api/non_existent_resource")
      response = json_response(conn, 404)

      assert response["errors"] |> hd() |> get_in(["status"]) == "404"
      assert response["errors"] |> hd() |> get_in(["title"]) == "Not Found"
      assert response["errors"] |> hd() |> get_in(["detail"]) =~ "No route found"
    end
  end

  describe "pagination" do
    test "enforces default page size", %{conn: conn} do
      conn = get(conn, "/api/reservations")
      response = json_response(conn, 200)

      assert length(response["data"]) <= Application.get_env(:riva_ash, :default_page_size, 25)
    end

    test "enforces maximum page size", %{conn: conn} do
      conn = get(conn, "/api/reservations?page[size]=1000")
      response = json_response(conn, 400)

      assert response["errors"] |> hd() |> get_in(["status"]) == "400"
      assert response["errors"] |> hd() |> get_in(["title"]) == "Invalid Page Size"
      assert response["errors"] |> hd() |> get_in(["detail"]) =~ "exceeds maximum"
    end
  end

  describe "authentication" do
    test "public endpoints accessible without auth", %{conn: conn} do
      conn = get(conn, "/api/open_api")
      assert response(conn, 200)
    end

    test "write operations require authentication", %{conn: conn} do
      conn = post(conn, "/api/booking/create", %{})
      assert response(conn, 401)
    end

    test "authenticated requests succeed with valid token", %{conn: conn} do
      # Mock a valid authentication token
      conn = conn |> put_req_header("authorization", "Bearer valid_token")
      conn = post(conn, "/api/booking/create", %{item_id: 1, start_time: "2025-08-10T10:00:00Z"})
      assert response(conn, 201)
    end
  end

  describe "rate limiting" do
    test "exceeding rate limit returns 429", %{conn: conn} do
      # Simulate multiple requests to trigger rate limiting
      Enum.each(1..11, fn _ ->
        conn = get(conn, "/api/open_api")
        assert response(conn, 200)
      end)

      conn = get(conn, "/api/open_api")
      assert response(conn, 429)
    end
  end
end
