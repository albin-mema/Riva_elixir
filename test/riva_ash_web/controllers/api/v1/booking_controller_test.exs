defmodule RivaAshWeb.Api.V1.BookingControllerTest do
  use RivaAshWeb.ConnCase, async: true
  import RivaAsh.Factory

  describe "POST /api/booking/create" do
    setup do
      business = insert(:business)
      item = insert(:item, business: business)
      %{item: item}
    end

    test "creates booking with valid params", %{conn: conn, item: item} do
      params = %{
        "client" => %{"name" => "Test User", "email" => "test@example.com"},
        "booking" => %{
          "item_id" => item.id,
          "reserved_from" => "2024-01-01T10:00:00Z",
          "reserved_until" => "2024-01-01T11:00:00Z"
        }
      }

      conn = post(conn, "/api/booking/create", params)
      response = json_response(conn, 201)

      assert response["data"]["booking_id"]
      assert response["data"]["client"]["name"] == "Test User"
      assert response["data"]["reservation"]["item_id"] == item.id
    end

    test "returns 400 for invalid datetime", %{conn: conn, item: item} do
      params = %{
        "client" => %{"name" => "Test User"},
        "booking" => %{
          "item_id" => item.id,
          "reserved_from" => "invalid",
          "reserved_until" => "2024-01-01T11:00:00Z"
        }
      }

      conn = post(conn, "/api/booking/create", params)
      response = json_response(conn, 400)

      assert response["error"] == "Invalid datetime format. Use ISO8601 format"
    end
  end

  describe "GET /api/booking/availability/:item_id" do
    setup do
      business = insert(:business)
      item = insert(:item, business: business)
      %{item: item}
    end

    test "returns available time slots", %{conn: conn, item: item} do
      conn = get(conn, "/api/booking/availability/#{item.id}", %{
        "date" => "2024-01-01",
        "duration" => "60"
      })

      response = json_response(conn, 200)

      assert response["data"]["item_id"] == item.id
      assert response["data"]["date"] == "2024-01-01"
      assert length(response["data"]["time_slots"]) > 0
    end

    test "returns 400 for invalid date", %{conn: conn, item: item} do
      conn = get(conn, "/api/booking/availability/#{item.id}", %{"date" => "invalid"})
      response = json_response(conn, 400)

      assert response["error"] == "Invalid date format. Use YYYY-MM-DD"
    end
  end

  describe "GET /api/booking/client/:email" do
    setup do
      client = insert(:client, email: "test@example.com")
      reservation = insert(:reservation, client: client)
      %{client: client, reservation: reservation}
    end

    test "returns client bookings", %{conn: conn, client: client, reservation: reservation} do
      conn = get(conn, "/api/booking/client/#{client.email}")
      response = json_response(conn, 200)

      assert response["data"]["client"]["id"] == client.id
      assert length(response["data"]["bookings"]) == 1
      assert hd(response["data"]["bookings"])["id"] == reservation.id
    end

    test "returns 404 for non-existent email", %{conn: conn} do
      conn = get(conn, "/api/booking/client/nonexistent@example.com")
      response = json_response(conn, 404)

      assert response["error"] == "No bookings found for this email address"
    end
  end

  describe "POST /api/booking/confirm/:id" do
    setup do
      client = insert(:client)
      reservation = insert(:reservation, client: client, status: "pending")
      %{reservation: reservation}
    end

    test "confirms booking", %{conn: conn, reservation: reservation} do
      params = %{"register_client" => true}
      conn = post(conn, "/api/booking/confirm/#{reservation.id}", params)
      response = json_response(conn, 200)

      assert response["data"]["status"] == "confirmed"
      assert response["data"]["client"]["is_registered"] == true
    end

    test "returns 404 for non-existent booking", %{conn: conn} do
      conn = post(conn, "/api/booking/confirm/invalid-id", %{})
      response = json_response(conn, 404)

      assert response["error"] =~ "not found"
    end
  end
end
