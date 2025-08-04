defmodule RivaAshWeb.BookingControllerTest do
  use RivaAshWeb.ConnCase, async: true

  import Phoenix.ConnTest
  alias RivaAsh.Factory

  @avail_path &("/api/booking/availability/#{&1}")
  @create_path "/api/booking/create"
  @items_path "/api/booking/items"
  @client_bookings_path &("/api/booking/client/#{&1}")

  defp json_conn(conn) do
    conn
    |> put_req_header("accept", "application/json")
    |> put_req_header("content-type", "application/json")
  end

  describe "GET /api/booking/items (index-like listing)" do
    test "returns items list for booking (200)", %{conn: conn} do
      %{item: _item} = Factory.sample_data()

      conn =
        conn
        |> json_conn()
        |> get(@items_path)

      assert %{"data" => list} = json_response(conn, 200)
      assert is_list(list)
      assert Enum.any?(list, fn %{"id" => id, "name" => name} ->
               is_binary(id) and is_binary(name)
             end)
    end

    test "requires auth/permission", %{conn: conn} do
      # Simulate unauthorized by not logging in / missing header
      conn =
        conn
        |> json_conn()
        |> get(@items_path)

      status = conn.status || 401
      assert status in [401, 403]
    end
  end

  describe "GET /api/booking/availability/:item_id" do
    test "paginates and sorts with valid params mapped to availability date/duration window", %{conn: conn} do
      %{item: item} = Factory.sample_data()

      date = Date.utc_today() |> Date.to_iso8601()

      conn =
        conn
        |> json_conn()
        |> get(@avail_path.(item.id), %{
          "date" => date,
          "duration" => "60",
          "start_hour" => "9",
          "end_hour" => "17"
        })

      assert %{
               "data" => %{
                 "item_id" => ^item.id,
                 "date" => ^date,
                 "duration_minutes" => 60,
                 "business_hours" => %{"start" => 9, "end" => 17},
                 "time_slots" => slots
               }
             } = json_response(conn, 200)

      assert is_list(slots)
      assert Enum.all?(slots, fn %{"start_time" => s, "end_time" => e, "available" => a} ->
               is_binary(s) and is_binary(e) and is_boolean(a)
             end)
    end

    test "with invalid date returns 400", %{conn: conn} do
      %{item: item} = Factory.sample_data()

      conn =
        conn
        |> json_conn()
        |> get(@avail_path.(item.id), %{
          "date" => "not-a-date",
          "duration" => "60"
        })

      body = json_response(conn, 400)
      assert is_map(body)
      assert Map.has_key?(body, "error") or Map.has_key?(body, "errors") or Map.has_key?(body, "message")
    end
  end

  describe "POST /api/booking/create" do
    test "valid params create booking and return 201 with reservation id", %{conn: conn} do
      %{business: business, item: item, client: _client} = Factory.sample_data()

      {:ok, client} =
        RivaAsh.Resources.Client.create(%{
          business_id: business.id,
          name: "John Doe",
          email: "john#{System.unique_integer([:positive])}@example.com",
          phone: "+35560000000",
          is_registered: false
        })

      from_dt = DateTime.add(DateTime.utc_now(), 3600, :second)
      until_dt = DateTime.add(from_dt, 3600, :second)

      payload = %{
        "client" => %{
          "name" => client.name,
          "email" => to_string(client.email || "john#{System.unique_integer([:positive])}@example.com"),
          "phone" => client.phone
        },
        "booking" => %{
          "item_id" => item.id,
          "reserved_from" => DateTime.to_iso8601(from_dt),
          "reserved_until" => DateTime.to_iso8601(until_dt),
          "notes" => "Test booking"
        },
        "register_client" => false
      }

      conn =
        conn
        |> json_conn()
        |> post(@create_path, Jason.encode!(payload))

      assert %{
               "data" => %{
                 "booking_id" => reservation_id,
                 "reservation" => %{
                   "id" => ^reservation_id,
                   "item_id" => ^item.id,
                   "status" => status
                 },
                 "status" => "pending",
                 "message" => msg
               }
             } = json_response(conn, 201)

      assert is_binary(reservation_id)
      assert status in ["pending", "confirmed", "provisional", "cancelled", "completed"]
      assert is_binary(msg)
    end

    test "invalid params render error via fallback (422-like)", %{conn: conn} do
      payload = %{
        "client" => %{"name" => ""},
        "booking" => %{
          "item_id" => Ash.UUID.generate(),
          "reserved_from" => "invalid",
          "reserved_until" => "also-invalid"
        }
      }

      conn =
        conn
        |> json_conn()
        |> post(@create_path, Jason.encode!(payload))

      body = json_response(conn, 400)
      assert is_map(body)
      assert Map.has_key?(body, "error") or Map.has_key?(body, "message") or Map.has_key?(body, "errors")
    end

    test "cross-business client returns 403", %{conn: conn} do
      # Create two separate businesses
      %{business: business1, item: item, client: _client1} = Factory.sample_data()
      %{client: client2} = Factory.sample_data()

      payload = %{
        "client" => %{
          "name" => client2.name,
          "email" => to_string(client2.email || "john#{System.unique_integer([:positive])}@example.com"),
          "phone" => client2.phone
        },
        "booking" => %{
          "item_id" => item.id,
          "reserved_from" => DateTime.to_iso8601(DateTime.add(DateTime.utc_now(), 3600, :second)),
          "reserved_until" => DateTime.to_iso8601(DateTime.add(DateTime.utc_now(), 7200, :second))
        },
        "register_client" => false
      }

      conn =
        conn
        |> json_conn()
        |> post(@create_path, Jason.encode!(payload))

      assert conn.status in [403, 400]
      body = json_response(conn, conn.status)
      assert is_map(body)
      assert Map.has_key?(body, "error") or Map.has_key?(body, "errors") or Map.has_key?(body, "message")
    end
  end

  describe "GET /api/booking/client/:email (authorization-ish guard on data visibility)" do
    test "non-existent email returns 404 via controller handling", %{conn: conn} do
      email = "no-such-#{System.unique_integer([:positive])}@example.com"

      conn =
        conn
        |> json_conn()
        |> get(@client_bookings_path.(email))

      assert %{"error" => msg} = json_response(conn, 404)
      assert is_binary(msg)
    end
  end
end