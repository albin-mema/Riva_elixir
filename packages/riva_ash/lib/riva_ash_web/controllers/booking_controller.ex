defmodule RivaAshWeb.BookingController do
  @moduledoc """
  Client-facing booking API endpoints.

  Provides simplified, user-friendly endpoints for clients to:
  - Browse availability
  - Create bookings (with optional registration)
  - Confirm bookings (with optional registration upgrade)
  - View their bookings
  """

  use Phoenix.Controller, formats: [:json]

  alias RivaAsh.Booking
  alias RivaAsh.Resources.{Item, Client}
  alias RivaAsh.ErrorHelpers

  action_fallback(RivaAshWeb.FallbackController)

  @doc """
  GET /api/booking/availability/:item_id

  Get available time slots for an item on a specific date.

  Query parameters:
  - date: YYYY-MM-DD (required)
  - duration: Duration in minutes (default: 60)
  - start_hour: Business start hour (default: 9)
  - end_hour: Business end hour (default: 17)
  """
  def availability(conn, %{"item_id" => item_id} = params) do
    with {:ok, date} <- parse_date(params["date"]),
         duration = parse_duration(params["duration"]),
         business_hours = parse_business_hours(params),
         {:ok, slots} <- Booking.get_availability(item_id, date, duration, business_hours) do
      conn
      |> put_status(:ok)
      |> json(%{
        data: %{
          item_id: item_id,
          date: Date.to_iso8601(date),
          duration_minutes: duration,
          business_hours: business_hours,
          time_slots: format_time_slots(slots)
        }
      })
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  @doc """
  POST /api/booking/create

  Create a new booking. Client can optionally register immediately.

  Body:
  {
    "client": {
      "name": "John Doe",
      "email": "john@example.com", // optional
      "phone": "+1234567890"       // optional
    },
    "booking": {
      "item_id": "uuid",
      "reserved_from": "2024-01-01T10:00:00Z",
      "reserved_until": "2024-01-01T11:00:00Z",
      "notes": "Optional notes"
    },
    "register_client": false  // optional, default false
  }
  """
  def create(conn, params) do
    with {:ok, booking_params} <- parse_booking_params(params),
         {:ok, result} <- Booking.create_booking(booking_params) do
      conn
      |> put_status(:created)
      |> json(%{
        data: %{
          booking_id: result.reservation.id,
          client: format_client(result.client),
          reservation: format_reservation(result.reservation),
          status: "pending",
          message:
            if(result.client.is_registered,
              do: "Booking created successfully! You are now registered.",
              else: "Booking created successfully! You can register when confirming your booking."
            )
        }
      })
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  @doc """
  POST /api/booking/confirm/:booking_id

  Confirm a pending booking. Client can optionally register during confirmation.

  Body:
  {
    "register_client": false,  // optional
    "client_updates": {        // optional, for registration
      "email": "updated@example.com"
    }
  }
  """
  def confirm(conn, %{"booking_id" => booking_id} = params) do
    register_client = Map.get(params, "register_client", false)
    client_updates = Map.get(params, "client_updates", %{})

    case Booking.confirm_booking(booking_id, register_client, client_updates) do
      {:ok, result} ->
        conn
        |> put_status(:ok)
        |> json(%{
          data: %{
            booking_id: result.reservation.id,
            client: format_client(result.client),
            reservation: format_reservation(result.reservation),
            status: "confirmed",
            message:
              if(result.client.is_registered and register_client,
                do: "Booking confirmed and you are now registered! Welcome!",
                else: "Booking confirmed successfully!"
              )
          }
        })

      {:error, reason} ->
        ErrorHelpers.failure(reason)
    end
  end

  @doc """
  GET /api/booking/items

  Get list of available items for booking.
  """
  def items(conn, _params) do
    case Item.read(domain: RivaAsh.Domain) do
      {:ok, items} ->
        conn
        |> put_status(:ok)
        |> json(%{
          data: Enum.map(items, &format_item/1)
        })

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  GET /api/booking/client/:email

  Get client bookings by email (for unregistered clients or simple lookup).
  """
  def client_bookings(conn, %{"email" => email}) do
    case Client.by_email(email, domain: RivaAsh.Domain, load: [:reservations]) do
      {:ok, client} ->
        conn
        |> put_status(:ok)
        |> json(%{
          data: %{
            client: format_client(client),
            bookings: Enum.map(client.reservations, &format_reservation/1)
          }
        })

      {:error, %Ash.Error.Query.NotFound{}} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "No bookings found for this email address"})

      {:error, reason} ->
        ErrorHelpers.failure(reason)
    end
  end

  # Private helper functions

  defp parse_date(nil), do: ErrorHelpers.failure("Date parameter is required")

  defp parse_date(date_string) do
    date_string
    |> Date.from_iso8601()
    |> case do
      {:ok, date} -> ErrorHelpers.success(date)
      {:error, _} -> ErrorHelpers.failure("Invalid date format. Use YYYY-MM-DD")
    end
  end

  defp parse_duration(nil), do: 60

  defp parse_duration(duration_string) when is_binary(duration_string) do
    case Integer.parse(duration_string) do
      {duration, ""} when duration > 0 -> duration
      _ -> 60
    end
  end

  defp parse_duration(duration) when is_integer(duration) and duration > 0, do: duration
  defp parse_duration(_), do: 60

  defp parse_business_hours(params) do
    start_hour = parse_hour(params["start_hour"], 9)
    end_hour = parse_hour(params["end_hour"], 17)
    %{start: start_hour, end: end_hour}
  end

  defp parse_hour(nil, default), do: default

  defp parse_hour(hour_string, default) when is_binary(hour_string) do
    case Integer.parse(hour_string) do
      {hour, ""} when hour >= 0 and hour <= 23 -> hour
      _ -> default
    end
  end

  defp parse_hour(hour, _default) when is_integer(hour) and hour >= 0 and hour <= 23, do: hour
  defp parse_hour(_, default), do: default

  defp parse_booking_params(params) do
    with {:ok, client_info} <- extract_client_info(params),
         {:ok, booking_info} <- extract_booking_info(params),
         register_client = Map.get(params, "register_client", false) do
      ErrorHelpers.success(%{
        client_info: client_info,
        item_id: booking_info.item_id,
        reserved_from: booking_info.reserved_from,
        reserved_until: booking_info.reserved_until,
        notes: booking_info[:notes],
        register_client: register_client
      })
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  defp extract_client_info(%{"client" => client_params}) do
    case client_params do
      %{"name" => name} when is_binary(name) and name != "" ->
        ErrorHelpers.success(%{
          name: name,
          email: client_params["email"],
          phone: client_params["phone"]
        })

      _ ->
        ErrorHelpers.failure("Client name is required")
    end
  end

  defp extract_client_info(_), do: ErrorHelpers.failure("Client information is required")

  defp extract_booking_info(%{"booking" => booking_params}) do
    with {:ok, reserved_from} <- parse_datetime(booking_params["reserved_from"]),
         {:ok, reserved_until} <- parse_datetime(booking_params["reserved_until"]) do
      ErrorHelpers.success(%{
        item_id: booking_params["item_id"],
        reserved_from: reserved_from,
        reserved_until: reserved_until,
        notes: booking_params["notes"]
      })
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  defp extract_booking_info(_), do: ErrorHelpers.failure("Booking information is required")

  defp parse_datetime(nil), do: ErrorHelpers.failure("DateTime is required")

  defp parse_datetime(datetime_string) when is_binary(datetime_string) do
    datetime_string
    |> DateTime.from_iso8601()
    |> case do
      {:ok, datetime, _offset} -> ErrorHelpers.success(datetime)
      {:error, _} -> ErrorHelpers.failure("Invalid datetime format. Use ISO8601 format")
    end
  end

  # Formatting functions

  defp format_time_slots(slots) do
    Enum.map(slots, fn slot ->
      %{
        start_time: DateTime.to_iso8601(slot.start_time),
        end_time: DateTime.to_iso8601(slot.end_time),
        available: slot.available
      }
    end)
  end

  defp format_client(client) do
    %{
      id: client.id,
      name: client.name,
      email: client.email,
      phone: client.phone,
      is_registered: client.is_registered
    }
  end

  defp format_reservation(reservation) do
    %{
      id: reservation.id,
      reserved_from: DateTime.to_iso8601(reservation.reserved_from),
      reserved_until: DateTime.to_iso8601(reservation.reserved_until),
      status: reservation.status,
      notes: reservation.notes,
      item_id: reservation.item_id
    }
  end

  defp format_item(item) do
    %{
      id: item.id,
      name: item.name
    }
  end
end
