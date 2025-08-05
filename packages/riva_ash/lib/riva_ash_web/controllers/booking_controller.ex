defmodule RivaAshWeb.BookingController do
  @moduledoc """
  Client-facing booking API endpoints.

  Provides simplified, user-friendly endpoints for clients to:
  - Browse availability
  - Create bookings (with optional registration)
  - Confirm bookings (with optional registration upgrade)
  - View their bookings

  Uses functional programming patterns with proper error handling and
  type safety specifications.
  """

  use Phoenix.Controller, formats: [:json]

  alias RivaAsh.Booking
  alias RivaAsh.Resources.{Item, Client}
  alias RivaAsh.ErrorHelpers

  action_fallback(RivaAshWeb.FallbackController)

  @type conn :: Plug.Conn.t()
  @type params :: map()
  @type result :: {:ok, any()} | {:error, any()}
  @type date :: Date.t()
  @type datetime :: DateTime.t()
  @type duration :: pos_integer()
  @type business_hours :: %{start: integer(), end: integer()}

  @doc """
  Get available time slots for an item on a specific date.

  ## Parameters
    - item_id: The ID of the item to check availability for
    - params: Query parameters including date, duration, business hours

  ## Query Parameters
    - date: YYYY-MM-DD (required)
    - duration: Duration in minutes (default: 60)
    - start_hour: Business start hour (default: 9)
    - end_hour: Business end hour (default: 17)

  ## Returns
    - 200: JSON response with availability data
    - 400: Error response for invalid parameters
  """
  @spec availability(conn(), params()) :: conn()
  def availability(conn, %{"item_id" => item_id} = params) do
    with {:ok, date} <- parse_date(params["date"]),
         duration <- parse_duration(params["duration"]),
         business_hours <- parse_business_hours(params),
         {:ok, slots} <- Booking.get_availability(item_id, date, duration, business_hours) do
      render_availability_response(conn, item_id, date, duration, business_hours, slots)
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  @doc """
  Create a new booking with optional client registration.

  ## Parameters
    - params: Request body with client and booking information

  ## Request Body
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

  ## Returns
    - 201: JSON response with booking confirmation
    - 400: Error response for invalid parameters
  """
  @spec create(conn(), params()) :: conn()
  def create(conn, params) do
    with {:ok, booking_params} <- parse_booking_params(params),
         {:ok, result} <- Booking.create_booking(booking_params) do
      render_booking_creation_response(conn, result)
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  @doc """
  Confirm a pending booking with optional client registration.

  ## Parameters
    - booking_id: The ID of the booking to confirm
    - params: Request body with registration options

  ## Request Body
    {
      "register_client": false,  // optional
      "client_updates": {        // optional, for registration
        "email": "updated@example.com"
      }
    }

  ## Returns
    - 200: JSON response with confirmation details
    - 400: Error response for invalid parameters
    - 404: Error response for booking not found
  """
  @spec confirm(conn(), params()) :: conn()
  def confirm(conn, %{"booking_id" => booking_id} = params) do
    with {:ok, result} <- process_booking_confirmation(booking_id, params) do
      render_booking_confirmation_response(conn, result)
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  @doc """
  Get list of available items for booking.

  ## Returns
    - 200: JSON response with list of available items
    - 500: Error response for server errors
  """
  @spec items(conn(), params()) :: conn()
  def items(conn, _params) do
    case Item.read(domain: RivaAsh.Domain) do
      {:ok, items} ->
        conn
        |> put_status(:ok)
        |> json(%{data: Enum.map(items, &format_item/1)})

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Get client bookings by email for unregistered clients.

  ## Parameters
    - email: The email address to search for

  ## Returns
    - 200: JSON response with client bookings
    - 404: Error response if no bookings found
    - 400: Error response for invalid email
  """
  @spec client_bookings(conn(), params()) :: conn()
  def client_bookings(conn, %{"email" => email}) do
    with {:ok, client} <- find_client_by_email(email) do
      render_client_bookings_response(conn, client)
    else
      {:error, %Ash.Error.Query.NotFound{}} ->
        render_no_bookings_found(conn)
      {:error, reason} ->
        ErrorHelpers.failure(reason)
    end
  end

  # Private helper functions

  defp render_availability_response(conn, item_id, date, duration, business_hours, slots) do
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
  end

  defp render_booking_creation_response(conn, result) do
    conn
    |> put_status(:created)
    |> json(%{
      data: %{
        booking_id: result.reservation.id,
        client: format_client(result.client),
        reservation: format_reservation(result.reservation),
        status: "pending",
        message: generate_booking_creation_message(result.client)
      }
    })
  end

  defp render_booking_confirmation_response(conn, result) do
    conn
    |> put_status(:ok)
    |> json(%{
      data: %{
        booking_id: result.reservation.id,
        client: format_client(result.client),
        reservation: format_reservation(result.reservation),
        status: "confirmed",
        message: generate_booking_confirmation_message(result.client)
      }
    })
  end

  defp render_client_bookings_response(conn, client) do
    conn
    |> put_status(:ok)
    |> json(%{
      data: %{
        client: format_client(client),
        bookings: Enum.map(client.reservations, &format_reservation/1)
      }
    })
  end

  defp render_no_bookings_found(conn) do
    conn
    |> put_status(:not_found)
    |> json(%{error: "No bookings found for this email address"})
  end

  defp generate_booking_creation_message(%{is_registered: true}) do
    "Booking created successfully! You are now registered."
  end

  defp generate_booking_creation_message(%{is_registered: false}) do
    "Booking created successfully! You can register when confirming your booking."
  end

  defp generate_booking_confirmation_message(%{is_registered: true}) do
    "Booking confirmed successfully!"
  end

  defp generate_booking_confirmation_message(%{is_registered: false}) do
    "Booking confirmed successfully!"
  end

  defp process_booking_confirmation(booking_id, params) do
    register_client = Map.get(params, "register_client", false)
    client_updates = Map.get(params, "client_updates", %{})
    Booking.confirm_booking(booking_id, register_client, client_updates)
  end

  defp find_client_by_email(email) do
    Client.by_email(email, domain: RivaAsh.Domain, load: [:reservations])
  end

  # Parameter parsing functions

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
         register_client <- Map.get(params, "register_client", false) do
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

  # Data formatting functions

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