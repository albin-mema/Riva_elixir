defmodule RivaAsh.Booking do
  @moduledoc """
  Handles client-facing booking operations with flexible registration flow.

  Supports both:
  - Immediate registration during booking
  - Registration at confirmation time
  - Unregistered client bookings
  """

  alias RivaAsh.Resources.{Client, Reservation, Item}
  alias RivaAsh.Domain

  @doc """
  Creates a booking with flexible client registration.

  ## Flow Options:
  1. **Unregistered booking**: Client provides minimal info, books immediately
  2. **Register at confirmation**: Client can upgrade to registered during confirmation
  3. **Pre-registered**: Existing registered clients book normally

  ## Parameters:
  - `booking_params`: Map with booking details
    - `client_info`: %{name: string, email: string (optional), phone: string (optional)}
    - `item_id`: UUID of item to reserve
    - `reserved_from`: DateTime
    - `reserved_until`: DateTime
    - `notes`: Optional notes
    - `register_client`: Boolean (default: false) - whether to register client immediately

  ## Returns:
  - `{:ok, %{client: client, reservation: reservation}}` on success
  - `{:error, reason}` on failure
  """
  def create_booking(booking_params) do
    with {:ok, client} <- find_or_create_client(booking_params),
         {:ok, reservation} <- create_reservation(client, booking_params) do
      {:ok, %{client: client, reservation: reservation}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Confirms a pending reservation and optionally registers the client.

  ## Parameters:
  - `reservation_id`: UUID of the reservation to confirm
  - `register_client`: Boolean - whether to register the client
  - `client_updates`: Optional map with additional client info for registration

  ## Returns:
  - `{:ok, %{client: client, reservation: reservation}}` on success
  - `{:error, reason}` on failure
  """
  def confirm_booking(reservation_id, register_client \\ false, client_updates \\ %{}) do
    with {:ok, reservation} <- get_reservation_with_client(reservation_id),
         {:ok, updated_client} <- maybe_register_client(reservation.client, register_client, client_updates),
         {:ok, confirmed_reservation} <- confirm_reservation(reservation) do
      {:ok, %{client: updated_client, reservation: confirmed_reservation}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Gets available time slots for an item on a specific date.

  ## Parameters:
  - `item_id`: UUID of the item
  - `date`: Date to check availability
  - `duration_minutes`: Duration of the booking in minutes (default: 60)
  - `business_hours`: Map with start/end times (default: 9-17)

  ## Returns:
  - `{:ok, [%{start_time: datetime, end_time: datetime, available: boolean}]}`
  - `{:error, reason}` on failure
  """
  def get_availability(item_id, date, duration_minutes \\ 60, business_hours \\ %{start: 9, end: 17}) do
    with {:ok, _item} <- Item.by_id(item_id),
         {:ok, existing_reservations} <- get_existing_reservations(item_id, date),
         time_slots <- generate_time_slots(date, duration_minutes, business_hours),
         available_slots <- mark_availability(time_slots, existing_reservations) do
      {:ok, available_slots}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Private functions

  defp find_or_create_client(%{client_info: client_info, register_client: register_immediately}) when register_immediately do
    # Create registered client immediately
    client_attrs = Map.put(client_info, :is_registered, true)

    case Client.create(client_attrs, domain: Domain) do
      {:ok, client} -> {:ok, client}
      {:error, error} -> {:error, format_error(error)}
    end
  end

  defp find_or_create_client(%{client_info: client_info}) do
    # Create unregistered client or find existing by email
    case client_info do
      %{email: email} when not is_nil(email) ->
        # Try to find existing client by email first
        case Client.by_email(email, domain: Domain) do
          {:ok, existing_client} -> {:ok, existing_client}
          {:error, _not_found} ->
            # Create new unregistered client
            Client.create_for_booking(client_info, domain: Domain)
        end

      _ ->
        # No email provided, create unregistered client
        Client.create_for_booking(client_info, domain: Domain)
    end
  end

  defp create_reservation(client, %{item_id: item_id, reserved_from: reserved_from, reserved_until: reserved_until} = params) do
    reservation_attrs = %{
      client_id: client.id,
      item_id: item_id,
      reserved_from: reserved_from,
      reserved_until: reserved_until,
      notes: Map.get(params, :notes)
    }

    case Reservation.create_for_booking(reservation_attrs, domain: Domain) do
      {:ok, reservation} -> {:ok, reservation}
      {:error, error} -> {:error, format_error(error)}
    end
  end

  defp get_reservation_with_client(reservation_id) do
    case Reservation.by_id(reservation_id, domain: Domain, load: [:client]) do
      {:ok, reservation} -> {:ok, reservation}
      {:error, error} -> {:error, format_error(error)}
    end
  end

  defp maybe_register_client(client, false, _client_updates), do: {:ok, client}

  defp maybe_register_client(client, true, client_updates) do
    if client.is_registered do
      {:ok, client}
    else
      # Upgrade to registered client
      update_attrs = Map.merge(client_updates, %{is_registered: true})

      case Client.register(client, update_attrs, domain: Domain) do
        {:ok, updated_client} -> {:ok, updated_client}
        {:error, error} -> {:error, format_error(error)}
      end
    end
  end

  defp confirm_reservation(reservation) do
    case Reservation.update(reservation, %{status: :confirmed}, domain: Domain) do
      {:ok, updated_reservation} -> {:ok, updated_reservation}
      {:error, error} -> {:error, format_error(error)}
    end
  end

  defp get_existing_reservations(item_id, date) do
    start_of_day = DateTime.new!(date, ~T[00:00:00], "Etc/UTC")
    end_of_day = DateTime.new!(date, ~T[23:59:59], "Etc/UTC")

    case Reservation.by_item(item_id, domain: Domain) do
      {:ok, reservations} ->
        filtered = Enum.filter(reservations, fn res ->
          DateTime.compare(res.reserved_from, start_of_day) != :lt and
          DateTime.compare(res.reserved_until, end_of_day) != :gt and
          res.status in [:confirmed, :pending]
        end)
        {:ok, filtered}

      {:error, error} -> {:error, format_error(error)}
    end
  end

  defp generate_time_slots(date, duration_minutes, %{start: start_hour, end: end_hour}) do
    start_time = DateTime.new!(date, Time.new!(start_hour, 0, 0), "Etc/UTC")
    end_time = DateTime.new!(date, Time.new!(end_hour, 0, 0), "Etc/UTC")

    generate_slots(start_time, end_time, duration_minutes, [])
  end

  defp generate_slots(current_time, end_time, duration_minutes, acc) do
    slot_end = DateTime.add(current_time, duration_minutes * 60, :second)

    if DateTime.compare(slot_end, end_time) == :gt do
      Enum.reverse(acc)
    else
      slot = %{start_time: current_time, end_time: slot_end, available: true}
      next_time = DateTime.add(current_time, duration_minutes * 60, :second)
      generate_slots(next_time, end_time, duration_minutes, [slot | acc])
    end
  end

  defp mark_availability(time_slots, existing_reservations) do
    Enum.map(time_slots, fn slot ->
      available = not Enum.any?(existing_reservations, fn reservation ->
        times_overlap?(slot.start_time, slot.end_time, reservation.reserved_from, reservation.reserved_until)
      end)

      %{slot | available: available}
    end)
  end

  defp times_overlap?(start1, end1, start2, end2) do
    DateTime.compare(start1, end2) == :lt and DateTime.compare(end1, start2) == :gt
  end

  defp format_error(error) do
    case error do
      %{errors: errors} when is_list(errors) ->
        errors
        |> Enum.map(&Exception.message/1)
        |> Enum.join(", ")

      %{message: message} -> message
      error when is_binary(error) -> error
      _ -> "An unexpected error occurred"
    end
  end
end
