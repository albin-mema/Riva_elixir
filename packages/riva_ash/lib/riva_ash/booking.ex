defmodule RivaAsh.Booking do
  @moduledoc """
  Handles client-facing booking operations with flexible registration flow.

  Supports both:
  - Immediate registration during booking
  - Registration at confirmation time
  - Unregistered client bookings
  """

  use Timex

  alias RivaAsh.Resources.{Client, Reservation, Item}
  alias RivaAsh.Domain
  alias RivaAsh.Repo
  alias Ecto.Multi


  require Ash.Query



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
  - `{:error, map}` on failure, where map contains :code and :message

  Creates a new booking within a transaction to ensure data consistency.
  The transaction will be rolled back if any step fails.
  """
  def create_booking(booking_params) do
    require Logger

    RivaAsh.ErrorHelpers.with_error_handling(fn ->
      Multi.new()
      |> Multi.run(:client, fn _repo, _changes ->
        client_params = Map.get(booking_params, :client_info, %{})

        # Try to find existing client by email if provided
        find_or_create_client(client_params, Map.get(booking_params, :register_client, false))
        |> case do
          {:ok, client} ->
            {:ok, client}

          {:error, error} ->
            {:error, RivaAsh.ErrorHelpers.format_error(error)}
        end
      end)
      |> Multi.run(:reservation, fn _repo, %{client: client} ->
        reservation_params =
          booking_params
          |> Map.put(:client_id, client.id)

        case RivaAsh.Validations.check_item_availability(
               reservation_params.item_id,
               reservation_params.reserved_from,
               reservation_params.reserved_until
             ) do
          {:ok, :available} ->
            case create_reservation(reservation_params) do
              {:ok, reservation} ->
                {:ok, reservation}

              {:error, error} ->
                {:error, RivaAsh.ErrorHelpers.format_error(error)}
            end

          {:ok, {:unavailable, reason}} ->
            {:error, %{code: :item_unavailable, message: reason}}

          {:error, reason} ->
            {:error, RivaAsh.ErrorHelpers.format_error(reason)}
        end
      end)
      |> Repo.transaction()
      |> case do
        {:ok, %{client: client, reservation: reservation}} ->
          {:ok, %{client: client, reservation: reservation}}

        {:error, _operation, reason, _changes} ->
          {:error, reason}
      end
    end)
  end

  @doc """
  Confirms a pending reservation and optionally registers the client.

  ## Parameters:
  - `reservation_id`: UUID of the reservation to confirm
  - `register_client`: Boolean - whether to register the client
  - `client_updates`: Optional map with additional client info for registration

  ## Returns:
  - `{:ok, %{client: client, reservation: reservation}}` on success
  - `{:error, map}` on failure, where map contains :code and :message
  """
  def confirm_booking(reservation_id, register_client \\ false, client_updates \\ %{}) do
    with {:ok, reservation} <- get_reservation_with_client(reservation_id),
         {:ok, updated_client} <-
           maybe_register_client(reservation.client, register_client, client_updates),
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
  - `{:error, map}` on failure, where map contains :code and :message
  """
  def get_availability(
        item_id,
        date,
        duration_minutes \\ 60,
        business_hours \\ %{start: 9, end: 17}
      ) do
    with {:ok, _item} <- Item.by_id(item_id),
         {:ok, existing_reservations} <- get_existing_reservations(item_id, date) do
      time_slots = generate_time_slots(date, duration_minutes, business_hours)
      available_slots = mark_availability(time_slots, existing_reservations)
      {:ok, available_slots}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Private functions

  defp find_or_create_client(%{email: email} = params, register_immediately)
       when is_binary(email) and email != "" do
    Client
    |> Ash.Query.filter(email: String.downcase(email))
    |> Ash.read_one(domain: Domain)
    |> case do
      nil ->
        # Create new client if not found
        Client.create(Map.put(params, :is_registered, register_immediately))
        |> case do
          {:ok, client} ->
            {:ok, client}

          {:error, changeset} ->
            {:error,
             %{
               code: :client_creation_failed,
               message: "Failed to create client",
               errors: changeset.errors
             }}
        end

      client ->
        # Return existing client
        {:ok, client}
    end
  end

  defp find_or_create_client(params, register_immediately) do
    # Create unregistered client without email
    Client.create(Map.put(params, :is_registered, register_immediately))
    |> case do
      {:ok, client} ->
        {:ok, client}

      {:error, changeset} ->
        {:error,
         %{
           code: :client_creation_failed,
           message: "Failed to create client",
           errors: changeset.errors
         }}
    end
  end

  defp create_reservation(params) do
    Reservation.create(params)
    |> case do
      {:ok, reservation} ->
        {:ok, reservation}

      {:error, changeset} ->
        {:error,
         %{
           code: :reservation_creation_failed,
           message: "Failed to create reservation",
           errors: changeset.errors
         }}
    end
  end

  # Removed validate_booking_times and check_item_availability as they are now handled by Validations module.
  # defp validate_booking_times(reserved_from, reserved_until) do
  #   # Logic moved to RivaAsh.Validations
  # end
  #
  # defp check_item_availability(item_id, reserved_from, reserved_until) do
  #   # Logic moved to RivaAsh.Validations
  # end

  defp get_reservation_with_client(reservation_id) do
    Reservation.by_id(reservation_id, domain: Domain, load: [:client])
    |> case do
      {:ok, reservation} -> {:ok, reservation}
      {:error, error} -> {:error, RivaAsh.ErrorHelpers.format_error(error)}
    end
  end

  defp maybe_register_client(client, false, _client_updates), do: {:ok, client}

  defp maybe_register_client(client, true, client_updates) do
    if client.is_registered do
      {:ok, client}
    else
      # Upgrade to registered client
      update_attrs = Map.merge(client_updates, %{is_registered: true})

      Client.register(client, update_attrs, domain: Domain)
      |> case do
        {:ok, updated_client} -> {:ok, updated_client}
        {:error, error} -> {:error, RivaAsh.ErrorHelpers.format_error(error)}
      end
    end
  end

  defp confirm_reservation(reservation) do
    Reservation.update(reservation, %{status: :confirmed}, domain: Domain)
    |> case do
      {:ok, updated_reservation} -> {:ok, updated_reservation}
      {:error, error} -> {:error, RivaAsh.ErrorHelpers.format_error(error)}
    end
  end

  defp get_existing_reservations(item_id, date) do
    start_of_day = DateTime.new!(date, ~T[00:00:00], "Etc/UTC")
    end_of_day = DateTime.new!(date, ~T[23:59:59], "Etc/UTC")

    Reservation.by_item(item_id, domain: Domain)
    |> case do
      {:ok, reservations} ->
        filtered =
          Enum.filter(reservations, fn res ->
            Timex.compare(res.reserved_from, start_of_day) != -1 and
              Timex.compare(res.reserved_until, end_of_day) != 1 and
              res.status in [:confirmed, :pending]
          end)

        {:ok, filtered}

      {:error, error} ->
        {:error, RivaAsh.ErrorHelpers.format_error(error)}
    end
  end

  defp generate_time_slots(date, duration_minutes, %{start: start_hour, end: end_hour}) do
    start_time = DateTime.new!(date, Time.new!(start_hour, 0, 0), "Etc/UTC")
    end_time = DateTime.new!(date, Time.new!(end_hour, 0, 0), "Etc/UTC")

    generate_slots(start_time, end_time, duration_minutes, [])
  end

  defp generate_slots(current_time, end_time, duration_minutes, acc) do
    slot_end = Timex.shift(current_time, minutes: duration_minutes)

    if Timex.compare(slot_end, end_time) == 1 do
      Enum.reverse(acc)
    else
      slot = %{start_time: current_time, end_time: slot_end, available: true}
      next_time = Timex.shift(current_time, minutes: duration_minutes)
      generate_slots(next_time, end_time, duration_minutes, [slot | acc])
    end
  end

  defp mark_availability(time_slots, existing_reservations) do
    Enum.map(time_slots, fn slot ->
      available =
        not Enum.any?(existing_reservations, fn reservation ->
          times_overlap?(
            slot.start_time,
            slot.end_time,
            reservation.reserved_from,
            reservation.reserved_until
          )
        end)

      %{slot | available: available}
    end)
  end

  defp times_overlap?(start1, end1, start2, end2) do
    Timex.compare(start1, end2) == -1 and Timex.compare(end1, start2) == 1
  end
end
