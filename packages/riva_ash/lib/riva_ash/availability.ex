defmodule RivaAsh.Availability do
  @moduledoc """
  Functions for checking item availability based on schedules, exceptions, and existing reservations.

  This module combines:
  1. Item schedules (recurring weekly patterns)
  2. Availability exceptions (holidays, maintenance, etc.)
  3. Existing reservations
  4. Item capacity

  To determine if an item is available for a given time slot.
  """

  use Timex

  alias RivaAsh.Resources.ItemSchedule
  alias RivaAsh.Resources.AvailabilityException
  alias RivaAsh.Resources.Reservation

  @doc """
  Check if an item is available for the given time period.

  Returns:
  - `{:ok, :available}` - Item is available
  - `{:ok, :partial, available_capacity}` - Item has partial availability (for items with capacity > 1)
  - `{:error, reason}` - Item is not available

  ## Examples

      iex> RivaAsh.Availability.check_availability(item_id, ~U[2024-01-15 10:00:00Z], ~U[2024-01-15 12:00:00Z])
      {:ok, :available}

      iex> RivaAsh.Availability.check_availability(item_id, ~U[2024-01-15 02:00:00Z], ~U[2024-01-15 04:00:00Z])
      {:error, :outside_business_hours}
  """
  @spec check_availability(String.t(), DateTime.t(), DateTime.t()) ::
          {:ok, :available} | {:ok, {:partial, non_neg_integer()}} | {:error, atom() | String.t()}
  def check_availability(item_id, start_datetime, end_datetime) when is_binary(item_id) do
    with {:ok, item} <- get_item(item_id),
         :ok <- validate_item_active(item),
         :ok <- validate_duration(item, start_datetime, end_datetime),
         :ok <- check_schedule_availability(item, start_datetime, end_datetime),
         :ok <- check_exceptions(item_id, start_datetime, end_datetime),
         {:ok, available_capacity} <-
           check_reservation_conflicts(item_id, start_datetime, end_datetime, item.capacity) do
      determine_availability_status(available_capacity, item.capacity)
    else
      error -> error
    end
  end

  @spec determine_availability_status(non_neg_integer(), non_neg_integer()) ::
          {:ok, :available} | {:ok, {:partial, non_neg_integer()}}
  defp determine_availability_status(available_capacity, total_capacity) do
    if available_capacity == total_capacity do
      {:ok, :available}
    else
      {:ok, {:partial, available_capacity}}
    end
  end

  @doc """
  Get available time slots for an item on a specific date.

  Returns a list of available time ranges considering all constraints.
  """
  @spec get_available_slots(String.t(), Date.t(), non_neg_integer()) ::
          {:ok, list({Time.t(), Time.t()})} | {:error, atom() | String.t()}
  def get_available_slots(item_id, date, slot_duration_minutes \\ 60)
      when is_binary(item_id) and is_integer(slot_duration_minutes) and slot_duration_minutes > 0 do
    with {:ok, item} <- get_item(item_id),
         :ok <- validate_item_active(item) do
      day_of_week = if RivaAsh.DateTimeHelpers.weekend?(date), do: 0, else: Timex.weekday(date)

      base_slots = get_base_slots(item, item_id, day_of_week, date, slot_duration_minutes)
      available_slots = filter_and_sort_slots(base_slots, item_id, date, item.capacity)

      {:ok, available_slots}
    else
      error -> error
    end
  end

  @spec get_base_slots(map(), String.t(), integer(), Date.t(), non_neg_integer()) :: list({Time.t(), Time.t()})
  defp get_base_slots(%{is_always_available: true}, _item_id, _day_of_week, date, slot_duration_minutes) do
    create_24_hour_slots(date, slot_duration_minutes)
  end

  defp get_base_slots(_item, item_id, day_of_week, date, slot_duration_minutes) do
    case get_scheduled_slots(item_id, day_of_week, date, slot_duration_minutes) do
      {:ok, slots} -> slots
      {:error, _reason} -> []
    end
  end

  @spec filter_and_sort_slots(list({Time.t(), Time.t()}), String.t(), Date.t(), non_neg_integer()) ::
          list({Time.t(), Time.t()})
  defp filter_and_sort_slots(base_slots, item_id, date, capacity) do
    base_slots
    |> filter_exceptions(item_id, date)
    |> filter_reservations(item_id, date, capacity)
  end

  # Private functions with comprehensive specs

  @spec get_item(String.t()) :: {:ok, map()} | {:error, atom()}
  defp get_item(item_id) when is_binary(item_id) do
    case RivaAsh.Resources.Item.by_id(item_id) do
      {:ok, item} -> {:ok, item}
      {:error, _item_not_found} -> {:error, :item_not_found}
    end
  end

  @spec validate_item_active(map()) :: :ok | {:error, atom()}
  defp validate_item_active(%{is_active: true}), do: :ok
  defp validate_item_active(_inactive_item), do: {:error, :item_inactive}

  @spec validate_duration(map(), DateTime.t(), DateTime.t()) :: :ok | {:error, atom()}
  defp validate_duration(item, start_datetime, end_datetime) do
    duration_minutes = Timex.diff(end_datetime, start_datetime, :minutes)

    cond do
      duration_minutes <= 0 ->
        {:error, :invalid_duration}

      item.minimum_duration_minutes && duration_minutes < item.minimum_duration_minutes ->
        {:error, :duration_too_short}

      item.maximum_duration_minutes && duration_minutes > item.maximum_duration_minutes ->
        {:error, :duration_too_long}

      true ->
        :ok
    end
  end

  @spec check_schedule_availability(map(), DateTime.t(), DateTime.t()) :: :ok | {:error, atom()}
  defp check_schedule_availability(%{is_always_available: true}, _start_datetime, _end_datetime), do: :ok

  defp check_schedule_availability(item, start_datetime, end_datetime) do
    start_date = Timex.to_date(start_datetime)
    end_date = Timex.to_date(end_datetime)

    date_range =
      Timex.Interval.new(from: start_date, until: end_date)
      |> Timex.Interval.with_step(days: 1)
      |> Enum.map(&Timex.to_date/1)

    if all_dates_in_schedule?(date_range, item.id, start_datetime, end_datetime) do
      :ok
    else
      {:error, :outside_schedule}
    end
  end

  @spec all_dates_in_schedule?(list(Date.t()), String.t(), DateTime.t(), DateTime.t()) :: boolean()
  defp all_dates_in_schedule?(date_range, item_id, start_datetime, end_datetime) do
    Enum.all?(date_range, fn date ->
      day_of_week = if RivaAsh.DateTimeHelpers.weekend?(date), do: 0, else: Timex.weekday(date)
      check_day_schedule(item_id, day_of_week, start_datetime, end_datetime, date)
    end)
  end

  @spec check_day_schedule(String.t(), integer(), DateTime.t(), DateTime.t(), Date.t()) :: boolean()
  defp check_day_schedule(item_id, day_of_week, start_datetime, end_datetime, date) do
    case ItemSchedule.by_item(item_id) do
      {:ok, schedules} ->
        day_schedules =
          Enum.filter(schedules, &(&1.day_of_week == day_of_week && &1.is_available))

        if Enum.empty?(day_schedules) do
          # No schedule = not available
          false
        else
          # Check if the requested time falls within any available schedule
          Enum.any?(day_schedules, fn schedule ->
            time_in_schedule?(start_datetime, end_datetime, schedule, date)
          end)
        end

      {:error, _schedule_not_found} ->
        false
    end
  end

  @spec time_in_schedule?(DateTime.t(), DateTime.t(), map(), Date.t()) :: boolean()
  defp time_in_schedule?(start_datetime, end_datetime, schedule, date) do
    start_time = DateTime.to_time(start_datetime)
    end_time = DateTime.to_time(end_datetime)

    # Handle same-day reservations
    # Determine the time range to check for the current date based on the overall reservation duration.
    # For dates within the multi-day reservation, but not the start or end dates, it must be available all day.
    check_start_time =
      if Timex.to_date(start_datetime) == date, do: start_time, else: Time.new!(0, 0, 0)

    check_end_time =
      if Timex.to_date(end_datetime) == date, do: end_time, else: Time.new!(23, 59, 59)

    # Convert schedule times to current date's DateTime for comparison
    schedule_start_dt = DateTime.new!(date, schedule.start_time)
    schedule_end_dt = DateTime.new!(date, schedule.end_time)
    check_start_dt = DateTime.new!(date, check_start_time)
    check_end_dt = DateTime.new!(date, check_end_time)

    # Check if the requested segment overlaps with or is fully contained within the schedule
    # A segment is available if its start is before the schedule's end AND its end is after the schedule's start.
    Timex.compare(check_start_dt, schedule_end_dt) == -1 &&
      Timex.compare(check_end_dt, schedule_start_dt) == 1
  end

  @spec check_exceptions(String.t(), DateTime.t(), DateTime.t()) :: :ok | {:error, atom()}
  defp check_exceptions(item_id, start_datetime, end_datetime) do
    start_date = Timex.to_date(start_datetime)
    end_date = Timex.to_date(end_datetime)

    case AvailabilityException.by_item(item_id) do
      {:ok, exceptions} ->
        blocking_exceptions = find_blocking_exceptions(exceptions, start_date, end_date, start_datetime, end_datetime)

        if Enum.empty?(blocking_exceptions) do
          :ok
        else
          {:error, :blocked_by_exception}
        end

      # No exceptions = no blocking
      {:error, _exceptions_not_found} ->
        :ok
    end
  end

  @spec find_blocking_exceptions(list(map()), Date.t(), Date.t(), DateTime.t(), DateTime.t()) :: list(map())
  defp find_blocking_exceptions(exceptions, start_date, end_date, start_datetime, end_datetime) do
    Enum.filter(exceptions, fn exception ->
      # Check if exception date is within reservation date range
      date_after_start = Timex.compare(exception.date, start_date) in [1, 0]
      date_before_end = Timex.compare(exception.date, end_date) in [-1, 0]

      # Exception must be in date range, marked as unavailable, and overlap the time
      date_after_start and date_before_end and
        not exception.is_available and
        exception_overlaps?(exception, start_datetime, end_datetime)
    end)
  end

  @spec exception_overlaps?(map(), DateTime.t(), DateTime.t()) :: boolean()
  defp exception_overlaps?(exception, start_datetime, end_datetime) do
    # If exception has no specific times, it blocks the whole day
    if is_nil(exception.start_time) || is_nil(exception.end_time) do
      true
    else
      # Check time overlap on the exception date
      exception_start = DateTime.new!(exception.date, exception.start_time)
      exception_end = DateTime.new!(exception.date, exception.end_time)

      # Check for overlap: start < exception_end && end > exception_start
      Timex.compare(start_datetime, exception_end) == -1 &&
        Timex.compare(end_datetime, exception_start) == 1
    end
  end

  @spec check_reservation_conflicts(String.t(), DateTime.t(), DateTime.t(), non_neg_integer()) ::
          {:ok, non_neg_integer()} | {:error, atom()}
  defp check_reservation_conflicts(item_id, start_datetime, end_datetime, capacity) do
    case Reservation.by_item(item_id) do
      {:ok, reservations} ->
        conflicting_reservations = find_conflicting_reservations(reservations, start_datetime, end_datetime)
        available_capacity = calculate_available_capacity(conflicting_reservations, capacity)

        if available_capacity > 0 do
          {:ok, available_capacity}
        else
          {:error, :fully_booked}
        end

      # No reservations = full capacity available
      {:error, _reservations_not_found} ->
        {:ok, capacity}
    end
  end

  @spec find_conflicting_reservations(list(map()), DateTime.t(), DateTime.t()) :: list(map())
  defp find_conflicting_reservations(reservations, start_datetime, end_datetime) do
    Enum.filter(reservations, fn reservation ->
      reservation.status in [:confirmed, :pending] &&
        reservations_overlap?(reservation, start_datetime, end_datetime)
    end)
  end

  @spec calculate_available_capacity(list(map()), non_neg_integer()) :: non_neg_integer()
  defp calculate_available_capacity(conflicting_reservations, capacity) do
    used_capacity = length(conflicting_reservations)
    capacity - used_capacity
  end

  @spec reservations_overlap?(map(), DateTime.t(), DateTime.t()) :: boolean()
  defp reservations_overlap?(reservation, start_datetime, end_datetime) do
    # Check for overlap: start < reservation_end && end > reservation_start
    Timex.compare(start_datetime, reservation.reserved_until) == -1 &&
      Timex.compare(end_datetime, reservation.reserved_from) == 1
  end

  # Helper functions for slot generation

  # The convert_to_sunday_start/1 function has been removed as its functionality
  # is now handled by direct use of RivaAsh.DateTimeHelpers.weekend?/1
  # to determine if a day should be treated as 0 (weekend) or its Timex weekday value.

  @spec create_24_hour_slots(Date.t(), non_neg_integer()) :: list({Time.t(), Time.t()})
  defp create_24_hour_slots(_date, slot_duration_minutes)
       when is_integer(slot_duration_minutes) and slot_duration_minutes > 0 do
    # Create slots from 00:00 to 23:59
    0..23
    |> Enum.flat_map(fn hour ->
      0..(60 - slot_duration_minutes)
      |> Enum.take_every(slot_duration_minutes)
      |> Enum.map(fn minute ->
        start_time = Time.new!(hour, minute, 0)
        end_time = Timex.shift(start_time, minutes: slot_duration_minutes)
        {start_time, end_time}
      end)
    end)
  end

  @spec get_scheduled_slots(String.t(), integer(), Date.t(), non_neg_integer()) ::
          {:ok, list({Time.t(), Time.t()})} | {:error, atom()}
  defp get_scheduled_slots(item_id, day_of_week, _date, slot_duration_minutes)
       when is_binary(item_id) and is_integer(slot_duration_minutes) and slot_duration_minutes > 0 do
    case ItemSchedule.by_item(item_id) do
      {:ok, schedules} ->
        slots =
          schedules
          |> Enum.filter(&(&1.day_of_week == day_of_week && &1.is_available))
          |> Enum.flat_map(fn schedule ->
            generate_slots_for_schedule(schedule, slot_duration_minutes)
          end)

        {:ok, slots}

      {:error, _schedules_not_found} ->
        {:ok, []}
    end
  end

  @spec generate_slots_for_schedule(map(), non_neg_integer()) :: list({Time.t(), Time.t()})
  defp generate_slots_for_schedule(schedule, slot_duration_minutes)
       when is_integer(slot_duration_minutes) and slot_duration_minutes > 0 do
    # Create a sequence of start times at regular intervals
    # from schedule.start_time to just before schedule.end_time

    # First convert the start and end times to their hour and minute components
    start_hour = schedule.start_time.hour
    start_min = schedule.start_time.minute
    end_hour = schedule.end_time.hour
    end_min = schedule.end_time.minute

    # Calculate total minutes for start and end
    start_minutes = start_hour * 60 + start_min
    end_minutes = end_hour * 60 + end_min

    # Generate slot sequences
    slots = []
    slots = do_generate_slots(start_minutes, end_minutes, slot_duration_minutes, slots)

    # Convert minute offsets back to Time structs
    Enum.map(slots, fn {slot_start, slot_end} ->
      start_time = Time.new!(div(slot_start, 60), rem(slot_start, 60), 0)
      end_time = Time.new!(div(slot_end, 60), rem(slot_end, 60), 0)
      {start_time, end_time}
    end)
  end

  # Helper function to recursively generate slots
  @spec do_generate_slots(non_neg_integer(), non_neg_integer(), non_neg_integer(), list()) ::
          list({non_neg_integer(), non_neg_integer()})
  defp do_generate_slots(current, ending, duration, acc) when is_integer(duration) and duration > 0 do
    next = current + duration

    if next <= ending do
      do_generate_slots(next, ending, duration, [{current, next} | acc])
    else
      # Return in chronological order
      Enum.reverse(acc)
    end
  end

  @spec filter_exceptions(list({Time.t(), Time.t()}), String.t(), Date.t()) :: list({Time.t(), Time.t()})
  defp filter_exceptions(slots, item_id, date) do
    case AvailabilityException.by_item(item_id) do
      {:ok, exceptions} ->
        day_exceptions = Enum.filter(exceptions, &(&1.date == date && !&1.is_available))

        Enum.reject(slots, fn {start_time, end_time} ->
          Enum.any?(day_exceptions, fn exception ->
            slot_blocked_by_exception?({start_time, end_time}, exception)
          end)
        end)

      {:error, _exceptions_not_found} ->
        slots
    end
  end

  @spec slot_blocked_by_exception?({Time.t(), Time.t()}, map()) :: boolean()
  defp slot_blocked_by_exception?({start_time, end_time}, exception) do
    if is_nil(exception.start_time) || is_nil(exception.end_time) do
      # All-day exception blocks everything
      true
    else
      # Check time overlap
      Timex.compare(start_time, exception.end_time) == -1 &&
        Timex.compare(end_time, exception.start_time) == 1
    end
  end

  @spec filter_reservations(list({Time.t(), Time.t()}), String.t(), Date.t(), non_neg_integer()) ::
          list({Time.t(), Time.t()})
  defp filter_reservations(slots, item_id, date, capacity) do
    case Reservation.by_item(item_id) do
      {:ok, reservations} ->
        day_reservations = get_day_reservations(reservations, date)
        filter_slots_by_capacity(slots, day_reservations, date, capacity)

      {:error, _reservations_not_found} ->
        slots
    end
  end

  @spec get_day_reservations(list(map()), Date.t()) :: list(map())
  defp get_day_reservations(reservations, date) do
    Enum.filter(reservations, fn reservation ->
      reservation.status in [:confirmed, :pending] &&
        (Timex.to_date(reservation.reserved_from) == date ||
           Timex.to_date(reservation.reserved_until) == date)
    end)
  end

  @spec filter_slots_by_capacity(list({Time.t(), Time.t()}), list(map()), Date.t(), non_neg_integer()) ::
          list({Time.t(), Time.t()})
  defp filter_slots_by_capacity(slots, day_reservations, date, capacity) do
    Enum.filter(slots, fn {start_time, end_time} ->
      slot_datetime_start = DateTime.new!(date, start_time)
      slot_datetime_end = DateTime.new!(date, end_time)

      conflicting_count =
        Enum.count(day_reservations, fn reservation ->
          reservations_overlap?(reservation, slot_datetime_start, slot_datetime_end)
        end)

      conflicting_count < capacity
    end)
  end
end
