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

  # Only using require to avoid unused import warning
  require Ash.Query

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
  def check_availability(item_id, start_datetime, end_datetime) do
    with {:ok, item} <- get_item(item_id),
         :ok <- validate_item_active(item),
         :ok <- validate_duration(item, start_datetime, end_datetime),
         :ok <- check_schedule_availability(item, start_datetime, end_datetime),
         :ok <- check_exceptions(item_id, start_datetime, end_datetime),
         {:ok, available_capacity} <-
           check_reservation_conflicts(item_id, start_datetime, end_datetime, item.capacity) do
      if available_capacity == item.capacity do
        {:ok, :available}
      else
        {:ok, {:partial, available_capacity}}
      end
    else
      error -> error
    end
  end

  @doc """
  Get available time slots for an item on a specific date.

  Returns a list of available time ranges considering all constraints.
  """
  def get_available_slots(item_id, date, slot_duration_minutes \\ 60) do
    with {:ok, item} <- get_item(item_id),
         :ok <- validate_item_active(item) do
      day_of_week = if RivaAsh.DateTimeHelpers.weekend?(date), do: 0, else: Timex.weekday(date)

      base_slots =
        if item.is_always_available do
          # 24/7 availability - create hourly slots
          create_24_hour_slots(date, slot_duration_minutes)
        else
          case get_scheduled_slots(item_id, day_of_week, date, slot_duration_minutes) do
            {:ok, slots} -> slots
            {:error, _} -> []
          end
        end

      # Filter out exceptions and existing reservations
      available_slots =
        base_slots
        |> filter_exceptions(item_id, date)
        |> filter_reservations(item_id, date, item.capacity)

      {:ok, available_slots}
    else
      error -> error
    end
  end

  # Private functions

  defp get_item(item_id) do
    case RivaAsh.Resources.Item.by_id(item_id) do
      {:ok, item} -> {:ok, item}
      {:error, _} -> {:error, :item_not_found}
    end
  end

  defp validate_item_active(%{is_active: true}), do: :ok
  defp validate_item_active(_), do: {:error, :item_inactive}

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

  defp check_schedule_availability(%{is_always_available: true}, _, _), do: :ok

  defp check_schedule_availability(item, start_datetime, end_datetime) do
    start_date = Timex.to_date(start_datetime)
    end_date = Timex.to_date(end_datetime)

    # Check each day in the range
    Timex.Interval.new(from: start_date, until: end_date)
    |> Timex.Interval.with_step(days: 1)
    |> Enum.map(&Timex.to_date/1)
    |> Enum.all?(fn date ->
      day_of_week = if RivaAsh.DateTimeHelpers.weekend?(date), do: 0, else: Timex.weekday(date)
      check_day_schedule(item.id, day_of_week, start_datetime, end_datetime, date)
    end)
    |> case do
      true -> :ok
      false -> {:error, :outside_schedule}
    end
  end

  defp check_day_schedule(item_id, day_of_week, start_datetime, end_datetime, date) do
    case RivaAsh.Resources.ItemSchedule.by_item(item_id) do
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

      {:error, _} ->
        false
    end
  end

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

  defp check_exceptions(item_id, start_datetime, end_datetime) do
    start_date = Timex.to_date(start_datetime)
    end_date = Timex.to_date(end_datetime)

    case RivaAsh.Resources.AvailabilityException.by_item(item_id) do
      {:ok, exceptions} ->
        blocking_exceptions =
          Enum.filter(exceptions, fn exception ->
            # Check if exception date is within reservation date range
            date_after_start = Timex.compare(exception.date, start_date) in [1, 0]
            date_before_end = Timex.compare(exception.date, end_date) in [-1, 0]

            # Exception must be in date range, marked as unavailable, and overlap the time
            date_after_start and date_before_end and
              not exception.is_available and
              exception_overlaps?(exception, start_datetime, end_datetime)
          end)

        if Enum.empty?(blocking_exceptions) do
          :ok
        else
          {:error, :blocked_by_exception}
        end

      # No exceptions = no blocking
      {:error, _} ->
        :ok
    end
  end

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

  defp check_reservation_conflicts(item_id, start_datetime, end_datetime, capacity) do
    case RivaAsh.Resources.Reservation.by_item(item_id) do
      {:ok, reservations} ->
        conflicting_reservations =
          Enum.filter(reservations, fn reservation ->
            reservation.status in [:confirmed, :pending] &&
              reservations_overlap?(reservation, start_datetime, end_datetime)
          end)

        used_capacity = length(conflicting_reservations)
        available_capacity = capacity - used_capacity

        if available_capacity > 0 do
          {:ok, available_capacity}
        else
          {:error, :fully_booked}
        end

      # No reservations = full capacity available
      {:error, _} ->
        {:ok, capacity}
    end
  end

  defp reservations_overlap?(reservation, start_datetime, end_datetime) do
    # Check for overlap: start < reservation_end && end > reservation_start
    Timex.compare(start_datetime, reservation.reserved_until) == -1 &&
      Timex.compare(end_datetime, reservation.reserved_from) == 1
  end

  # Helper functions for slot generation

  # The convert_to_sunday_start/1 function has been removed as its functionality
  # is now handled by direct use of RivaAsh.DateTimeHelpers.weekend?/1
  # to determine if a day should be treated as 0 (weekend) or its Timex weekday value.

  defp create_24_hour_slots(_date, slot_duration_minutes) do
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

  defp get_scheduled_slots(item_id, day_of_week, _date, slot_duration_minutes) do
    case RivaAsh.Resources.ItemSchedule.by_item(item_id) do
      {:ok, schedules} ->
        slots =
          schedules
          |> Enum.filter(&(&1.day_of_week == day_of_week && &1.is_available))
          |> Enum.flat_map(fn schedule ->
            generate_slots_for_schedule(schedule, slot_duration_minutes)
          end)

        {:ok, slots}

      {:error, _} ->
        {:ok, []}
    end
  end

  defp generate_slots_for_schedule(schedule, slot_duration_minutes) do
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
  defp do_generate_slots(current, ending, duration, acc) do
    next = current + duration

    if next <= ending do
      do_generate_slots(next, ending, duration, [{current, next} | acc])
    else
      # Return in chronological order
      Enum.reverse(acc)
    end
  end

  defp filter_exceptions(slots, item_id, date) do
    case RivaAsh.Resources.AvailabilityException.by_item(item_id) do
      {:ok, exceptions} ->
        day_exceptions = Enum.filter(exceptions, &(&1.date == date && !&1.is_available))

        Enum.reject(slots, fn {start_time, end_time} ->
          Enum.any?(day_exceptions, fn exception ->
            slot_blocked_by_exception?({start_time, end_time}, exception)
          end)
        end)

      {:error, _} ->
        slots
    end
  end

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

  defp filter_reservations(slots, item_id, date, capacity) do
    case RivaAsh.Resources.Reservation.by_item(item_id) do
      {:ok, reservations} ->
        day_reservations =
          Enum.filter(reservations, fn reservation ->
            reservation.status in [:confirmed, :pending] &&
              (Timex.to_date(reservation.reserved_from) == date ||
                 Timex.to_date(reservation.reserved_until) == date)
          end)

        Enum.filter(slots, fn {start_time, end_time} ->
          slot_datetime_start = DateTime.new!(date, start_time)
          slot_datetime_end = DateTime.new!(date, end_time)

          conflicting_count =
            Enum.count(day_reservations, fn reservation ->
              reservations_overlap?(reservation, slot_datetime_start, slot_datetime_end)
            end)

          conflicting_count < capacity
        end)

      {:error, _} ->
        slots
    end
  end
end
