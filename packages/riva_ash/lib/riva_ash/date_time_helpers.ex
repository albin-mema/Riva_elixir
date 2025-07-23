defmodule RivaAsh.DateTimeHelpers do
  @moduledoc """
  Utility functions for date and time operations, including weekday/weekend logic.

  This module provides consistent date/time handling across the application,
  with special focus on business day vs weekend differentiation.
  """

  @doc """
  Determines if a given date falls on a weekend (Saturday or Sunday).

  ## Examples

      iex> RivaAsh.DateTimeHelpers.weekend?(~D[2024-01-06])  # Saturday
      true

      iex> RivaAsh.DateTimeHelpers.weekend?(~D[2024-01-08])  # Monday
      false
  """
  def weekend?(date) when is_struct(date, Date) do
    day_of_week = Timex.weekday(date)
    day_of_week in [6, 7]  # Saturday = 6, Sunday = 7
  end

  def weekend?(datetime) when is_struct(datetime, DateTime) do
    datetime |> DateTime.to_date() |> weekend?()
  end

  def weekend?(naive_datetime) when is_struct(naive_datetime, NaiveDateTime) do
    naive_datetime |> NaiveDateTime.to_date() |> weekend?()
  end

  @doc """
  Determines if a given date falls on a weekday (Monday through Friday).

  ## Examples

      iex> RivaAsh.DateTimeHelpers.weekday?(~D[2024-01-08])  # Monday
      true

      iex> RivaAsh.DateTimeHelpers.weekday?(~D[2024-01-06])  # Saturday
      false
  """
  def weekday?(date_or_datetime) do
    not weekend?(date_or_datetime)
  end

  @doc """
  Returns the day type as an atom: :weekday or :weekend.

  ## Examples

      iex> RivaAsh.DateTimeHelpers.day_type(~D[2024-01-08])  # Monday
      :weekday

      iex> RivaAsh.DateTimeHelpers.day_type(~D[2024-01-06])  # Saturday
      :weekend
  """
  def day_type(date_or_datetime) do
    if weekend?(date_or_datetime), do: :weekend, else: :weekday
  end

  @doc """
  Returns a human-readable string for the day type.

  ## Examples

      iex> RivaAsh.DateTimeHelpers.day_type_string(~D[2024-01-08])
      "Weekday"

      iex> RivaAsh.DateTimeHelpers.day_type_string(~D[2024-01-06])
      "Weekend"
  """
  def day_type_string(date_or_datetime) do
    case day_type(date_or_datetime) do
      :weekday -> "Weekday"
      :weekend -> "Weekend"
    end
  end

  @doc """
  Gets the day of week as an integer (1 = Monday, 7 = Sunday).
  """
  def day_of_week(date) when is_struct(date, Date) do
    Timex.weekday(date)
  end

  def day_of_week(datetime) when is_struct(datetime, DateTime) do
    Timex.weekday(datetime)
  end

  def day_of_week(naive_datetime) when is_struct(naive_datetime, NaiveDateTime) do
    Timex.weekday(naive_datetime)
  end

  @doc """
  Gets the day of week as a human-readable string.

  ## Examples

      iex> RivaAsh.DateTimeHelpers.day_name(~D[2024-01-08])  # Monday
      "Monday"
  """
  def day_name(date_or_datetime) do
    case day_of_week(date_or_datetime) do
      1 -> "Monday"
      2 -> "Tuesday"
      3 -> "Wednesday"
      4 -> "Thursday"
      5 -> "Friday"
      6 -> "Saturday"
      7 -> "Sunday"
    end
  end

  @doc """
  Gets the abbreviated day name.

  ## Examples

      iex> RivaAsh.DateTimeHelpers.day_abbrev(~D[2024-01-08])  # Monday
      "Mon"
  """
  def day_abbrev(date_or_datetime) do
    case day_of_week(date_or_datetime) do
      1 -> "Mon"
      2 -> "Tue"
      3 -> "Wed"
      4 -> "Thu"
      5 -> "Fri"
      6 -> "Sat"
      7 -> "Sun"
    end
  end

  @doc """
  Calculates the number of weekdays and weekend days in a date range.

  ## Examples

      iex> RivaAsh.DateTimeHelpers.count_day_types(~D[2024-01-08], ~D[2024-01-14])
      %{weekdays: 5, weekends: 2}
  """
  def count_day_types(start_date, end_date) do
    start_date
    |> Timex.Interval.new(from: start_date, until: end_date)
    |> Timex.Interval.to_list()
    |> Enum.reduce(%{weekdays: 0, weekends: 0}, fn date, acc ->
      if weekend?(date) do
        %{acc | weekends: acc.weekends + 1}
      else
        %{acc | weekdays: acc.weekdays + 1}
      end
    end)
  end

  @doc """
  Filters a list of dates to only include weekdays.
  """
  def filter_weekdays(dates) do
    Enum.filter(dates, &weekday?/1)
  end

  @doc """
  Filters a list of dates to only include weekends.
  """
  def filter_weekends(dates) do
    Enum.filter(dates, &weekend?/1)
  end

  @doc """
  Gets the next weekday from a given date.
  If the given date is already a weekday, returns the next weekday.
  """
  def next_weekday(date) do
    next_date = Timex.add(date, Timex.Duration.from_days(1))
    if weekday?(next_date) do
      next_date
    else
      next_weekday(next_date)
    end
  end

  @doc """
  Gets the previous weekday from a given date.
  If the given date is already a weekday, returns the previous weekday.
  """
  def previous_weekday(date) do
    prev_date = Timex.add(date, Timex.Duration.from_days(-1))
    if weekday?(prev_date) do
      prev_date
    else
      previous_weekday(prev_date)
    end
  end

  @doc """
  Checks if a datetime range spans both weekdays and weekends.
  """
  def spans_weekday_and_weekend?(start_datetime, end_datetime) do
    start_date = DateTime.to_date(start_datetime)
    end_date = DateTime.to_date(end_datetime)

    date_range = Timex.Interval.new(from: start_date, until: end_date)
    |> Timex.Interval.to_list()

    has_weekday = Enum.any?(date_range, &weekday?/1)
    has_weekend = Enum.any?(date_range, &weekend?/1)

    has_weekday and has_weekend
  end

  @doc """
  Splits a datetime range into weekday and weekend portions.
  Returns a map with :weekday_ranges and :weekend_ranges lists.
  """
  def split_datetime_range_by_day_type(start_datetime, end_datetime) do
    start_date = DateTime.to_date(start_datetime)
    end_date = DateTime.to_date(end_datetime)

    start_time = DateTime.to_time(start_datetime)
    end_time = DateTime.to_time(end_datetime)

    date_range = Timex.Interval.new(from: start_date, until: end_date) |> Timex.Interval.to_list()

    {weekday_ranges, weekend_ranges} =
      date_range
      |> Enum.reduce({[], []}, fn date, {weekday_acc, weekend_acc} ->
        day_start_time = if Timex.compare(date, start_date) == 0, do: start_time, else: ~T[00:00:00]
        day_end_time = if Timex.compare(date, end_date) == 0, do: end_time, else: ~T[23:59:59]

        day_start_datetime = DateTime.new!(date, day_start_time, "Etc/UTC")
        day_end_datetime = DateTime.new!(date, day_end_time, "Etc/UTC")

        range = {day_start_datetime, day_end_datetime}

        if weekday?(date) do
          {[range | weekday_acc], weekend_acc}
        else
          {weekday_acc, [range | weekend_acc]}
        end
      end)

    %{
      weekday_ranges: Enum.reverse(weekday_ranges),
      weekend_ranges: Enum.reverse(weekend_ranges)
    }
  end
end
