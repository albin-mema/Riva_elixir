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
  @spec weekend?(Date.t() | DateTime.t() | NaiveDateTime.t()) :: boolean()
  def weekend?(date) when is_struct(date, Date) do
    day_of_week = Timex.weekday(date)
    # Saturday = 6, Sunday = 7
    day_of_week in [6, 7]
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
  @spec weekday?(Date.t() | DateTime.t() | NaiveDateTime.t()) :: boolean()
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
  @spec day_type(Date.t() | DateTime.t() | NaiveDateTime.t()) :: :weekday | :weekend
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
  @spec day_type_string(Date.t() | DateTime.t() | NaiveDateTime.t()) :: String.t()
  def day_type_string(date_or_datetime) do
    case day_type(date_or_datetime) do
      :weekday -> "Weekday"
      :weekend -> "Weekend"
    end
  end

  @doc """
  Gets the day of week as an integer (1 = Monday, 7 = Sunday).
  """
  @spec day_of_week(Date.t() | DateTime.t() | NaiveDateTime.t()) :: integer()
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
  @spec day_name(Date.t() | DateTime.t() | NaiveDateTime.t()) :: String.t()
  def day_name(date_or_datetime) do
    day_of_week(date_or_datetime)
    |> day_name_from_integer()
  end

  @spec day_name_from_integer(integer()) :: String.t()
  defp day_name_from_integer(1), do: "Monday"
  defp day_name_from_integer(2), do: "Tuesday"
  defp day_name_from_integer(3), do: "Wednesday"
  defp day_name_from_integer(4), do: "Thursday"
  defp day_name_from_integer(5), do: "Friday"
  defp day_name_from_integer(6), do: "Saturday"
  defp day_name_from_integer(7), do: "Sunday"

  @doc """
  Gets the abbreviated day name.

  ## Examples

      iex> RivaAsh.DateTimeHelpers.day_abbrev(~D[2024-01-08])  # Monday
      "Mon"
  """
  @spec day_abbrev(Date.t() | DateTime.t() | NaiveDateTime.t()) :: String.t()
  def day_abbrev(date_or_datetime) do
    day_of_week(date_or_datetime)
    |> day_abbrev_from_integer()
  end

  @spec day_abbrev_from_integer(integer()) :: String.t()
  defp day_abbrev_from_integer(1), do: "Mon"
  defp day_abbrev_from_integer(2), do: "Tue"
  defp day_abbrev_from_integer(3), do: "Wed"
  defp day_abbrev_from_integer(4), do: "Thu"
  defp day_abbrev_from_integer(5), do: "Fri"
  defp day_abbrev_from_integer(6), do: "Sat"
  defp day_abbrev_from_integer(7), do: "Sun"

  @doc """
  Calculates the number of weekdays and weekend days in a date range.

  ## Examples

      iex> RivaAsh.DateTimeHelpers.count_day_types(~D[2024-01-08], ~D[2024-01-14])
      %{weekdays: 5, weekends: 2}
  """
  @spec count_day_types(Date.t(), Date.t()) :: %{weekdays: non_neg_integer(), weekends: non_neg_integer()}
  def count_day_types(start_date, end_date) when is_struct(start_date, Date) and is_struct(end_date, Date) do
    Timex.Interval.new(from: start_date, until: end_date)
    |> Enum.to_list()
    |> count_day_types_in_list()
  end

  @spec count_day_types_in_list(list(Date.t())) :: %{weekdays: non_neg_integer(), weekends: non_neg_integer()}
  defp count_day_types_in_list(dates) do
    Enum.reduce(dates, %{weekdays: 0, weekends: 0}, fn date, acc ->
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
  @spec filter_weekdays(list(Date.t())) :: list(Date.t())
  def filter_weekdays(dates) when is_list(dates) do
    Enum.filter(dates, &weekday?/1)
  end

  @doc """
  Filters a list of dates to only include weekends.
  """
  @spec filter_weekends(list(Date.t())) :: list(Date.t())
  def filter_weekends(dates) when is_list(dates) do
    Enum.filter(dates, &weekend?/1)
  end

  @doc """
  Gets the next weekday from a given date.
  If the given date is already a weekday, returns the next weekday.
  """
  @spec next_weekday(Date.t()) :: Date.t()
  def next_weekday(date) when is_struct(date, Date) do
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
  @spec previous_weekday(Date.t()) :: Date.t()
  def previous_weekday(date) when is_struct(date, Date) do
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
  @spec spans_weekday_and_weekend?(DateTime.t(), DateTime.t()) :: boolean()
  def spans_weekday_and_weekend?(start_datetime, end_datetime)
      when is_struct(start_datetime, DateTime) and is_struct(end_datetime, DateTime) do
    start_date = DateTime.to_date(start_datetime)
    end_date = DateTime.to_date(end_datetime)

    date_range = Timex.Interval.new(from: start_date, until: end_date) |> Enum.to_list()

    has_weekday = Enum.any?(date_range, &weekday?/1)
    has_weekend = Enum.any?(date_range, &weekend?/1)

    has_weekday and has_weekend
  end

  @doc """
  Splits a datetime range into weekday and weekend portions.
  Returns a map with :weekday_ranges and :weekend_ranges lists.
  """
  @spec split_datetime_range_by_day_type(DateTime.t(), DateTime.t()) ::
          %{weekday_ranges: list({DateTime.t(), DateTime.t()}), weekend_ranges: list({DateTime.t(), DateTime.t()})}
  def split_datetime_range_by_day_type(start_datetime, end_datetime)
      when is_struct(start_datetime, DateTime) and is_struct(end_datetime, DateTime) do
    start_date = DateTime.to_date(start_datetime)
    end_date = DateTime.to_date(end_datetime)

    start_time = DateTime.to_time(start_datetime)
    end_time = DateTime.to_time(end_datetime)

    date_range = Timex.Interval.new(from: start_date, until: end_date) |> Enum.to_list()

    {weekday_ranges, weekend_ranges} =
      date_range
      |> Enum.reduce({[], []}, fn date, {weekday_acc, weekend_acc} ->
        range = build_datetime_range(date, start_date, end_date, start_time, end_time)

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

  @spec build_datetime_range(Date.t(), Date.t(), Date.t(), Time.t(), Time.t()) :: {DateTime.t(), DateTime.t()}
  defp build_datetime_range(date, start_date, end_date, start_time, end_time) do
    day_start_time = if Timex.compare(date, start_date) == 0, do: start_time, else: ~T[00:00:00]
    day_end_time = if Timex.compare(date, end_date) == 0, do: end_time, else: ~T[23:59:59]

    day_start_datetime = DateTime.new!(date, day_start_time, "Etc/UTC")
    day_end_datetime = DateTime.new!(date, day_end_time, "Etc/UTC")

    {day_start_datetime, day_end_datetime}
  end
end
