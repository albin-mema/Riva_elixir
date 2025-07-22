defmodule RivaAsh.DateTimeHelpersTest do
  use ExUnit.Case, async: true
  alias RivaAsh.DateTimeHelpers

  @moduletag :unit
  @moduletag :pure
  @moduletag :fast

  describe "weekend?/1" do
    test "returns true for Saturday" do
      date = ~D[2024-01-06]
      assert DateTimeHelpers.weekend?(date) == true
    end

    test "returns true for Sunday" do
      date = ~D[2024-01-07]
      assert DateTimeHelpers.weekend?(date) == true
    end

    test "returns false for weekdays" do
      date = ~D[2024-01-08]
      assert DateTimeHelpers.weekend?(date) == false
    end
  end

  describe "day_type_string/1" do
    test "returns 'weekday' for Monday" do
      date = ~D[2024-01-08]
      assert DateTimeHelpers.day_type_string(date) == "weekday"
    end

    test "returns 'weekend' for Saturday" do
      date = ~D[2024-01-06]
      assert DateTimeHelpers.day_type_string(date) == "weekend"
    end
  end

  describe "day_name/1" do
    test "returns correct day name for date" do
      date = ~D[2024-01-08]
      assert DateTimeHelpers.day_name(date) == "Monday"
    end
  end

  describe "day_abbrev/1" do
    test "returns correct day abbreviation" do
      date = ~D[2024-01-08]
      assert DateTimeHelpers.day_abbrev(date) == "Mon"
    end
  end

  describe "count_day_types/2" do
    test "counts weekdays and weekends correctly" do
      start_date = ~D[2024-01-01]
      end_date = ~D[2024-01-07]
      result = DateTimeHelpers.count_day_types(start_date, end_date)

      assert result.weekdays == 5
      assert result.weekends == 2
    end

    test "handles single day range" do
      date = ~D[2024-01-06]
      result = DateTimeHelpers.count_day_types(date, date)

      assert result.weekdays == 0
      assert result.weekends == 1
    end
  end

  describe "next_weekday/1" do
    test "returns same day for weekday" do
      date = ~D[2024-01-08]
      assert DateTimeHelpers.next_weekday(date) == date
    end

    test "returns Monday for Sunday" do
      sunday = ~D[2024-01-07]
      monday = ~D[2024-01-08]
      assert DateTimeHelpers.next_weekday(sunday) == monday
    end

    test "returns Monday for Saturday" do
      saturday = ~D[2024-01-06]
      monday = ~D[2024-01-08]
      assert DateTimeHelpers.next_weekday(saturday) == monday
    end
  end

  describe "previous_weekday/1" do
    test "returns same day for weekday" do
      date = ~D[2024-01-08]
      assert DateTimeHelpers.previous_weekday(date) == date
    end

    test "returns Friday for Sunday" do
      sunday = ~D[2024-01-07]
      friday = ~D[2024-01-05]
      assert DateTimeHelpers.previous_weekday(sunday) == friday
    end
  end

  describe "spans_weekday_and_weekend?/2" do
    test "returns true when range spans both types" do
      start_dt = ~U[2024-01-05 23:00:00Z]
      end_dt = ~U[2024-01-06 01:00:00Z]
      assert DateTimeHelpers.spans_weekday_and_weekend?(start_dt, end_dt) == true
    end

    test "returns false when range is only weekdays" do
      start_dt = ~U[2024-01-08 09:00:00Z]
      end_dt = ~U[2024-01-09 17:00:00Z]
      assert DateTimeHelpers.spans_weekday_and_weekend?(start_dt, end_dt) == false
    end
  end

  describe "split_datetime_range_by_day_type/2" do
    test "splits range by day type correctly" do
      start_dt = ~U[2024-01-05 20:00:00Z]
      end_dt = ~U[2024-01-07 08:00:00Z]

      result = DateTimeHelpers.split_datetime_range_by_day_type(start_dt, end_dt)

      assert length(result) >= 2
      assert Enum.all?(result, fn {type, _start, _end} -> type in ["weekday", "weekend"] end)
    end
  end
end
