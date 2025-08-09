defmodule RivaAshWeb.BookingControllerTest do
  use ExUnit.Case, async: true
  import RivaAshWeb.BookingController, only: [
    parse_date: 1,
    parse_duration: 1,
    parse_hour: 2,
    parse_datetime: 1
  ]

  describe "parse_date/1" do
    test "parses valid ISO8601 date" do
      assert {:ok, %Date{year: 2024, month: 1, day: 1}} = parse_date("2024-01-01")
    end

    test "rejects invalid date format" do
      assert {:error, "Invalid date format. Use YYYY-MM-DD"} = parse_date("2024/01/01")
      assert {:error, "Invalid date format. Use YYYY-MM-DD"} = parse_date("20240101")
      assert {:error, "Date parameter is required"} = parse_date(nil)
    end
  end

  describe "parse_duration/1" do
    test "parses valid duration string" do
      assert 60 = parse_duration("60")
      assert 120 = parse_duration("120")
    end

    test "parses valid integer duration" do
      assert 90 = parse_duration(90)
    end

    test "defaults to 60 for invalid input" do
      assert 60 = parse_duration("invalid")
      assert 60 = parse_duration(-30)
      assert 60 = parse_duration("")
    end
  end

  describe "parse_hour/2" do
    test "parses valid hour string" do
      assert 9 = parse_hour("9", 0)
      assert 17 = parse_hour("17", 0)
    end

    test "uses default for invalid input" do
      assert 9 = parse_hour("invalid", 9)
      assert 0 = parse_hour("-1", 0)
      assert 23 = parse_hour("24", 23)
    end

    test "accepts valid integer hours" do
      assert 10 = parse_hour(10, 0)
    end
  end

  describe "parse_datetime/1" do
    test "parses valid ISO8601 datetime" do
      assert {:ok, %DateTime{}} = parse_datetime("2024-01-01T10:00:00Z")
    end

    test "rejects invalid datetime format" do
      assert {:error, "Invalid datetime format. Use ISO8601 format"} = parse_datetime("2024-01-01 10:00")
      assert {:error, "DateTime is required"} = parse_datetime(nil)
    end
  end

  describe "input validation" do
    test "requires client name" do
      assert {:error, "Client name is required"} =
        RivaAshWeb.BookingController.extract_client_info(%{})
    end

    test "requires booking information" do
      assert {:error, "Booking information is required"} =
        RivaAshWeb.BookingController.extract_booking_info(%{})
    end

    test "validates duration is positive" do
      assert 60 = parse_duration(0)
      assert 60 = parse_duration(-10)
    end

    test "validates hours are within 0-23 range" do
      assert 0 = parse_hour(0, 9)
      assert 23 = parse_hour(23, 17)
      assert 9 = parse_hour(24, 9)
      assert 9 = parse_hour(-1, 9)
    end
  end
end
