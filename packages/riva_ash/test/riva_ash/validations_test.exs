defmodule RivaAsh.ValidationsTest do
  use ExUnit.Case, async: true
  alias RivaAsh.Validations

  describe "validate_email/1" do
    test "returns :ok for valid email format" do
      assert Validations.validate_email("user@example.com") == :ok
      assert Validations.validate_email("test.email@domain.co.uk") == :ok
      assert Validations.validate_email("user+tag@example.org") == :ok
    end

    test "returns {:error, reason} for invalid email format" do
      assert {:error, _} = Validations.validate_email("invalid-email")
      assert {:error, _} = Validations.validate_email("@example.com")
      assert {:error, _} = Validations.validate_email("user@")
      assert {:error, _} = Validations.validate_email("")
    end
  end

  describe "validate_phone/1" do
    test "returns :ok for valid phone formats" do
      assert Validations.validate_phone("+1234567890") == :ok
      assert Validations.validate_phone("123-456-7890") == :ok
      assert Validations.validate_phone("(123) 456-7890") == :ok
      assert Validations.validate_phone("1234567890") == :ok
    end

    test "returns {:error, reason} for invalid phone formats" do
      assert {:error, _} = Validations.validate_phone("invalid")
      assert {:error, _} = Validations.validate_phone("123")
      assert {:error, _} = Validations.validate_phone("")
    end
  end

  describe "validate_url/1" do
    test "returns :ok for valid URLs" do
      assert Validations.validate_url("https://example.com") == :ok
      assert Validations.validate_url("http://www.example.com") == :ok
      assert Validations.validate_url("https://subdomain.example.com/path") == :ok
    end

    test "returns {:error, reason} for invalid URLs" do
      assert {:error, _} = Validations.validate_url("not-a-url")
      assert {:error, _} = Validations.validate_url("http://")
      assert {:error, _} = Validations.validate_url("")
    end
  end

  describe "validate_positive_integer/1" do
    test "returns :ok for positive integers" do
      assert Validations.validate_positive_integer(1) == :ok
      assert Validations.validate_positive_integer(100) == :ok
    end

    test "returns {:error, reason} for non-positive integers" do
      assert {:error, _} = Validations.validate_positive_integer(0)
      assert {:error, _} = Validations.validate_positive_integer(-1)
      assert {:error, _} = Validations.validate_positive_integer(-100)
    end

    test "returns {:error, reason} for non-integers" do
      assert {:error, _} = Validations.validate_positive_integer(1.5)
      assert {:error, _} = Validations.validate_positive_integer("string")
      assert {:error, _} = Validations.validate_positive_integer(nil)
    end
  end

  describe "validate_date_range/2" do
    test "returns :ok for valid date ranges" do
      start_date = ~D[2024-01-01]
      end_date = ~D[2024-01-31]
      assert Validations.validate_date_range(start_date, end_date) == :ok
    end

    test "returns {:error, reason} when start date is after end date" do
      start_date = ~D[2024-01-31]
      end_date = ~D[2024-01-01]
      assert {:error, _} = Validations.validate_date_range(start_date, end_date)
    end

    test "returns :ok for same start and end date" do
      date = ~D[2024-01-01]
      assert Validations.validate_date_range(date, date) == :ok
    end
  end

  describe "validate_time_range/2" do
    test "returns :ok for valid time ranges" do
      start_time = ~T[09:00:00]
      end_time = ~T[17:00:00]
      assert Validations.validate_time_range(start_time, end_time) == :ok
    end

    test "returns {:error, reason} when start time is after end time" do
      start_time = ~T[17:00:00]
      end_time = ~T[09:00:00]
      assert {:error, _} = Validations.validate_time_range(start_time, end_time)
    end

    test "returns :ok for same start and end time" do
      time = ~T[09:00:00]
      assert Validations.validate_time_range(time, time) == :ok
    end
  end

  describe "validate_business_hours/2" do
    test "returns :ok for valid business hours" do
      assert Validations.validate_business_hours(~T[09:00:00], ~T[17:00:00]) == :ok
    end

    test "returns {:error, reason} for invalid business hours" do
      assert {:error, _} = Validations.validate_business_hours(~T[17:00:00], ~T[09:00:00])
    end
  end

  describe "validate_duration/1" do
    test "returns :ok for positive durations" do
      assert Validations.validate_duration(3600) == :ok
      assert Validations.validate_duration(1800) == :ok
    end

    test "returns {:error, reason} for non-positive durations" do
      assert {:error, _} = Validations.validate_duration(0)
      assert {:error, _} = Validations.validate_duration(-3600)
    end
  end
end
