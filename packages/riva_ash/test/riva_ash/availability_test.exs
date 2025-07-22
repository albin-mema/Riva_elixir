defmodule RivaAsh.AvailabilityTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Availability

  describe "check_availability/3" do
    test "returns available slots for valid parameters" do
      business_id = "business-123"
      date = ~D[2024-01-15]
      duration = 60

      assert {:ok, slots} = Availability.check_availability(business_id, date, duration)
      assert is_list(slots)
    end

    test "returns empty list when no availability" do
      business_id = "business-no-availability"
      date = ~D[2024-01-15]
      duration = 60

      assert {:ok, []} = Availability.check_availability(business_id, date, duration)
    end
  end

  describe "get_business_hours/2" do
    test "returns business hours for date" do
      business_id = "business-123"
      date = ~D[2024-01-15]

      assert {:ok, hours} = Availability.get_business_hours(business_id, date)
      assert is_map(hours)
      assert Map.has_key?(hours, :open)
      assert Map.has_key?(hours, :close)
    end

    test "handles business with no hours set" do
      business_id = "business-no-hours"
      date = ~D[2024-01-15]

      assert {:ok, nil} = Availability.get_business_hours(business_id, date)
    end
  end

  describe "block_time_slot/3" do
    test "blocks time slot successfully" do
      business_id = "business-123"
      start_time = ~N[2024-01-15 09:00:00]
      end_time = ~N[2024-01-15 10:00:00]

      assert :ok = Availability.block_time_slot(business_id, start_time, end_time)
    end

    test "returns error for invalid time range" do
      business_id = "business-123"
      start_time = ~N[2024-01-15 10:00:00]
      end_time = ~N[2024-01-15 09:00:00]

      assert {:error, _} = Availability.block_time_slot(business_id, start_time, end_time)
    end
  end

  describe "unblock_time_slot/3" do
    test "unblocks time slot successfully" do
      business_id = "business-123"
      start_time = ~N[2024-01-15 09:00:00]
      end_time = ~N[2024-01-15 10:00:00]

      assert :ok = Availability.unblock_time_slot(business_id, start_time, end_time)
    end
  end

  describe "get_blocked_slots/2" do
    test "returns blocked slots for date range" do
      business_id = "business-123"
      start_date = ~D[2024-01-15]
      end_date = ~D[2024-01-20]

      assert {:ok, slots} = Availability.get_blocked_slots(business_id, start_date, end_date)
      assert is_list(slots)
    end
  end

  describe "set_business_hours/2" do
    test "sets business hours successfully" do
      business_id = "business-123"
      hours = %{
        monday: %{open: ~T[09:00:00], close: ~T[17:00:00]},
        tuesday: %{open: ~T[09:00:00], close: ~T[17:00:00]},
        wednesday: %{open: ~T[09:00:00], close: ~T[17:00:00]},
        thursday: %{open: ~T[09:00:00], close: ~T[17:00:00]},
        friday: %{open: ~T[09:00:00], close: ~T[17:00:00]},
        saturday: %{open: ~T[10:00:00], close: ~T[14:00:00]},
        sunday: nil
      }

      assert :ok = Availability.set_business_hours(business_id, hours)
    end

    test "returns error for invalid hours format" do
      business_id = "business-123"
      hours = %{
        monday: %{open: "invalid", close: "invalid"}
      }

      assert {:error, _} = Availability.set_business_hours(business_id, hours)
    end
  end

  describe "calculate_available_slots/4" do
    test "calculates available slots correctly" do
      business_id = "business-123"
      date = ~D[2024-01-15]
      duration = 60
      interval = 30

      assert {:ok, slots} = Availability.calculate_available_slots(business_id, date, duration, interval)
      assert is_list(slots)
    end
  end

  describe "check_holiday/2" do
    test "identifies holidays correctly" do
      business_id = "business-123"
      date = ~D[2024-12-25]

      assert {:ok, boolean} = Availability.check_holiday(business_id, date)
      assert is_boolean(boolean)
    end
  end

  describe "get_special_hours/2" do
    test "returns special hours for date" do
      business_id = "business-123"
      date = ~D[2024-12-25]

      assert {:ok, hours} = Availability.get_special_hours(business_id, date)
      assert is_map(hours) or is_nil(hours)
    end
  end
end
