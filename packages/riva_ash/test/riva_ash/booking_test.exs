defmodule RivaAsh.BookingTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Booking

  describe "create_booking/1" do
    test "creates booking with valid params" do
      params = %{
        business_id: "business-123",
        user_id: "user-456",
        service_id: "service-789",
        start_time: ~N[2024-01-15 09:00:00],
        end_time: ~N[2024-01-15 10:00:00],
        notes: "Test booking"
      }

      assert {:ok, booking} = Booking.create_booking(params)
      assert booking.business_id == "business-123"
      assert booking.status == :pending
    end

    test "returns error with invalid params" do
      params = %{
        business_id: nil,
        user_id: "user-456",
        start_time: ~N[2024-01-15 09:00:00],
        end_time: ~N[2024-01-15 08:00:00] # Invalid time range
      }

      assert {:error, _changeset} = Booking.create_booking(params)
    end
  end

  describe "confirm_booking/1" do
    test "confirms pending booking" do
      booking_id = "booking-123"
      assert {:ok, booking} = Booking.confirm_booking(booking_id)
      assert booking.status == :confirmed
    end

    test "returns error for non-existent booking" do
      booking_id = "nonexistent"
      assert {:error, :not_found} = Booking.confirm_booking(booking_id)
    end
  end

  describe "cancel_booking/2" do
    test "cancels booking with reason" do
      booking_id = "booking-123"
      reason = "User requested cancellation"

      assert {:ok, booking} = Booking.cancel_booking(booking_id, reason)
      assert booking.status == :cancelled
      assert booking.cancellation_reason == reason
    end

    test "handles cancellation of non-existent booking" do
      booking_id = "nonexistent"
      reason = "Test reason"

      assert {:error, :not_found} = Booking.cancel_booking(booking_id, reason)
    end
  end

  describe "reschedule_booking/2" do
    test "reschedules booking to new time" do
      booking_id = "booking-123"
      new_params = %{
        start_time: ~N[2024-01-15 14:00:00],
        end_time: ~N[2024-01-15 15:00:00]
      }

      assert {:ok, booking} = Booking.reschedule_booking(booking_id, new_params)
      assert booking.start_time == ~N[2024-01-15 14:00:00]
    end
  end

  describe "get_booking/1" do
    test "returns booking by id" do
      booking_id = "booking-123"

      assert {:ok, booking} = Booking.get_booking(booking_id)
      assert booking.id == booking_id
    end

    test "returns error for non-existent booking" do
      booking_id = "nonexistent"

      assert {:error, :not_found} = Booking.get_booking(booking_id)
    end
  end

  describe "list_bookings/2" do
    test "returns bookings for user" do
      user_id = "user-123"
      filters = %{status: :confirmed}

      assert {:ok, bookings} = Booking.list_bookings(user_id, filters)
      assert is_list(bookings)
    end

    test "returns empty list for user with no bookings" do
      user_id = "user-no-bookings"
      filters = %{}

      assert {:ok, []} = Booking.list_bookings(user_id, filters)
    end
  end

  describe "list_business_bookings/2" do
    test "returns bookings for business" do
      business_id = "business-123"
      date_range = %{
        start_date: ~D[2024-01-01],
        end_date: ~D[2024-01-31]
      }

      assert {:ok, bookings} = Booking.list_business_bookings(business_id, date_range)
      assert is_list(bookings)
    end
  end

  describe "check_booking_conflict/3" do
    test "detects booking conflicts" do
      business_id = "business-123"
      start_time = ~N[2024-01-15 09:00:00]
      end_time = ~N[2024-01-15 10:00:00]

      assert {:ok, boolean} = Booking.check_booking_conflict(business_id, start_time, end_time)
      assert is_boolean(boolean)
    end
  end

  describe "validate_booking_time/2" do
    test "validates booking time against business hours" do
      business_id = "business-123"
      booking_time = %{
        start_time: ~N[2024-01-15 09:00:00],
        end_time: ~N[2024-01-15 10:00:00]
      }

      assert :ok = Booking.validate_booking_time(business_id, booking_time)
    end

    test "returns error for booking outside business hours" do
      business_id = "business-123"
      booking_time = %{
        start_time: ~N[2024-01-15 05:00:00],
        end_time: ~N[2024-01-15 06:00:00]
      }

      assert {:error, _} = Booking.validate_booking_time(business_id, booking_time)
    end
  end

  describe "calculate_booking_price/2" do
    test "calculates price for booking" do
      service_id = "service-123"
      duration = 60

      assert {:ok, price} = Booking.calculate_booking_price(service_id, duration)
      assert is_number(price)
      assert price >= 0
    end
  end

  describe "send_booking_confirmation/1" do
    test "sends booking confirmation" do
      booking_id = "booking-123"

      assert :ok = Booking.send_booking_confirmation(booking_id)
    end
  end

  describe "get_booking_status/1" do
    test "returns booking status" do
      booking_id = "booking-123"

      assert {:ok, status} = Booking.get_booking_status(booking_id)
      assert status in [:pending, :confirmed, :cancelled, :completed]
    end
  end
end
