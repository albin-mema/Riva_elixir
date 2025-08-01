defmodule RivaAsh.RecurringReservationsTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.RecurringReservations

  describe "create_recurring_reservation/1" do
    test "creates recurring reservation with valid params" do
      params = %{
        business_id: "business-123",
        user_id: "user-456",
        start_date: ~D[2024-01-01],
        end_date: ~D[2024-12-31],
        frequency: :weekly,
        interval: 1,
        weekdays: [1, 3, 5],
        start_time: ~T[09:00:00],
        end_time: ~T[17:00:00]
      }

      assert {:ok, reservation} = RecurringReservations.create_recurring_reservation(params)
      assert reservation.business_id == "business-123"
      assert reservation.frequency == :weekly
    end

    test "returns error with invalid params" do
      params = %{
        business_id: nil,
        user_id: "user-456",
        start_date: ~D[2024-01-01],
        end_date: ~D[2024-12-31]
      }

      assert {:error, _changeset} = RecurringReservations.create_recurring_reservation(params)
    end
  end

  describe "generate_occurrences/1" do
    test "generates weekly occurrences correctly" do
      reservation = %{
        start_date: ~D[2024-01-01],
        end_date: ~D[2024-01-31],
        frequency: :weekly,
        interval: 1,
        # Monday, Wednesday, Friday
        weekdays: [1, 3, 5],
        start_time: ~T[09:00:00],
        end_time: ~T[17:00:00]
      }

      occurrences = RecurringReservations.generate_occurrences(reservation)
      assert is_list(occurrences)
      assert length(occurrences) > 0

      # Check that all occurrences are within date range
      Enum.each(occurrences, fn occurrence ->
        assert Date.compare(occurrence.date, reservation.start_date) in [:gt, :eq]
        assert Date.compare(occurrence.date, reservation.end_date) in [:lt, :eq]
      end)
    end

    test "generates daily occurrences correctly" do
      reservation = %{
        start_date: ~D[2024-01-01],
        end_date: ~D[2024-01-07],
        frequency: :daily,
        interval: 1,
        start_time: ~T[09:00:00],
        end_time: ~T[17:00:00]
      }

      occurrences = RecurringReservations.generate_occurrences(reservation)
      assert length(occurrences) == 7
    end

    test "generates monthly occurrences correctly" do
      reservation = %{
        start_date: ~D[2024-01-01],
        end_date: ~D[2024-03-31],
        frequency: :monthly,
        interval: 1,
        day_of_month: 15,
        start_time: ~T[09:00:00],
        end_time: ~T[17:00:00]
      }

      occurrences = RecurringReservations.generate_occurrences(reservation)
      assert is_list(occurrences)
      assert length(occurrences) >= 3
    end
  end

  describe "calculate_next_occurrence/2" do
    test "calculates next weekly occurrence" do
      # Monday
      current_date = ~D[2024-01-01]

      rule = %{
        frequency: :weekly,
        interval: 1,
        # Monday, Wednesday, Friday
        weekdays: [1, 3, 5]
      }

      next_date = RecurringReservations.calculate_next_occurrence(current_date, rule)
      # Next Wednesday
      assert next_date == ~D[2024-01-03]
    end

    test "calculates next daily occurrence" do
      current_date = ~D[2024-01-01]

      rule = %{
        frequency: :daily,
        interval: 2
      }

      next_date = RecurringReservations.calculate_next_occurrence(current_date, rule)
      assert next_date == ~D[2024-01-03]
    end
  end

  describe "update_recurring_reservation/2" do
    test "updates recurring reservation" do
      reservation_id = "recurring-123"
      params = %{end_date: ~D[2025-12-31], frequency: :monthly}

      assert {:ok, reservation} =
               RecurringReservations.update_recurring_reservation(reservation_id, params)

      assert reservation.end_date == ~D[2025-12-31]
      assert reservation.frequency == :monthly
    end
  end

  describe "cancel_recurring_reservation/1" do
    test "cancels recurring reservation" do
      reservation_id = "recurring-123"
      assert :ok = RecurringReservations.cancel_recurring_reservation(reservation_id)
    end

    test "handles non-existent reservation" do
      reservation_id = "nonexistent"

      assert {:error, :not_found} =
               RecurringReservations.cancel_recurring_reservation(reservation_id)
    end
  end

  describe "get_active_reservations/1" do
    test "returns active recurring reservations for business" do
      business_id = "business-123"

      assert {:ok, reservations} = RecurringReservations.get_active_reservations(business_id)
      assert is_list(reservations)
    end

    test "returns empty list for business with no reservations" do
      business_id = "business-no-reservations"

      assert {:ok, []} = RecurringReservations.get_active_reservations(business_id)
    end
  end

  describe "validate_recurrence_rule/1" do
    test "validates correct weekly rule" do
      rule = %{
        frequency: :weekly,
        interval: 1,
        weekdays: [1, 3, 5]
      }

      assert :ok = RecurringReservations.validate_recurrence_rule(rule)
    end

    test "validates correct daily rule" do
      rule = %{
        frequency: :daily,
        interval: 1
      }

      assert :ok = RecurringReservations.validate_recurrence_rule(rule)
    end

    test "returns error for invalid rule" do
      rule = %{
        frequency: :weekly,
        interval: 0,
        weekdays: []
      }

      assert {:error, _} = RecurringReservations.validate_recurrence_rule(rule)
    end
  end

  describe "get_occurrences_in_range/3" do
    test "returns occurrences within date range" do
      reservation_id = "recurring-123"
      start_date = ~D[2024-01-01]
      end_date = ~D[2024-01-31]

      assert {:ok, occurrences} =
               RecurringReservations.get_occurrences_in_range(
                 reservation_id,
                 start_date,
                 end_date
               )

      assert is_list(occurrences)
    end
  end
end
