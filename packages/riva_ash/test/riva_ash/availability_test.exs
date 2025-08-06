defmodule RivaAsh.AvailabilityTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Availability
  alias RivaAsh.Resources.Item
  alias RivaAsh.Resources.ItemSchedule
  alias RivaAsh.Resources.AvailabilityException
  alias RivaAsh.Resources.Reservation

  describe "check_availability/3" do
    @spec test_returns_available_slots_for_valid_parameters :: :ok
    test "returns available slots for valid parameters" do
      business_id = "business-123"
      date = ~D[2024-01-15]
      duration = 60

      assert {:ok, slots} = Availability.check_availability(business_id, date, duration)
      assert is_list(slots)
    end

    @spec test_returns_empty_list_when_no_availability :: :ok
    test "returns empty list when no availability" do
      business_id = "business-no-availability"
      date = ~D[2024-01-15]
      duration = 60

      assert {:ok, []} = Availability.check_availability(business_id, date, duration)
    end
  end

  describe "weekend/weekday handling" do
    setup do
      # Create a test item with specific schedules
      item = %Item{
        id: "item-1",
        business_id: "business-1",
        is_active: true,
        capacity: 1,
        minimum_duration_minutes: 60,
        maximum_duration_minutes: 240,
        is_always_available: false
      }

      # Weekday schedule (Monday-Friday)
      weekday_schedule = %ItemSchedule{
        item_id: "item-1",
        # Monday
        day_of_week: 1,
        is_available: true,
        start_time: ~T[09:00:00],
        end_time: ~T[17:00:00]
      }

      # Weekend schedule (Saturday)
      weekend_schedule = %ItemSchedule{
        item_id: "item-1",
        # Saturday
        day_of_week: 6,
        is_available: true,
        start_time: ~T[10:00:00],
        end_time: ~T[14:00:00]
      }

      {:ok, item: item, weekday_schedule: weekday_schedule, weekend_schedule: weekend_schedule}
    end

    @spec test_item_is_available_during_weekday_business_hours :: :ok
    test "item is available during weekday business hours", %{
      item: item,
      weekday_schedule: weekday_schedule
    } do
      # Monday at 10:00-11:00 (within weekday schedule)
      # Monday
      start_datetime = ~U[2024-01-08 10:00:00Z]
      end_datetime = ~U[2024-01-08 11:00:00Z]

      # Mock the dependencies
      expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

      expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
        {:ok, [weekday_schedule]}
      end)

      expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
      expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

      assert {:ok, :available} =
               Availability.check_availability("item-1", start_datetime, end_datetime)
    end

    @spec test_item_is_not_available_during_weekday_outside_business_hours :: :ok
    test "item is not available during weekday outside business hours", %{
      item: item,
      weekday_schedule: weekday_schedule
    } do
      # Monday at 08:00-09:00 (before weekday schedule)
      # Monday
      start_datetime = ~U[2024-01-08 08:00:00Z]
      end_datetime = ~U[2024-01-08 09:00:00Z]

      # Mock the dependencies
      expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

      expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
        {:ok, [weekday_schedule]}
      end)

      expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
      expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

      assert {:error, :outside_schedule} =
               Availability.check_availability("item-1", start_datetime, end_datetime)
    end

    @spec test_item_is_available_during_weekend_business_hours :: :ok
    test "item is available during weekend business hours", %{
      item: item,
      weekend_schedule: weekend_schedule
    } do
      # Saturday at 11:00-12:00 (within weekend schedule)
      # Saturday
      start_datetime = ~U[2024-01-13 11:00:00Z]
      end_datetime = ~U[2024-01-13 12:00:00Z]

      # Mock the dependencies
      expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

      expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
        {:ok, [weekend_schedule]}
      end)

      expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
      expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

      assert {:ok, :available} =
               Availability.check_availability("item-1", start_datetime, end_datetime)
    end

    @spec test_item_is_not_available_during_weekend_outside_business_hours :: :ok
    test "item is not available during weekend outside business hours", %{
      item: item,
      weekend_schedule: weekend_schedule
    } do
      # Saturday at 09:00-10:00 (before weekend schedule)
      # Saturday
      start_datetime = ~U[2024-01-13 09:00:00Z]
      end_datetime = ~U[2024-01-13 10:00:00Z]

      # Mock the dependencies
      expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

      expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
        {:ok, [weekend_schedule]}
      end)

      expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
      expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

      assert {:error, :outside_schedule} =
               Availability.check_availability("item-1", start_datetime, end_datetime)
    end

    @spec test_weekday_and_weekend_have_different_business_hours :: :ok
    test "weekday and weekend have different business hours", %{
      item: item,
      weekday_schedule: weekday_schedule,
      weekend_schedule: weekend_schedule
    } do
      # Test that weekday and weekend schedules are handled differently
      # Tuesday
      weekday_start = ~U[2024-01-08 10:00:00Z]
      weekday_end = ~U[2024-01-08 11:00:00Z]
      # Saturday
      weekend_start = ~U[2024-01-13 11:00:00Z]
      weekend_end = ~U[2024-01-13 12:00:00Z]

      # Mock with both schedules available
      expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

      expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
        {:ok, [weekday_schedule, weekend_schedule]}
      end)

      expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
      expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

      # Both should be available during their respective hours
      assert {:ok, :available} =
               Availability.check_availability("item-1", weekday_start, weekday_end)

      assert {:ok, :available} =
               Availability.check_availability("item-1", weekend_start, weekend_end)
    end

    describe "date range calculations across week boundaries" do
      setup do
        # Create a test item with specific schedules
        item = %Item{
          id: "item-1",
          business_id: "business-1",
          is_active: true,
          capacity: 1,
          minimum_duration_minutes: 60,
          maximum_duration_minutes: 240,
          is_always_available: false
        }

        # Weekday schedule (Monday-Friday)
        weekday_schedule = %ItemSchedule{
          item_id: "item-1",
          # Monday
          day_of_week: 1,
          is_available: true,
          start_time: ~T[09:00:00],
          end_time: ~T[17:00:00]
        }

        # Weekend schedule (Saturday)
        weekend_schedule = %ItemSchedule{
          item_id: "item-1",
          # Saturday
          day_of_week: 6,
          is_available: true,
          start_time: ~T[10:00:00],
          end_time: ~T[14:00:00]
        }

        {:ok, item: item, weekday_schedule: weekday_schedule, weekend_schedule: weekend_schedule}
      end

      @spec test_multi_day_reservation_spanning_weekday_to_weekend :: :ok
      test "multi-day reservation spanning weekday to weekend", %{
        item: item,
        weekday_schedule: weekday_schedule,
        weekend_schedule: weekend_schedule
      } do
        # Friday at 16:00 to Saturday at 11:00 (spans Friday to Saturday)
        # Friday
        start_datetime = ~U[2024-01-12 16:00:00Z]
        # Saturday
        end_datetime = ~U[2024-01-13 11:00:00Z]

        # Mock the dependencies
        expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

        expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
          {:ok, [weekday_schedule, weekend_schedule]}
        end)

        expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
        expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

        # The reservation should be available as it's within Friday's hours (until 17:00)
        # and within Saturday's hours (from 10:00)
        assert {:ok, :available} =
                 Availability.check_availability("item-1", start_datetime, end_datetime)
      end

      describe "time slot availability during business hours vs non-business hours" do
        setup do
          # Create a test item with specific schedules
          item = %Item{
            id: "item-1",
            business_id: "business-1",
            is_active: true,
            capacity: 1,
            minimum_duration_minutes: 60,
            maximum_duration_minutes: 240,
            is_always_available: false
          }

          # Business hours (9am-5pm)
          business_schedule = %ItemSchedule{
            item_id: "item-1",
            # Monday
            day_of_week: 1,
            is_available: true,
            start_time: ~T[09:00:00],
            end_time: ~T[17:00:00]
          }

          # Non-business hours (outside 9am-5pm)
          non_business_schedule = %ItemSchedule{
            item_id: "item-1",
            # Monday
            day_of_week: 1,
            is_available: false,
            start_time: ~T[00:00:00],
            end_time: ~T[09:00:00]
          }

          {:ok, item: item, business_schedule: business_schedule, non_business_schedule: non_business_schedule}
        end

        @spec test_item_is_available_during_business_hours :: :ok
        test "item is available during business hours", %{
          item: item,
          business_schedule: business_schedule
        } do
          # Monday at 10:00-11:00 (within business hours)
          # Monday
          start_datetime = ~U[2024-01-08 10:00:00Z]
          end_datetime = ~U[2024-01-08 11:00:00Z]

          # Mock the dependencies
          expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

          expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
            {:ok, [business_schedule]}
          end)

          expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
          expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

          assert {:ok, :available} =
                   Availability.check_availability("item-1", start_datetime, end_datetime)
        end

        describe "edge cases for date/time logic" do
          setup do
            # Create a test item with specific schedules
            item = %Item{
              id: "item-1",
              business_id: "business-1",
              is_active: true,
              capacity: 1,
              minimum_duration_minutes: 60,
              maximum_duration_minutes: 240,
              is_always_available: false
            }

            # Business hours (9am-5pm)
            business_schedule = %ItemSchedule{
              item_id: "item-1",
              # Monday
              day_of_week: 1,
              is_available: true,
              start_time: ~T[09:00:00],
              end_time: ~T[17:00:00]
            }

            {:ok, item: item, business_schedule: business_schedule}
          end

          @spec test_midnight_transition_within_same_day :: :ok
          test "midnight transition within same day", %{
            item: item,
            business_schedule: business_schedule
          } do
            # Test a reservation that starts before midnight and ends after midnight (same day technically)
            # This is not possible in a single day, so we test a reservation that crosses midnight
            # Monday
            start_datetime = ~U[2024-01-08 23:30:00Z]
            # Tuesday
            end_datetime = ~U[2024-01-09 00:30:00Z]

            # Mock the dependencies
            expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

            expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
              {:ok, [business_schedule]}
            end)

            expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
            expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

            # The reservation should be blocked as Tuesday has no schedule
            assert {:error, :outside_schedule} =
                     Availability.check_availability("item-1", start_datetime, end_datetime)
          end

          @spec test_midnight_transition_with_available_schedule :: :ok
          test "midnight transition with available schedule", %{item: item} do
            # Create schedules for both days
            monday_schedule = %ItemSchedule{
              item_id: "item-1",
              # Monday
              day_of_week: 1,
              is_available: true,
              start_time: ~T[20:00:00],
              end_time: ~T[23:59:59]
            }

            tuesday_schedule = %ItemSchedule{
              item_id: "item-1",
              # Tuesday
              day_of_week: 2,
              is_available: true,
              start_time: ~T[00:00:00],
              end_time: ~T[02:00:00]
            }

            # Monday at 23:30 to Tuesday at 01:30 (crosses midnight)
            # Monday
            start_datetime = ~U[2024-01-08 23:30:00Z]
            # Tuesday
            end_datetime = ~U[2024-01-09 01:30:00Z]

            # Mock the dependencies
            expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

            expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
              {:ok, [monday_schedule, tuesday_schedule]}
            end)

            expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
            expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

            assert {:ok, :available} =
                     Availability.check_availability("item-1", start_datetime, end_datetime)
          end

          @spec test_month_boundary_transition :: :ok
          test "month boundary transition", %{item: item, business_schedule: business_schedule} do
            # Test a reservation that spans the end of one month to the beginning of the next
            # January 31st
            start_datetime = ~U[2024-01-31 16:00:00Z]
            # February 1st
            end_datetime = ~U[2024-02-01 10:00:00Z]

            # Mock the dependencies
            expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

            expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
              {:ok, [business_schedule]}
            end)

            expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
            expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

            # The reservation should be available if both days have schedules
            assert {:ok, :available} =
                     Availability.check_availability("item-1", start_datetime, end_datetime)
          end

          @spec test_year_boundary_transition :: :ok
          test "year boundary transition", %{item: item, business_schedule: business_schedule} do
            # Test a reservation that spans the end of one year to the beginning of the next
            # December 31st, 2024
            start_datetime = ~U[2024-12-31 16:00:00Z]
            # January 1st, 2025
            end_datetime = ~U[2025-01-01 10:00:00Z]

            # Mock the dependencies
            expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

            expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
              {:ok, [business_schedule]}
            end)

            expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
            expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

            # The reservation should be available if both days have schedules
            assert {:ok, :available} =
                     Availability.check_availability("item-1", start_datetime, end_datetime)
          end

          @spec test_leap_year_date_handling :: :ok
          test "leap year date handling", %{item: item, business_schedule: business_schedule} do
            # Test availability on February 29th of a leap year
            # February 29th, 2024 (leap year)
            start_datetime = ~U[2024-02-29 10:00:00Z]
            end_datetime = ~U[2024-02-29 11:00:00Z]

            # Mock the dependencies
            expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

            expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
              {:ok, [business_schedule]}
            end)

            expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
            expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

            assert {:ok, :available} =
                     Availability.check_availability("item-1", start_datetime, end_datetime)
          end

          @spec test_get_available_slots_at_midnight_boundary :: :ok
          test "get_available_slots at midnight boundary", %{item: item} do
            # Test getting available slots for a day that has a schedule crossing midnight
            item_with_midnight_schedule = %Item{
              id: "item-2",
              business_id: "business-1",
              is_active: true,
              capacity: 1,
              minimum_duration_minutes: 60,
              maximum_duration_minutes: 240,
              is_always_available: false
            }

            midnight_schedule = %ItemSchedule{
              item_id: "item-2",
              # Monday
              day_of_week: 1,
              is_available: true,
              start_time: ~T[22:00:00],
              # Crosses midnight
              end_time: ~T[02:00:00]
            }

            # Monday
            date = ~D[2024-01-08]
            slot_duration = 60

            # Mock the dependencies
            expect(RivaAsh.Resources.Item, :by_id, fn "item-2" ->
              {:ok, item_with_midnight_schedule}
            end)

            expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-2" ->
              {:ok, [midnight_schedule]}
            end)

            expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
            expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

            assert {:ok, slots} = Availability.get_available_slots("item-2", date, slot_duration)

            # Should return slots from 22:00-23:00 and 00:00-01:00 (but not 23:00-00:00 as it crosses the day boundary)
            assert length(slots) == 2
            assert Enum.member?(slots, {~T[22:00:00], ~T[23:00:00]})
            assert Enum.member?(slots, {~T[00:00:00], ~T[01:00:00]})
          end
        end

        @spec test_item_is_not_available_during_non_business_hours :: :ok
        test "item is not available during non-business hours", %{
          item: item,
          business_schedule: business_schedule
        } do
          # Monday at 08:00-09:00 (before business hours)
          # Monday
          start_datetime = ~U[2024-01-08 08:00:00Z]
          end_datetime = ~U[2024-01-08 09:00:00Z]

          # Mock the dependencies
          expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

          expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
            {:ok, [business_schedule]}
          end)

          expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
          expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

          assert {:error, :outside_schedule} =
                   Availability.check_availability("item-1", start_datetime, end_datetime)
        end

        @spec test_item_is_not_available_after_business_hours :: :ok
        test "item is not available after business hours", %{
          item: item,
          business_schedule: business_schedule
        } do
          # Monday at 17:00-18:00 (after business hours)
          # Monday
          start_datetime = ~U[2024-01-08 17:00:00Z]
          end_datetime = ~U[2024-01-08 18:00:00Z]

          # Mock the dependencies
          expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

          expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
            {:ok, [business_schedule]}
          end)

          expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
          expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

          assert {:error, :outside_schedule} =
                   Availability.check_availability("item-1", start_datetime, end_datetime)
        end

        @spec test_get_available_slots_returns_slots_only_during_business_hours :: :ok
        test "get_available_slots returns slots only during business hours", %{
          item: item,
          business_schedule: business_schedule
        } do
          # Test getting available slots for a date
          # Monday
          date = ~D[2024-01-08]
          slot_duration = 60

          # Mock the dependencies
          expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

          expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
            {:ok, [business_schedule]}
          end)

          expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
          expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

          assert {:ok, slots} = Availability.get_available_slots("item-1", date, slot_duration)
          # Should return slots based on business hours (9am-5pm)
          assert length(slots) == 8
          assert List.first(slots) == {~T[09:00:00], ~T[10:00:00]}
          assert List.last(slots) == {~T[16:00:00], ~T[17:00:00]}
        end

        @spec test_time_slot_availability_with_partial_business_hours :: :ok
        test "time slot availability with partial business hours", %{item: item} do
          # Create a schedule with partial business hours
          partial_schedule = %ItemSchedule{
            item_id: "item-1",
            # Monday
            day_of_week: 1,
            is_available: true,
            start_time: ~T[13:00:00],
            end_time: ~T[15:00:00]
          }

          # Monday at 14:00-15:00 (within partial business hours)
          # Monday
          start_datetime = ~U[2024-01-08 14:00:00Z]
          end_datetime = ~U[2024-01-08 15:00:00Z]

          # Mock the dependencies
          expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

          expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
            {:ok, [partial_schedule]}
          end)

          expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
          expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

          assert {:ok, :available} =
                   Availability.check_availability("item-1", start_datetime, end_datetime)
        end
      end

      @spec test_multi_day_reservation_spanning_weekend_to_weekday :: :ok
      test "multi-day reservation spanning weekend to weekday", %{
        item: item,
        weekday_schedule: weekday_schedule,
        weekend_schedule: weekend_schedule
      } do
        # Saturday at 13:00 to Monday at 10:00 (spans Saturday to Monday)
        # Saturday
        start_datetime = ~U[2024-01-13 13:00:00Z]
        # Monday
        end_datetime = ~U[2024-01-15 10:00:00Z]

        # Mock the dependencies
        expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

        expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
          {:ok, [weekday_schedule, weekend_schedule]}
        end)

        expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
        expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

        # The reservation should be available as it's within Saturday's hours (until 14:00)
        # and within Monday's hours (from 09:00)
        assert {:ok, :available} =
                 Availability.check_availability("item-1", start_datetime, end_datetime)
      end

      @spec test_multi_day_reservation_blocked_by_schedule_gap :: :ok
      test "multi-day reservation blocked by schedule gap", %{
        item: item,
        weekday_schedule: weekday_schedule
      } do
        # Friday at 16:00 to Monday at 08:00 (spans Friday to Monday, but Sunday has no schedule)
        # Friday
        start_datetime = ~U[2024-01-12 16:00:00Z]
        # Monday
        end_datetime = ~U[2024-01-15 08:00:00Z]

        # Mock the dependencies - only weekday schedule available (no weekend schedule)
        expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

        expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
          {:ok, [weekday_schedule]}
        end)

        expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
        expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

        # The reservation should be blocked because Sunday has no schedule
        assert {:error, :outside_schedule} =
                 Availability.check_availability("item-1", start_datetime, end_datetime)
      end

      @spec test_date_range_calculation_with_week_boundary_in_get_available_slots :: :ok
      test "date range calculation with week boundary in get_available_slots", %{
        item: item,
        weekday_schedule: weekday_schedule,
        weekend_schedule: weekend_schedule
      } do
        # Test getting available slots for a date that crosses week boundaries
        # Saturday
        date = ~D[2024-01-13]
        slot_duration = 60

        # Mock the dependencies
        expect(RivaAsh.Resources.Item, :by_id, fn "item-1" -> {:ok, item} end)

        expect(RivaAsh.Resources.ItemSchedule, :by_item, fn "item-1" ->
          {:ok, [weekday_schedule, weekend_schedule]}
        end)

        expect(RivaAsh.Resources.AvailabilityException, :by_item, fn _ -> {:ok, []} end)
        expect(RivaAsh.Resources.Reservation, :by_item, fn _ -> {:ok, []} end)

        assert {:ok, slots} = Availability.get_available_slots("item-1", date, slot_duration)
        # Should return slots based on Saturday's schedule (10:00-14:00)
        assert length(slots) == 4
        assert List.first(slots) == {~T[10:00:00], ~T[11:00:00]}
        assert List.last(slots) == {~T[13:00:00], ~T[14:00:00]}
      end
    end
  end

  describe "get_business_hours/2" do
    @spec test_returns_business_hours_for_date :: :ok
    test "returns business hours for date" do
      business_id = "business-123"
      date = ~D[2024-01-15]

      assert {:ok, hours} = Availability.get_business_hours(business_id, date)
      assert is_map(hours)
      assert Map.has_key?(hours, :open)
      assert Map.has_key?(hours, :close)
    end

    @spec test_handles_business_with_no_hours_set :: :ok
    test "handles business with no hours set" do
      business_id = "business-no-hours"
      date = ~D[2024-01-15]

      assert {:ok, nil} = Availability.get_business_hours(business_id, date)
    end
  end

  describe "block_time_slot/3" do
    @spec test_blocks_time_slot_successfully :: :ok
    test "blocks time slot successfully" do
      business_id = "business-123"
      start_time = ~N[2024-01-15 09:00:00]
      end_time = ~N[2024-01-15 10:00:00]

      assert :ok = Availability.block_time_slot(business_id, start_time, end_time)
    end

    @spec test_returns_error_for_invalid_time_range :: :ok
    test "returns error for invalid time range" do
      business_id = "business-123"
      start_time = ~N[2024-01-15 10:00:00]
      end_time = ~N[2024-01-15 09:00:00]

      assert {:error, _} = Availability.block_time_slot(business_id, start_time, end_time)
    end
  end

  describe "unblock_time_slot/3" do
    @spec test_unblocks_time_slot_successfully :: :ok
    test "unblocks time slot successfully" do
      business_id = "business-123"
      start_time = ~N[2024-01-15 09:00:00]
      end_time = ~N[2024-01-15 10:00:00]

      assert :ok = Availability.unblock_time_slot(business_id, start_time, end_time)
    end
  end

  describe "get_blocked_slots/2" do
    @spec test_returns_blocked_slots_for_date_range :: :ok
    test "returns blocked slots for date range" do
      business_id = "business-123"
      start_date = ~D[2024-01-15]
      end_date = ~D[2024-01-20]

      assert {:ok, slots} = Availability.get_blocked_slots(business_id, start_date, end_date)
      assert is_list(slots)
    end
  end

  describe "set_business_hours/2" do
    @spec test_sets_business_hours_successfully :: :ok
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

    @spec test_returns_error_for_invalid_hours_format :: :ok
    test "returns error for invalid hours format" do
      business_id = "business-123"

      hours = %{
        monday: %{open: "invalid", close: "invalid"}
      }

      assert {:error, _} = Availability.set_business_hours(business_id, hours)
    end
  end

  describe "calculate_available_slots/4" do
    @spec test_calculates_available_slots_correctly :: :ok
    test "calculates available slots correctly" do
      business_id = "business-123"
      date = ~D[2024-01-15]
      duration = 60
      interval = 30

      assert {:ok, slots} =
               Availability.calculate_available_slots(business_id, date, duration, interval)

      assert is_list(slots)
    end
  end

  describe "check_holiday/2" do
    @spec test_identifies_holidays_correctly :: :ok
    test "identifies holidays correctly" do
      business_id = "business-123"
      date = ~D[2024-12-25]

      assert {:ok, boolean} = Availability.check_holiday(business_id, date)
      assert is_boolean(boolean)
    end
  end

  describe "get_special_hours/2" do
    @spec test_returns_special_hours_for_date :: :ok
    test "returns special hours for date" do
      business_id = "business-123"
      date = ~D[2024-12-25]

      assert {:ok, hours} = Availability.get_special_hours(business_id, date)
      assert is_map(hours) or is_nil(hours)
    end
  end
end
