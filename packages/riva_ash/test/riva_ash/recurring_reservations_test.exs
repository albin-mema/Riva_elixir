defmodule RivaAsh.RecurringReservationsTest do
  use RivaAsh.DataCase, async: true

  alias RivaAsh.RecurringReservations
  alias RivaAsh.Resources.{Business, Employee, Client, Item, RecurringReservation, RecurringReservationInstance}

  describe "create_recurring_reservation/2" do
    setup do
      {:ok, business} = create_business()
      {:ok, employee} = create_employee(business.id, business.owner)
      {:ok, client} = create_client(business.id, business.owner)
      {:ok, item} = create_item(business.id, business.owner)

      %{
        business: business,
        employee: employee,
        client: client,
        item: item
      }
    end

    test "creates recurring reservation with all days pattern", %{employee: employee, client: client, item: item} do
      attrs = %{
        client_id: client.id,
        item_id: item.id,
        employee_id: employee.id,
        start_date: Date.utc_today(),
        start_time: ~T[09:00:00],
        end_time: ~T[10:00:00],
        consecutive_days: 5,
        pattern_type: :all_days,
        notes: "Test recurring reservation"
      }

      assert {:ok, recurring_reservation} = RecurringReservations.create_recurring_reservation(attrs, false)
      assert recurring_reservation.consecutive_days == 5
      assert recurring_reservation.pattern_type == :all_days
      assert recurring_reservation.status == :draft
    end

    test "creates recurring reservation and generates instances immediately", %{employee: employee, client: client, item: item} do
      attrs = %{
        client_id: client.id,
        item_id: item.id,
        employee_id: employee.id,
        start_date: Date.utc_today(),
        start_time: ~T[09:00:00],
        end_time: ~T[10:00:00],
        consecutive_days: 3,
        pattern_type: :all_days,
        notes: "Test recurring reservation"
      }

      assert {:ok, recurring_reservation} = RecurringReservations.create_recurring_reservation(attrs, true)
      assert recurring_reservation.status == :active

      # Check that instances were created
      assert {:ok, instances} = RecurringReservationInstance
      |> Ash.Query.for_read(:by_recurring_reservation, %{recurring_reservation_id: recurring_reservation.id})
      |> Ash.read(domain: RivaAsh.Domain)
      assert length(instances) == 3

      # Check sequence numbers and dates
      Enum.with_index(instances, 1)
      |> Enum.each(fn {instance, expected_sequence} ->
        assert instance.sequence_number == expected_sequence
        expected_date = Date.add(Date.utc_today(), expected_sequence - 1)
        assert instance.scheduled_date == expected_date
        assert instance.status == :pending
      end)
    end

    test "creates weekdays only pattern correctly", %{employee: employee, client: client, item: item} do
      # Start on a Monday (2024-01-01 was a Monday)
      start_date = ~D[2024-01-01]

      attrs = %{
        client_id: client.id,
        item_id: item.id,
        employee_id: employee.id,
        start_date: start_date,
        start_time: ~T[09:00:00],
        end_time: ~T[10:00:00],
        consecutive_days: 5,  # 5 weekdays
        pattern_type: :weekdays_only,
        notes: "Weekdays only test"
      }

      assert {:ok, recurring_reservation} = RecurringReservations.create_recurring_reservation(attrs, true)

      # Check that instances were created for weekdays only
      assert {:ok, instances} = RecurringReservationInstance
      |> Ash.Query.for_read(:by_recurring_reservation, %{recurring_reservation_id: recurring_reservation.id})
      |> Ash.read(domain: RivaAsh.Domain)
      assert length(instances) == 5

      # All dates should be weekdays (Monday = 1, Friday = 5)
      instances
      |> Enum.each(fn instance ->
        day_of_week = Date.day_of_week(instance.scheduled_date)
        assert day_of_week in 1..5, "Expected weekday, got #{day_of_week} for #{instance.scheduled_date}"
      end)
    end
  end

  describe "process_instance/1" do
    setup do
      {:ok, business} = create_business()
      {:ok, employee} = create_employee(business.id, business.owner)
      {:ok, client} = create_client(business.id, business.owner)
      {:ok, item} = create_item(business.id, business.owner)

      attrs = %{
        client_id: client.id,
        item_id: item.id,
        employee_id: employee.id,
        start_date: Date.add(Date.utc_today(), 1),  # Tomorrow
        start_time: ~T[09:00:00],
        end_time: ~T[10:00:00],
        consecutive_days: 2,
        pattern_type: :all_days,
        notes: "Test for processing"
      }

      {:ok, recurring_reservation} = RecurringReservations.create_recurring_reservation(attrs, true)

      {:ok, instances} = RecurringReservationInstance
      |> Ash.Query.for_read(:by_recurring_reservation, %{recurring_reservation_id: recurring_reservation.id})
      |> Ash.read(domain: RivaAsh.Domain)

      [first_instance | _] = Enum.sort_by(instances, & &1.sequence_number)

      %{
        business: business,
        employee: employee,
        client: client,
        item: item,
        recurring_reservation: recurring_reservation,
        first_instance: first_instance
      }
    end

    test "successfully processes instance when time slot is available", %{first_instance: instance} do
      assert {:ok, updated_instance} = RecurringReservations.process_instance(instance.id)
      assert updated_instance.status == :confirmed
      assert updated_instance.reservation_id != nil
      assert updated_instance.error_message == nil
    end

    test "fails to process instance when time slot conflicts", %{first_instance: instance, item: item, client: client, employee: employee} do
      # Create a conflicting reservation first
      start_datetime = DateTime.new!(instance.scheduled_date, ~T[09:30:00], "Etc/UTC")
      end_datetime = DateTime.new!(instance.scheduled_date, ~T[10:30:00], "Etc/UTC")

      conflict_attrs = %{
        client_id: client.id,
        item_id: item.id,
        employee_id: employee.id,
        reserved_from: start_datetime,
        reserved_until: end_datetime,
        notes: "Conflicting reservation"
      }

      assert {:ok, _conflict} = RivaAsh.Resources.Reservation
        |> Ash.Changeset.for_create(:create, conflict_attrs)
        |> Ash.create(domain: RivaAsh.Domain)

      # Now try to process the instance
      assert {:ok, updated_instance} = RecurringReservations.process_instance(instance.id)
      assert updated_instance.status == :failed
      assert updated_instance.reservation_id == nil
      assert updated_instance.error_message =~ "conflicts"
    end
  end

  describe "get_recurring_reservation_stats/1" do
    test "returns correct statistics for recurring reservation" do
      {:ok, business} = create_business()
      {:ok, employee} = create_employee(business.id, business.owner)
      {:ok, client} = create_client(business.id, business.owner)
      {:ok, item} = create_item(business.id, business.owner)

      attrs = %{
        client_id: client.id,
        item_id: item.id,
        employee_id: employee.id,
        start_date: Date.add(Date.utc_today(), 1),
        start_time: ~T[09:00:00],
        end_time: ~T[10:00:00],
        consecutive_days: 3,
        pattern_type: :all_days
      }

      {:ok, recurring_reservation} = RecurringReservations.create_recurring_reservation(attrs, true)

      assert {:ok, stats} = RecurringReservations.get_recurring_reservation_stats(recurring_reservation.id)
      assert stats.pending == 3
      assert stats.confirmed == 0
      assert stats.failed == 0
      assert stats.skipped == 0
    end
  end

  describe "cancel_recurring_reservation/2" do
    test "cancels recurring reservation and all pending instances" do
      {:ok, business} = create_business()
      {:ok, employee} = create_employee(business.id, business.owner)
      {:ok, client} = create_client(business.id, business.owner)
      {:ok, item} = create_item(business.id, business.owner)

      attrs = %{
        client_id: client.id,
        item_id: item.id,
        employee_id: employee.id,
        start_date: Date.add(Date.utc_today(), 1),
        start_time: ~T[09:00:00],
        end_time: ~T[10:00:00],
        consecutive_days: 3,
        pattern_type: :all_days
      }

      {:ok, recurring_reservation} = RecurringReservations.create_recurring_reservation(attrs, true)

      # Cancel the recurring reservation
      assert {:ok, updated_reservation} = RecurringReservations.cancel_recurring_reservation(
        recurring_reservation.id,
        "Test cancellation"
      )
      assert updated_reservation.status == :cancelled

      # Check that all instances are skipped
      assert {:ok, instances} = RecurringReservationInstance
      |> Ash.Query.for_read(:by_recurring_reservation, %{recurring_reservation_id: recurring_reservation.id})
      |> Ash.read(domain: RivaAsh.Domain)
      assert length(instances) == 3

      instances
      |> Enum.each(fn instance ->
        assert instance.status == :skipped
        assert instance.skip_reason == "Test cancellation"
      end)
    end
  end

  describe "process_all_instances/1" do
    test "processes all pending instances for a recurring reservation" do
      {:ok, business} = create_business()
      {:ok, employee} = create_employee(business.id, business.owner)
      {:ok, client} = create_client(business.id, business.owner)
      {:ok, item} = create_item(business.id, business.owner)

      attrs = %{
        client_id: client.id,
        item_id: item.id,
        employee_id: employee.id,
        start_date: Date.add(Date.utc_today(), 1),
        start_time: ~T[09:00:00],
        end_time: ~T[10:00:00],
        consecutive_days: 2,
        pattern_type: :all_days
      }

      {:ok, recurring_reservation} = RecurringReservations.create_recurring_reservation(attrs, true)

      assert {:ok, results} = RecurringReservations.process_all_instances(recurring_reservation.id)
      assert results.total == 2
      assert length(results.successes) == 2
      assert length(results.failures) == 0

      # Verify all instances are now confirmed
      assert {:ok, stats} = RecurringReservations.get_recurring_reservation_stats(recurring_reservation.id)
      assert stats.confirmed == 2
      assert stats.pending == 0
    end
  end

  # Helper functions for creating test data
  defp create_business do
    # Create a user to act as the business owner
    user = RivaAsh.Accounts.User
           |> Ash.Changeset.for_create(:register_with_password, %{
             name: "Test Owner #{System.unique_integer()}",
             email: "owner#{System.unique_integer()}@example.com",
             password: "password123",
             role: :admin
           })
           |> Ash.create!(domain: RivaAsh.Accounts)

    attrs = %{
      name: "Test Business #{System.unique_integer()}",
      description: "A test business for recurring reservations"
    }

    Business
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.Changeset.manage_relationship(:owner, user, type: :append_and_remove)
    |> Ash.create(actor: user, domain: RivaAsh.Domain)
  end

  defp create_employee(business_id, actor) do
    unique_id = System.unique_integer([:positive])
    attrs = %{
      business_id: business_id,
      first_name: "Test",
      last_name: "Employee #{unique_id}",
      email: "employee#{unique_id}@example.com",
      role: :staff
    }

    Employee
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create(actor: actor, domain: RivaAsh.Domain)
  end

  defp create_client(business_id, actor) do
    attrs = %{
      business_id: business_id,
      name: "Test Client #{System.unique_integer()}",
      email: "client#{System.unique_integer()}@example.com",
      phone: "555-0456"
    }

    Client
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create(actor: actor, domain: RivaAsh.Domain)
  end

  defp create_item(business_id, actor) do
    attrs = %{
      business_id: business_id,
      name: "Test Item #{System.unique_integer()}",
      description: "A test item for reservations"
    }

    Item
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create(actor: actor, domain: RivaAsh.Domain)
  end
end
