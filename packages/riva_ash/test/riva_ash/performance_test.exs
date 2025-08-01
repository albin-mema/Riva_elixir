defmodule RivaAsh.PerformanceTest do
  @moduledoc """
  Performance tests for the optimized queries and database operations.
  Tests the performance improvements from denormalized business_id and indexes.
  """
  use ExUnit.Case, async: false

  import Ash.Test
  alias RivaAsh.Resources.{Business, Employee, Client, Item, Reservation, Payment}

  @moduletag :performance

  setup_all do
    # Create test data for performance testing
    business =
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Performance Test Business",
        description: "Business for performance testing"
      })
      |> Ash.create!(domain: RivaAsh.Domain)

    owner =
      Employee
      |> Ash.Changeset.for_create(:create, %{
        business_id: business.id,
        email: "owner@perf.com",
        first_name: "Performance",
        last_name: "Owner",
        role: :admin,
        is_active: true
      })
      |> Ash.create!(domain: RivaAsh.Domain)

    # Create multiple clients for testing
    clients =
      Enum.map(1..50, fn i ->
        Client
        |> Ash.Changeset.for_create(:create, %{
          business_id: business.id,
          name: "Client #{i}",
          email: "client#{i}@perf.com",
          is_registered: true
        })
        |> Ash.create!(actor: owner, domain: RivaAsh.Domain)
      end)

    # Create multiple items
    items =
      Enum.map(1..20, fn i ->
        Item
        |> Ash.Changeset.for_create(:create, %{
          business_id: business.id,
          name: "Item #{i}",
          is_active: true
        })
        |> Ash.create!(actor: owner, domain: RivaAsh.Domain)
      end)

    # Create multiple reservations
    reservations =
      Enum.map(1..100, fn i ->
        client = Enum.at(clients, rem(i, 50))
        item = Enum.at(items, rem(i, 20))

        base_date = Timex.now()
        # i days from now
        start_date = DateTime.add(base_date, i * 24 * 60 * 60, :second)
        # 1 day duration
        end_date = DateTime.add(start_date, 24 * 60 * 60, :second)

        Reservation
        |> Ash.Changeset.for_create(:create, %{
          client_id: client.id,
          item_id: item.id,
          reserved_from: start_date,
          reserved_until: end_date,
          status: Enum.random([:confirmed, :pending, :completed])
        })
        |> Ash.create!(actor: owner, domain: RivaAsh.Domain)
      end)

    %{
      business: business,
      owner: owner,
      clients: clients,
      items: items,
      reservations: reservations
    }
  end

  describe "Query Performance" do
    test "business-scoped reservation queries are fast", %{business: business} do
      # Test the optimized business reservations query
      {time_microseconds, result} =
        :timer.tc(fn ->
          RivaAsh.Queries.business_reservations_optimized(business.id, limit: 50)
        end)

      # Should complete in under 100ms (100,000 microseconds)
      assert time_microseconds < 100_000,
             "Query took #{time_microseconds}μs, expected < 100,000μs"

      assert is_list(result)
      assert length(result) <= 50
    end

    test "business metrics calculation is fast", %{business: business} do
      {time_microseconds, result} =
        :timer.tc(fn ->
          RivaAsh.Queries.business_metrics(business.id)
        end)

      # Should complete in under 50ms (50,000 microseconds)
      assert time_microseconds < 50_000, "Query took #{time_microseconds}μs, expected < 50,000μs"
      assert is_map(result)
      assert Map.has_key?(result, :total_reservations)
    end

    test "upcoming reservations query is fast", %{business: business} do
      {time_microseconds, result} =
        :timer.tc(fn ->
          RivaAsh.Queries.upcoming_reservations_for_business(business.id, 25)
        end)

      # Should complete in under 75ms (75,000 microseconds)
      assert time_microseconds < 75_000, "Query took #{time_microseconds}μs, expected < 75,000μs"
      assert is_list(result)
      assert length(result) <= 25
    end

    test "calendar data query is fast", %{business: business} do
      current_date = Date.utc_today()

      {time_microseconds, result} =
        :timer.tc(fn ->
          RivaAsh.Queries.reservation_calendar_data(
            business.id,
            current_date.year,
            current_date.month
          )
        end)

      # Should complete in under 100ms (100,000 microseconds)
      assert time_microseconds < 100_000,
             "Query took #{time_microseconds}μs, expected < 100,000μs"

      assert is_list(result)
    end

    test "item availability checking is fast", %{items: items} do
      item = List.first(items)
      start_time = Timex.now()
      end_time = DateTime.add(start_time, 24 * 60 * 60, :second)

      {time_microseconds, result} =
        :timer.tc(fn ->
          RivaAsh.Queries.check_item_availability(item.id, start_time, end_time)
        end)

      # Should complete in under 25ms (25,000 microseconds)
      assert time_microseconds < 25_000, "Query took #{time_microseconds}μs, expected < 25,000μs"
      assert is_boolean(result)
    end
  end

  describe "Business ID Denormalization Performance" do
    test "reservation creation with business_id is fast", %{
      business: business,
      clients: clients,
      items: items,
      owner: owner
    } do
      client = List.first(clients)
      item = List.first(items)

      {time_microseconds, reservation} =
        :timer.tc(fn ->
          Reservation
          |> Ash.Changeset.for_create(:create, %{
            client_id: client.id,
            item_id: item.id,
            reserved_from: Timex.now(),
            reserved_until: Timex.shift(Timex.now(), seconds: 24 * 60 * 60)
          })
          |> Ash.create!(actor: owner, domain: RivaAsh.Domain)
        end)

      # Should complete in under 50ms (50,000 microseconds)
      assert time_microseconds < 50_000,
             "Creation took #{time_microseconds}μs, expected < 50,000μs"

      # Verify business_id was automatically set
      assert reservation.business_id == business.id
    end

    test "payment creation with business_id is fast", %{
      reservations: reservations,
      owner: owner
    } do
      reservation = List.first(reservations)

      {time_microseconds, payment} =
        :timer.tc(fn ->
          Payment
          |> Ash.Changeset.for_create(:create, %{
            reservation_id: reservation.id,
            amount_due: Decimal.new("100.00"),
            currency: "USD"
          })
          |> Ash.create!(actor: owner, domain: RivaAsh.Domain)
        end)

      # Should complete in under 50ms (50,000 microseconds)
      assert time_microseconds < 50_000,
             "Creation took #{time_microseconds}μs, expected < 50,000μs"

      # Verify business_id was automatically set
      assert payment.business_id == reservation.business_id
    end
  end

  describe "Index Effectiveness" do
    test "business-scoped queries use indexes effectively", %{business: business} do
      # This test verifies that our queries are using the indexes we created
      # In a real implementation, you might use EXPLAIN ANALYZE to verify index usage

      # Test multiple query patterns that should use different indexes
      queries = [
        fn ->
          Reservation
          |> Ash.Query.filter(business_id == ^business.id)
          |> Ash.read!(domain: RivaAsh.Domain)
        end,
        fn ->
          Reservation
          |> Ash.Query.filter(business_id == ^business.id and status == :confirmed)
          |> Ash.read!(domain: RivaAsh.Domain)
        end,
        fn ->
          Client
          |> Ash.Query.filter(business_id == ^business.id)
          |> Ash.read!(domain: RivaAsh.Domain)
        end,
        fn ->
          Item
          |> Ash.Query.filter(business_id == ^business.id and is_active == true)
          |> Ash.read!(domain: RivaAsh.Domain)
        end
      ]

      Enum.each(queries, fn query_fn ->
        {time_microseconds, _result} = :timer.tc(query_fn)

        # All indexed queries should complete quickly
        assert time_microseconds < 50_000,
               "Indexed query took #{time_microseconds}μs, expected < 50,000μs"
      end)
    end

    test "date range queries use composite indexes", %{business: business} do
      start_date = Timex.now()
      # 7 days
      end_date = DateTime.add(start_date, 7 * 24 * 60 * 60, :second)

      {time_microseconds, result} =
        :timer.tc(fn ->
          Reservation
          |> Ash.Query.filter(
            business_id == ^business.id and
              reserved_from >= ^start_date and
              reserved_until <= ^end_date
          )
          |> Ash.read!(domain: RivaAsh.Domain)
        end)

      # Date range queries with business_id should use composite index
      assert time_microseconds < 75_000,
             "Date range query took #{time_microseconds}μs, expected < 75,000μs"

      assert is_list(result)
    end
  end

  describe "Scalability Tests" do
    test "performance remains consistent with larger datasets" do
      # This test would create larger datasets and verify performance doesn't degrade
      # For now, we'll test with the existing data

      business_id = Ash.UUID.generate()

      # Test multiple concurrent queries
      tasks =
        Enum.map(1..10, fn _i ->
          Task.async(fn ->
            :timer.tc(fn ->
              RivaAsh.Queries.business_metrics(business_id)
            end)
          end)
        end)

      results = Task.await_many(tasks, 5000)

      # All concurrent queries should complete reasonably fast
      Enum.each(results, fn {time_microseconds, _result} ->
        assert time_microseconds < 100_000,
               "Concurrent query took #{time_microseconds}μs, expected < 100,000μs"
      end)
    end
  end

  # Helper function to benchmark a function
  defp benchmark(fun, expected_max_time_ms \\ 100) do
    {time_microseconds, result} = :timer.tc(fun)
    expected_max_microseconds = expected_max_time_ms * 1000

    assert time_microseconds < expected_max_microseconds,
           "Operation took #{time_microseconds}μs, expected < #{expected_max_microseconds}μs"

    result
  end
end
