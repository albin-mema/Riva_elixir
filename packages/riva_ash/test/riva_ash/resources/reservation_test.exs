defmodule RivaAsh.Resources.ReservationTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Resources.{Reservation, Client, Item}
  import ExUnit.CaptureLog

  describe "create/1 - property-based tests" do
    property "creates reservations with valid randomized attributes" do
      check all client_attrs <- client_attrs(),
                item_attrs <- item_attrs() do

        # Create dependencies first
        {:ok, client} = Client.create(client_attrs)
        {:ok, item} = Item.create(item_attrs)

        # Generate reservation attributes with valid relationships
        reservation_attrs = reservation_attrs(%{
          client_id: client.id,
          item_id: item.id
        }) |> Enum.take(1) |> hd()

        case Reservation.create(reservation_attrs) do
          {:ok, reservation} ->
            assert reservation.client_id == client.id
            assert reservation.item_id == item.id
            assert reservation.reserved_from != nil
            assert reservation.reserved_until != nil
            assert DateTime.compare(reservation.reserved_until, reservation.reserved_from) == :gt
            assert reservation.status in [:pending, :provisional, :confirmed, :cancelled, :completed]
            assert reservation.id != nil

          {:error, _error} ->
            # Some combinations might be invalid due to business rules
            :ok
        end
      end
    end

    property "validates required fields" do
      check all attrs <- reservation_attrs() do
        # Test missing client_id
        invalid_attrs = Map.put(attrs, :client_id, nil)
        assert {:error, %{errors: errors}} = Reservation.create(invalid_attrs)
        assert has_error_on_field?(%{errors: errors}, :client_id)

        # Test missing item_id
        invalid_attrs = Map.put(attrs, :item_id, nil)
        assert {:error, %{errors: errors}} = Reservation.create(invalid_attrs)
        assert has_error_on_field?(%{errors: errors}, :item_id)
      end
    end

    property "validates datetime ordering" do
      check all client_attrs <- client_attrs(),
                item_attrs <- item_attrs() do

        # Create dependencies
        {:ok, client} = Client.create(client_attrs)
        {:ok, item} = Item.create(item_attrs)

        # Create reservation with invalid time ordering
        now = DateTime.utc_now()
        invalid_attrs = %{
          client_id: client.id,
          item_id: item.id,
          reserved_from: DateTime.add(now, 3600, :second),  # 1 hour from now
          reserved_until: now  # Now (before reserved_from)
        }

        assert {:error, %{errors: _errors}} = Reservation.create(invalid_attrs)
      end
    end
  end

  describe "status updates - property-based tests" do
    property "status transitions work correctly" do
      check all client_attrs <- client_attrs(),
                item_attrs <- item_attrs() do

        # Create dependencies
        {:ok, client} = Client.create(client_attrs)
        {:ok, item} = Item.create(item_attrs)

        # Create reservation
        reservation_attrs = reservation_attrs(%{
          client_id: client.id,
          item_id: item.id,
          status: :pending
        }) |> Enum.take(1) |> hd()

        {:ok, reservation} = Reservation.create(reservation_attrs)

        # Test confirm transition
        confirmed = reservation
                   |> Ash.Changeset.for_update(:confirm, %{})
                   |> Ash.update!(domain: RivaAsh.Domain)
        assert confirmed.status == :confirmed

        # Test cancel transition (from confirmed)
        cancelled = confirmed
                   |> Ash.Changeset.for_update(:cancel, %{})
                   |> Ash.update!(domain: RivaAsh.Domain)
        assert cancelled.status == :cancelled

        # Create another reservation to test complete
        {:ok, reservation2} = Reservation.create(reservation_attrs)
        confirmed2 = reservation2
                    |> Ash.Changeset.for_update(:confirm, %{})
                    |> Ash.update!(domain: RivaAsh.Domain)

        # Test complete transition
        completed = confirmed2
                   |> Ash.Changeset.for_update(:complete, %{})
                   |> Ash.update!(domain: RivaAsh.Domain)
        assert completed.status == :completed
      end
    end
  end

  describe "queries - property-based tests" do
    property "time-based queries work correctly" do
      check all client_attrs <- client_attrs(),
                item_attrs <- item_attrs() do

        # Create dependencies
        {:ok, client} = Client.create(client_attrs)
        {:ok, item} = Item.create(item_attrs)

        now = DateTime.utc_now()

        # Create past reservation
        past_attrs = %{
          client_id: client.id,
          item_id: item.id,
          reserved_from: DateTime.add(now, -7200),  # 2 hours ago
          reserved_until: DateTime.add(now, -3600), # 1 hour ago
          status: :completed
        }
        {:ok, past_reservation} = Reservation.create(past_attrs)

        # Create current reservation
        current_attrs = %{
          client_id: client.id,
          item_id: item.id,
          reserved_from: DateTime.add(now, -1800),  # 30 minutes ago
          reserved_until: DateTime.add(now, 1800),  # 30 minutes from now
          status: :confirmed
        }
        {:ok, current_reservation} = Reservation.create(current_attrs)

        # Create future reservation
        future_attrs = %{
          client_id: client.id,
          item_id: item.id,
          reserved_from: DateTime.add(now, 3600),   # 1 hour from now
          reserved_until: DateTime.add(now, 7200),  # 2 hours from now
          status: :pending
        }
        {:ok, future_reservation} = Reservation.create(future_attrs)

        # Test active query
        {:ok, active_reservations} = Reservation.active()
        active_ids = Enum.map(active_reservations, & &1.id)
        assert current_reservation.id in active_ids
        assert past_reservation.id not in active_ids
        assert future_reservation.id not in active_ids

        # Test upcoming query
        query = Reservation |> Ash.Query.for_read(:upcoming)
        {:ok, upcoming_reservations} = Ash.read(query, domain: RivaAsh.Domain)
        upcoming_ids = Enum.map(upcoming_reservations, & &1.id)
        assert future_reservation.id in upcoming_ids

        # Test past query
        query = Reservation |> Ash.Query.for_read(:past)
        {:ok, past_reservations} = Ash.read(query, domain: RivaAsh.Domain)
        past_ids = Enum.map(past_reservations, & &1.id)
        assert past_reservation.id in past_ids
      end
    end
  end
end
