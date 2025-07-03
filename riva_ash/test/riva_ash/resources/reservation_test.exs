defmodule RivaAsh.Resources.ReservationTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Resources.{Reservation, Client, Item}
  import RivaAsh.Factory
  import ExUnit.CaptureLog

  setup do
    # Create test data
    {:ok, client} = Client.create(%{name: "Test Client", email: "test@example.com", is_registered: true})
    {:ok, item} = Item.create(%{name: "Test Item"})
    
    # Future dates for testing
    future_date = DateTime.utc_now() |> DateTime.add(3600, :second)  # 1 hour from now
    end_future_date = future_date |> DateTime.add(3600, :second)     # 2 hours from now
    
    {:ok, client: client, item: item, future_date: future_date, end_future_date: end_future_date}
  end

  describe "create/1" do
    setup %{client: client, item: item, future_date: from, end_future_date: until} do
      %{
        valid_attrs: %{
          client_id: client.id,
          item_id: item.id,
          reserved_from: from,
          reserved_until: until,
          notes: "Test reservation"
        }
      }
    end

    test "creates a reservation with valid attributes", %{valid_attrs: attrs} do
      assert {:ok, reservation} = Reservation.create(attrs)
      assert reservation.client_id == attrs.client_id
      assert reservation.item_id == attrs.item_id
      assert reservation.status == "pending"
    end

    test "validates time slot availability", %{valid_attrs: attrs, future_date: from, end_future_date: until} do
      # Create first reservation
      assert {:ok, _} = Reservation.create(attrs)
      
      # Try to create overlapping reservation
      overlapping_attrs = %{
        attrs | 
        reserved_from: from |> DateTime.add(1800, :second),  # 30 minutes after first starts
        reserved_until: until |> DateTime.add(1800, :second) # 30 minutes after first ends
      }
      
      assert {:error, %{errors: errors}} = Reservation.create(overlapping_attrs)
      assert "Time slot overlaps with an existing reservation" in errors_on(errors, :reserved_from)
    end
  end

  describe "status updates" do
    setup %{client: client, item: item, future_date: from, end_future_date: until} do
      {:ok, reservation} = Reservation.create(%{
        client_id: client.id,
        item_id: item.id,
        reserved_from: from,
        reserved_until: until
      })
      %{reservation: reservation}
    end

    test "confirm/1 changes status to confirmed", %{reservation: reservation} do
      assert {:ok, updated} = Reservation.confirm(reservation.id)
      assert updated.status == "confirmed"
    end

    test "cancel/1 changes status to cancelled", %{reservation: reservation} do
      assert {:ok, updated} = Reservation.cancel(reservation.id)
      assert updated.status == "cancelled"
    end

    test "complete/1 changes status to completed", %{reservation: reservation} do
      assert {:ok, updated} = Reservation.complete(reservation.id)
      assert updated.status == "completed"
    end
  end

  describe "queries" do
    setup %{client: client, item: item} do
      now = DateTime.utc_now()
      
      # Create reservations in different states
      {:ok, past} = Reservation.create(%{
        client_id: client.id,
        item_id: item.id,
        reserved_from: DateTime.add(now, -7200),  # 2 hours ago
        reserved_until: DateTime.add(now, -3600), # 1 hour ago
        status: "completed"
      })
      
      {:ok, current} = Reservation.create(%{
        client_id: client.id,
        item_id: item.id,
        reserved_from: DateTime.add(now, -1800), # 30 minutes ago
        reserved_until: DateTime.add(now, 1800), # 30 minutes from now
        status: "confirmed"
      })
      
      {:ok, future} = Reservation.create(%{
        client_id: client.id,
        item_id: item.id,
        reserved_from: DateTime.add(now, 3600),  # 1 hour from now
        reserved_until: DateTime.add(now, 7200), # 2 hours from now
        status: "pending"
      })
      
      %{past: past, current: current, future: future}
    end

    test "active/0 returns currently active reservations", %{current: current} do
      assert {:ok, [reservation]} = Reservation.active()
      assert reservation.id == current.id
    end

    test "upcoming/0 returns future reservations", %{future: future} do
      assert {:ok, reservations} = Reservation.upcoming()
      assert Enum.any?(reservations, &(&1.id == future.id))
    end

    test "past/0 returns completed or past reservations", %{past: past} do
      assert {:ok, reservations} = Reservation.past()
      assert Enum.any?(reservations, &(&1.id == past.id))
    end
  end
end
