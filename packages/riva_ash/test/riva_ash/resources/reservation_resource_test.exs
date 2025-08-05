defmodule RivaAsh.Resources.ReservationResourceTest do
  use RivaAsh.DataCase, async: true

  alias RivaAsh.Resources.Reservation
  alias RivaAsh.Factory

  describe "create via code interface" do
    @spec test_create_valid_attrs_ok :: :ok
    test "create valid attrs ok" do
      %{business: business, item: item, client: client} = Factory.sample_data()

      from_dt = DateTime.add(DateTime.utc_now(), 3600, :second)
      until_dt = DateTime.add(from_dt, 3600, :second)

      assert {:ok, res} =
               Reservation.create(%{
                 client_id: client.id,
                 item_id: item.id,
                 reserved_from: from_dt,
                 reserved_until: until_dt,
                 notes: "ok"
               })

      assert res.id
      assert res.item_id == item.id
      assert res.client_id == client.id
    end

    @spec test_duplicate_violates_unique_constraint_on_item_time_slot :: :ok
    test "duplicate violates unique constraint on item/time slot" do
      %{item: item, client: client} = Factory.sample_data()

      from_dt = DateTime.add(DateTime.utc_now(), 7200, :second)
      until_dt = DateTime.add(from_dt, 3600, :second)

      {:ok, _r1} =
        Reservation.create(%{
          client_id: client.id,
          item_id: item.id,
          reserved_from: from_dt,
          reserved_until: until_dt
        })

      assert {:error, changeset} =
               Reservation.create(%{
                 client_id: client.id,
                 item_id: item.id,
                 reserved_from: from_dt,
                 reserved_until: until_dt
               })

      # Identity unique_item_time_slot should cause error on fields
      errors = Ecto.Changeset.traverse_errors(changeset, fn {msg, _opts} -> msg end)
      assert is_map(errors)
    end
  end

  describe "update validations" do
    @spec test_update_range_validation_end_before_start :: :ok
    test "update range validation: end before start" do
      %{item: item, client: client} = Factory.sample_data()

      from_dt = DateTime.add(DateTime.utc_now(), 3600, :second)
      until_dt = DateTime.add(from_dt, 3600, :second)

      {:ok, res} =
        Reservation.create(%{
          client_id: client.id,
          item_id: item.id,
          reserved_from: from_dt,
          reserved_until: until_dt
        })

      # Set invalid range (until before from)
      bad_until = DateTime.add(from_dt, -600, :second)

      assert {:error, _changeset} =
               Reservation.update(res, %{reserved_until: bad_until}, domain: RivaAsh.Domain)
    end
  end
end