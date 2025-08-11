defmodule RivaAsh.Resources.PaymentResourceTest do
  use RivaAsh.DataCase

  alias RivaAsh.Resources.{Payment, Reservation}
  alias RivaAsh.Factory
  import RivaAsh.Test.TimeHelpers

  setup do
    clock_reset()
    :ok
  end

  defp create_reservation! do
    %{item: item, client: client} = Factory.sample_data()

    Reservation
    |> Ash.Changeset.for_create(:create, %{
      client_id: client.id,
      item_id: item.id,
      reserved_from: DateTime.add(DateTime.utc_now(), 3600, :second),
      reserved_until: DateTime.add(DateTime.utc_now(), 7200, :second),
      status: :pending
    })
    |> Ash.create!()
  end

  test "create valid payment updates reservation balance" do
    reservation = create_reservation!()

    {:ok, payment} =
      Payment
      |> Ash.Changeset.for_create(:create, %{
        reservation_id: reservation.id,
        amount_due: Decimal.new("50.00"),
        currency: "USD",
        payment_method: :cash
      })
      |> Ash.create()

    # After create, remaining balance should be positive
    assert Decimal.compare(payment.balance_remaining, Decimal.new("0")) == :gt

    {:ok, payment} =
      payment
      |> Ash.Changeset.for_update(:mark_as_paid, %{
        amount_paid: Decimal.new("50.00"),
        payment_method: :cash,
        payment_date: DateTime.utc_now()
      })
      |> Ash.update()

    assert payment.status == :paid
    assert payment.is_fully_paid == true
    refute Decimal.compare(payment.balance_remaining, Decimal.new("0")) == :gt
  end

  test "reject negative amount" do
    reservation = create_reservation!()

    assert {:error, %Ash.Error.Invalid{}} =
             Payment
             |> Ash.Changeset.for_create(:create, %{
               reservation_id: reservation.id,
               amount_due: Decimal.new("-1.00"),
               currency: "USD",
               payment_method: :cash
             })
             |> Ash.create()
  end

  test "currency validation fails for unsupported currency" do
    reservation = create_reservation!()

    assert {:error, %Ash.Error.Invalid{}} =
             Payment
             |> Ash.Changeset.for_create(:create, %{
               reservation_id: reservation.id,
               amount_due: Decimal.new("10.00"),
               # not a 3-letter ISO code per validations
               currency: "EURO",
               payment_method: :cash
             })
             |> Ash.create()
  end
end
