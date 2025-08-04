defmodule RivaAsh.Resources.PricingResourceTest do
  use RivaAsh.DataCase

  alias RivaAsh.Resources.{Pricing, ItemType}
  alias RivaAsh.Factory

  defp create_item_type!(business_id) do
    ItemType
    |> Ash.Changeset.for_create(
      :create,
      Factory.item_type_attrs(%{business_id: business_id}) |> Enum.take(1) |> hd()
    )
    |> Ash.create!()
  end

  test "set price tier applies correct amount" do
    %{business: business} = Factory.sample_data()

    item_type = create_item_type!(business.id)

    {:ok, pricing} =
      Pricing
      |> Ash.Changeset.for_create(:create, %{
        business_id: business.id,
        item_type_id: item_type.id,
        pricing_type: :base,
        price_per_day: Decimal.new("100.00"),
        currency: "USD",
        is_active: true,
        name: "Base"
      })
      |> Ash.create()

    assert pricing.price_per_day == Decimal.new("100.00")
    assert pricing.is_active == true
  end

  test "invalid tier returns validation error" do
    %{business: business} = Factory.sample_data()
    item_type = create_item_type!(business.id)

    assert {:error, %Ash.Error.Invalid{}} =
             Pricing
             |> Ash.Changeset.for_create(:create, %{
               business_id: business.id,
               item_type_id: item_type.id,
               pricing_type: :base,
               price_per_day: Decimal.new("-5.00"),
               currency: "USD",
               is_active: true
             })
             |> Ash.create()
  end
end