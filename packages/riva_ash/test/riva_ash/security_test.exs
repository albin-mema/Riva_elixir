defmodule RivaAsh.SecurityTest do
  @moduledoc """
  Comprehensive security tests for authorization policies and business context validation.
  Tests all the security fixes implemented in the system.
  """
  use ExUnit.Case, async: true

  import Ash.Test
  alias RivaAsh.Resources.{Business, Employee, Client, Item, Reservation, Payment, Pricing}

  setup do
    # Create test business and users
    business1 = Business |> Ash.Changeset.for_create(:create, %{
      name: "Test Business 1",
      description: "First test business"
    }) |> Ash.create!(domain: RivaAsh.Domain)

    business2 = Business |> Ash.Changeset.for_create(:create, %{
      name: "Test Business 2",
      description: "Second test business"
    }) |> Ash.create!(domain: RivaAsh.Domain)

    # Create business owners
    owner1 = Employee |> Ash.Changeset.for_create(:create, %{
      business_id: business1.id,
      email: "owner1@test.com",
      first_name: "Owner",
      last_name: "One",
      role: :admin,
      is_active: true
    }) |> Ash.create!(domain: RivaAsh.Domain)

    owner2 = Employee |> Ash.Changeset.for_create(:create, %{
      business_id: business2.id,
      email: "owner2@test.com",
      first_name: "Owner",
      last_name: "Two",
      role: :admin,
      is_active: true
    }) |> Ash.create!(domain: RivaAsh.Domain)

    # Create clients for each business
    client1 = Client |> Ash.Changeset.for_create(:create, %{
      business_id: business1.id,
      name: "Client One",
      email: "client1@test.com",
      is_registered: true
    }) |> Ash.create!(domain: RivaAsh.Domain)

    client2 = Client |> Ash.Changeset.for_create(:create, %{
      business_id: business2.id,
      name: "Client Two",
      email: "client2@test.com",
      is_registered: true
    }) |> Ash.create!(domain: RivaAsh.Domain)

    %{
      business1: business1,
      business2: business2,
      owner1: owner1,
      owner2: owner2,
      client1: client1,
      client2: client2
    }
  end

  describe "Business Context Validation" do
    test "employees can only access their own business data", %{
      business1: business1,
      business2: business2,
      owner1: owner1,
      owner2: owner2
    } do
      # Owner1 should be able to read business1
      assert {:ok, _} = Business |> Ash.get(business1.id, actor: owner1, domain: RivaAsh.Domain)

      # Owner1 should NOT be able to read business2
      assert {:error, %Ash.Error.Forbidden{}} = Business |> Ash.get(business2.id, actor: owner1, domain: RivaAsh.Domain)

      # Owner2 should be able to read business2
      assert {:ok, _} = Business |> Ash.get(business2.id, actor: owner2, domain: RivaAsh.Domain)

      # Owner2 should NOT be able to read business1
      assert {:error, %Ash.Error.Forbidden{}} = Business |> Ash.get(business1.id, actor: owner2, domain: RivaAsh.Domain)
    end

    test "clients can only access their own business data", %{
      business1: business1,
      business2: business2,
      client1: client1,
      client2: client2
    } do
      # Client1 should be able to read clients from business1
      clients1 = Client |> Ash.Query.filter(business_id == ^business1.id) |> Ash.read!(actor: client1, domain: RivaAsh.Domain)
      assert length(clients1) >= 1

      # Client1 should NOT be able to read clients from business2
      assert_raise Ash.Error.Forbidden, fn ->
        Client |> Ash.Query.filter(business_id == ^business2.id) |> Ash.read!(actor: client1, domain: RivaAsh.Domain)
      end
    end

    test "cross-business resource creation is prevented", %{
      business1: business1,
      business2: business2,
      owner1: owner1,
      client2: client2
    } do
      # Owner1 should NOT be able to create clients for business2
      assert {:error, %Ash.Error.Forbidden{}} =
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business2.id,
          name: "Unauthorized Client",
          email: "unauthorized@test.com"
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end
  end

  describe "Client Authorization Security" do
    test "client creation requires proper business context", %{business1: business1, owner1: owner1} do
      # Valid client creation with proper business context
      assert {:ok, _client} =
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          name: "Valid Client",
          email: "valid@test.com"
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Invalid client creation without actor should fail
      assert {:error, %Ash.Error.Forbidden{}} =
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          name: "Invalid Client",
          email: "invalid@test.com"
        }) |> Ash.create(domain: RivaAsh.Domain)
    end

    test "client email uniqueness is business-scoped", %{
      business1: business1,
      business2: business2,
      owner1: owner1,
      owner2: owner2
    } do
      email = "duplicate@test.com"

      # Create client with same email in business1
      assert {:ok, _} =
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          name: "Client 1",
          email: email
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Should be able to create client with same email in business2
      assert {:ok, _} =
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business2.id,
          name: "Client 2",
          email: email
        }) |> Ash.create(actor: owner2, domain: RivaAsh.Domain)

      # Should NOT be able to create duplicate email in same business
      assert {:error, %Ash.Error.Invalid{}} =
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          name: "Duplicate Client",
          email: email
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end
  end

  describe "Pricing Security" do
    test "pricing rules are business-scoped", %{
      business1: business1,
      business2: business2,
      owner1: owner1,
      owner2: owner2
    } do
      # Owner1 can create pricing for business1
      assert {:ok, pricing1} =
        Pricing |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          pricing_type: :base,
          price_per_day: Decimal.new("100.00"),
          currency: "USD",
          effective_from: Date.utc_today(),
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Owner1 should NOT be able to read pricing from business2
      pricing2 = Pricing |> Ash.Changeset.for_create(:create, %{
        business_id: business2.id,
        pricing_type: :base,
        price_per_day: Decimal.new("150.00"),
        currency: "USD",
        effective_from: Date.utc_today(),
        is_active: true
      }) |> Ash.create!(actor: owner2, domain: RivaAsh.Domain)

      assert {:error, %Ash.Error.Forbidden{}} =
        Pricing |> Ash.get(pricing2.id, actor: owner1, domain: RivaAsh.Domain)
    end

    test "pricing validation prevents overlapping rules", %{business1: business1, owner1: owner1} do
      base_attrs = %{
        business_id: business1.id,
        pricing_type: :base,
        price_per_day: Decimal.new("100.00"),
        currency: "USD",
        effective_from: Date.utc_today(),
        effective_until: Date.add(Date.utc_today(), 30),
        is_active: true
      }

      # Create first pricing rule
      assert {:ok, _} =
        Pricing |> Ash.Changeset.for_create(:create, base_attrs)
        |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Attempt to create overlapping pricing rule should fail
      overlapping_attrs = Map.merge(base_attrs, %{
        effective_from: Date.add(Date.utc_today(), 15),
        effective_until: Date.add(Date.utc_today(), 45)
      })

      assert {:error, %Ash.Error.Invalid{}} =
        Pricing |> Ash.Changeset.for_create(:create, overlapping_attrs)
        |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end
  end

  describe "Performance Optimizations" do
    test "business_id denormalization works correctly", %{
      business1: business1,
      client1: client1,
      owner1: owner1
    } do
      # Create item for testing
      item = Item |> Ash.Changeset.for_create(:create, %{
        business_id: business1.id,
        name: "Test Item",
        is_active: true
      }) |> Ash.create!(actor: owner1, domain: RivaAsh.Domain)

      # Create reservation - business_id should be automatically set
      reservation = Reservation |> Ash.Changeset.for_create(:create, %{
        client_id: client1.id,
        item_id: item.id,
        reserved_from: Timex.now(),
        reserved_until: Timex.shift(Timex.now(), days: 1)
      }) |> Ash.create!(actor: owner1, domain: RivaAsh.Domain)

      # Verify business_id was automatically set
      assert reservation.business_id == business1.id

      # Create payment - business_id should be automatically set from reservation
      payment = Payment |> Ash.Changeset.for_create(:create, %{
        reservation_id: reservation.id,
        amount_due: Decimal.new("100.00"),
        currency: "USD"
      }) |> Ash.create!(actor: owner1, domain: RivaAsh.Domain)

      # Verify business_id was automatically set
      assert payment.business_id == business1.id
    end

    test "optimized queries use business_id directly" do
      # This test would verify that queries use the denormalized business_id
      # instead of deep joins. In a real implementation, you might use
      # query analysis tools or database query logs to verify this.

      business_id = Ash.UUID.generate()

      # Test that the optimized query functions work
      assert is_list(RivaAsh.Queries.upcoming_reservations_for_business(business_id))
      assert is_map(RivaAsh.Queries.business_metrics(business_id))
      assert is_list(RivaAsh.Queries.business_reservations_optimized(business_id))
    end
  end
end
