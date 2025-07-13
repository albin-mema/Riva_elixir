defmodule RivaAsh.ValidationsTest do
  @moduledoc """
  Tests for all custom validations implemented in the system.
  Covers business logic validation, cross-business relationship validation, and data integrity.
  """
  use ExUnit.Case, async: true
  
  import Ash.Test
  alias RivaAsh.Resources.{Business, Employee, Client, Item, ItemType, Section, Plot, Reservation, Pricing}

  setup do
    # Create test businesses
    business1 = Business |> Ash.Changeset.for_create(:create, %{
      name: "Business One",
      description: "First business"
    }) |> Ash.create!(domain: RivaAsh.Domain)

    business2 = Business |> Ash.Changeset.for_create(:create, %{
      name: "Business Two", 
      description: "Second business"
    }) |> Ash.create!(domain: RivaAsh.Domain)

    # Create owners
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

    # Create supporting resources
    plot1 = Plot |> Ash.Changeset.for_create(:create, %{
      business_id: business1.id,
      name: "Plot 1",
      is_active: true
    }) |> Ash.create!(actor: owner1, domain: RivaAsh.Domain)

    section1 = Section |> Ash.Changeset.for_create(:create, %{
      business_id: business1.id,
      plot_id: plot1.id,
      name: "Section 1",
      is_active: true
    }) |> Ash.create!(actor: owner1, domain: RivaAsh.Domain)

    item_type1 = ItemType |> Ash.Changeset.for_create(:create, %{
      business_id: business1.id,
      name: "Type 1",
      is_active: true
    }) |> Ash.create!(actor: owner1, domain: RivaAsh.Domain)

    item_type2 = ItemType |> Ash.Changeset.for_create(:create, %{
      business_id: business2.id,
      name: "Type 2", 
      is_active: true
    }) |> Ash.create!(actor: owner2, domain: RivaAsh.Domain)

    %{
      business1: business1,
      business2: business2,
      owner1: owner1,
      owner2: owner2,
      plot1: plot1,
      section1: section1,
      item_type1: item_type1,
      item_type2: item_type2
    }
  end

  describe "Cross-Business Relationship Validation" do
    test "item cannot reference section from different business", %{
      business1: business1,
      business2: business2,
      section1: section1,
      item_type1: item_type1,
      owner2: owner2
    } do
      # Attempt to create item in business2 with section from business1
      assert {:error, %Ash.Error.Invalid{}} = 
        Item |> Ash.Changeset.for_create(:create, %{
          business_id: business2.id,
          section_id: section1.id,  # Section from business1
          item_type_id: item_type1.id,  # ItemType from business1
          name: "Invalid Item",
          is_active: true
        }) |> Ash.create(actor: owner2, domain: RivaAsh.Domain)
    end

    test "item cannot reference item_type from different business", %{
      business1: business1,
      section1: section1,
      item_type2: item_type2,  # From business2
      owner1: owner1
    } do
      # Attempt to create item in business1 with item_type from business2
      assert {:error, %Ash.Error.Invalid{}} = 
        Item |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          section_id: section1.id,
          item_type_id: item_type2.id,  # ItemType from business2
          name: "Invalid Item",
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end

    test "valid cross-business relationships are allowed", %{
      business1: business1,
      section1: section1,
      item_type1: item_type1,
      owner1: owner1
    } do
      # Create item with all resources from same business
      assert {:ok, _item} = 
        Item |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          section_id: section1.id,
          item_type_id: item_type1.id,
          name: "Valid Item",
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end
  end

  describe "Pricing Validation" do
    test "pricing date validation works correctly", %{business1: business1, owner1: owner1} do
      # Valid pricing with proper date range
      assert {:ok, _pricing} = 
        Pricing |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          pricing_type: :base,
          price_per_day: Decimal.new("100.00"),
          currency: "USD",
          effective_from: Date.utc_today(),
          effective_until: Date.add(Date.utc_today(), 30),
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Invalid pricing with end date before start date
      assert {:error, %Ash.Error.Invalid{}} = 
        Pricing |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          pricing_type: :base,
          price_per_day: Decimal.new("100.00"),
          currency: "USD",
          effective_from: Date.utc_today(),
          effective_until: Date.add(Date.utc_today(), -10),  # Before start date
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end

    test "pricing currency validation works", %{business1: business1, owner1: owner1} do
      # Valid currency code
      assert {:ok, _pricing} = 
        Pricing |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          pricing_type: :base,
          price_per_day: Decimal.new("100.00"),
          currency: "USD",
          effective_from: Date.utc_today(),
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Invalid currency code
      assert {:error, %Ash.Error.Invalid{}} = 
        Pricing |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          pricing_type: :base,
          price_per_day: Decimal.new("100.00"),
          currency: "INVALID",  # Invalid currency code
          effective_from: Date.utc_today(),
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end

    test "negative pricing is prevented", %{business1: business1, owner1: owner1} do
      assert {:error, %Ash.Error.Invalid{}} = 
        Pricing |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          pricing_type: :base,
          price_per_day: Decimal.new("-50.00"),  # Negative price
          currency: "USD",
          effective_from: Date.utc_today(),
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end

    test "overlapping pricing rules are prevented", %{
      business1: business1,
      item_type1: item_type1,
      owner1: owner1
    } do
      base_attrs = %{
        business_id: business1.id,
        item_type_id: item_type1.id,
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

      # Attempt to create overlapping rule
      overlapping_attrs = Map.merge(base_attrs, %{
        effective_from: Date.add(Date.utc_today(), 15),
        effective_until: Date.add(Date.utc_today(), 45)
      })

      assert {:error, %Ash.Error.Invalid{}} = 
        Pricing |> Ash.Changeset.for_create(:create, overlapping_attrs)
        |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end

    test "only one active base pricing rule per business/item_type", %{
      business1: business1,
      item_type1: item_type1,
      owner1: owner1
    } do
      # Create first active base pricing
      assert {:ok, _} = 
        Pricing |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          item_type_id: item_type1.id,
          pricing_type: :base,
          price_per_day: Decimal.new("100.00"),
          currency: "USD",
          effective_from: Date.utc_today(),
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Attempt to create second active base pricing for same business/item_type
      assert {:error, %Ash.Error.Invalid{}} = 
        Pricing |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          item_type_id: item_type1.id,
          pricing_type: :base,
          price_per_day: Decimal.new("150.00"),
          currency: "USD",
          effective_from: Date.add(Date.utc_today(), 60),
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end
  end

  describe "Business Access Validation" do
    test "business access validation works for employees", %{
      business1: business1,
      business2: business2,
      owner1: owner1
    } do
      # Owner1 can create employee for business1
      assert {:ok, _employee} = 
        Employee |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          email: "employee@business1.com",
          first_name: "Test",
          last_name: "Employee",
          role: :staff,
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Owner1 cannot create employee for business2
      assert {:error, %Ash.Error.Invalid{}} = 
        Employee |> Ash.Changeset.for_create(:create, %{
          business_id: business2.id,  # Different business
          email: "employee@business2.com",
          first_name: "Test",
          last_name: "Employee",
          role: :staff,
          is_active: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end

    test "business access validation works for clients", %{
      business1: business1,
      business2: business2,
      owner1: owner1
    } do
      # Owner1 can create client for business1
      assert {:ok, _client} = 
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          name: "Test Client",
          email: "client@business1.com",
          is_registered: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Owner1 cannot create client for business2
      assert {:error, %Ash.Error.Invalid{}} = 
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business2.id,  # Different business
          name: "Test Client",
          email: "client@business2.com",
          is_registered: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end
  end

  describe "Email and Text Validation" do
    test "email format validation works", %{business1: business1, owner1: owner1} do
      # Valid email
      assert {:ok, _client} = 
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          name: "Valid Client",
          email: "valid@example.com",
          is_registered: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Invalid email format
      assert {:error, %Ash.Error.Invalid{}} = 
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          name: "Invalid Client",
          email: "invalid-email",  # Invalid format
          is_registered: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end

    test "text sanitization works", %{business1: business1, owner1: owner1} do
      # Text with potential XSS should be sanitized
      assert {:ok, client} = 
        Client |> Ash.Changeset.for_create(:create, %{
          business_id: business1.id,
          name: "<script>alert('xss')</script>Clean Name",
          email: "test@example.com",
          is_registered: true
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Name should be sanitized (exact behavior depends on sanitization implementation)
      refute String.contains?(client.name, "<script>")
    end
  end

  describe "Reservation Validation" do
    test "reservation time validation works", %{
      business1: business1,
      section1: section1,
      item_type1: item_type1,
      owner1: owner1
    } do
      # Create client and item for testing
      client = Client |> Ash.Changeset.for_create(:create, %{
        business_id: business1.id,
        name: "Test Client",
        email: "client@test.com",
        is_registered: true
      }) |> Ash.create!(actor: owner1, domain: RivaAsh.Domain)

      item = Item |> Ash.Changeset.for_create(:create, %{
        business_id: business1.id,
        section_id: section1.id,
        item_type_id: item_type1.id,
        name: "Test Item",
        is_active: true
      }) |> Ash.create!(actor: owner1, domain: RivaAsh.Domain)

      # Valid reservation
      start_time = DateTime.utc_now()
      end_time = DateTime.add(start_time, 24 * 60 * 60, :second)

      assert {:ok, _reservation} = 
        Reservation |> Ash.Changeset.for_create(:create, %{
          client_id: client.id,
          item_id: item.id,
          reserved_from: start_time,
          reserved_until: end_time
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)

      # Invalid reservation (end before start)
      assert {:error, %Ash.Error.Invalid{}} = 
        Reservation |> Ash.Changeset.for_create(:create, %{
          client_id: client.id,
          item_id: item.id,
          reserved_from: end_time,
          reserved_until: start_time  # End before start
        }) |> Ash.create(actor: owner1, domain: RivaAsh.Domain)
    end
  end
end
