defmodule RivaAsh.Reactors.BusinessSetupFlowTest do
  use RivaAsh.DataCase, async: true

  alias RivaAsh.Reactors.BusinessSetupFlow
  alias RivaAsh.Resources.{Business, Plot, Layout, Section, ItemType, Pricing}

  describe "BusinessSetupFlow" do
    @tag :skip
    test "successfully sets up a complete business with plot and initial configuration" do
      # Create a user to be the business owner
      user = RivaAsh.Accounts.User
             |> Ash.Changeset.for_create(:register_with_password, %{
               name: "Test User",
               email: "owner@example.com",
               password: "password123",
               role: :admin
             })
             |> Ash.create!(domain: RivaAsh.Accounts)

      # Define inputs for the reactor
      inputs = %{
        business_info: %{
          name: "Test Fairground",
          description: "A test fairground for reactor testing",
          item_types: [
            %{name: "Food Stall", description: "Food vendor stall", color: "#FF6B6B"},
            %{name: "Game Booth", description: "Carnival game booth", color: "#4ECDC4"}
          ],
          pricing: %{
            default_daily_rate: "75.00",
            currency: "USD"
          },
          item_pricing: %{
            "Food Stall" => "100.00",
            "Game Booth" => "60.00"
          }
        },
        plot_details: %{
          name: "Main Fairground",
          description: "Primary fairground area",
          address: "123 Fair Street, Fairtown, ST 12345",
          total_area: "5000",
          area_unit: "sqft",
          grid_rows: 8,
          grid_columns: 12,
          coordinates: %{
            "lat" => 40.7128,
            "lng" => -74.0060
          },
          sections: [
            %{name: "Food Court", description: "Area for food vendors"},
            %{name: "Games Area", description: "Area for carnival games"}
          ]
        },
        owner_id: user.id
      }

      # Run the reactor
      assert {:ok, result} = Reactor.run(BusinessSetupFlow, inputs)

      # Verify the business was created
      assert result.business.name == "Test Fairground"
      assert result.business.owner_id == user.id

      # Verify the plot was created
      assert result.plot.name == "Main Fairground"
      assert result.plot.business_id == result.business.id
      assert result.plot.total_area == Decimal.new("5000")

      # Verify the layout was created
      assert result.layout.name == "Main Layout"
      assert result.layout.plot_id == result.plot.id
      assert result.layout.grid_rows == 8
      assert result.layout.grid_columns == 12

      # Verify sections were created
      assert length(result.sections) == 2
      section_names = Enum.map(result.sections, & &1.name)
      assert "Food Court" in section_names
      assert "Games Area" in section_names

      # Verify item types were created
      assert length(result.item_types) == 2
      item_type_names = Enum.map(result.item_types, & &1.name)
      assert "Food Stall" in item_type_names
      assert "Game Booth" in item_type_names

      # Verify pricing was set up
      assert length(result.pricing_rules) == 2

      # Check specific pricing
      food_stall_type = Enum.find(result.item_types, &(&1.name == "Food Stall"))
      food_stall_pricing = Enum.find(result.pricing_rules, &(&1.item_type_id == food_stall_type.id))
      assert food_stall_pricing.price_per_day == Decimal.new("100.00")

      game_booth_type = Enum.find(result.item_types, &(&1.name == "Game Booth"))
      game_booth_pricing = Enum.find(result.pricing_rules, &(&1.item_type_id == game_booth_type.id))
      assert game_booth_pricing.price_per_day == Decimal.new("60.00")

      # Verify result structure
      assert result.status == :completed
      assert %DateTime{} = result.setup_completed_at
    end

    @tag :skip
    test "handles failure and rolls back properly" do
      # Create a user to be the business owner
      user = RivaAsh.Accounts.User
             |> Ash.Changeset.for_create(:register_with_password, %{
               name: "Test User 2",
               email: "owner2@example.com",
               password: "password123",
               role: :admin
             })
             |> Ash.create!(domain: RivaAsh.Accounts)

      # Define inputs that will cause a failure (invalid grid size)
      inputs = %{
        business_info: %{
          name: "Test Business",
          description: "A test business for failure testing"
        },
        plot_details: %{
          name: "Test Plot",
          description: "A test plot",
          grid_rows: 0, # This should cause validation failure
          grid_columns: 10
        },
        owner_id: user.id
      }

      # Run the reactor and expect failure
      assert {:error, _reason} = Reactor.run(BusinessSetupFlow, inputs)

      # Verify no resources were left behind
      assert Ash.count!(Business, domain: RivaAsh.Domain) == 0
      assert Ash.count!(Plot, domain: RivaAsh.Domain) == 0
      assert Ash.count!(Layout, domain: RivaAsh.Domain) == 0
      assert Ash.count!(Section, domain: RivaAsh.Domain) == 0
      assert Ash.count!(ItemType, domain: RivaAsh.Domain) == 0
      assert Ash.count!(Pricing, domain: RivaAsh.Domain) == 0
    end

    @tag :skip
    test "creates default configuration when minimal inputs provided" do
      # Create a user to be the business owner
      user = RivaAsh.Accounts.User
             |> Ash.Changeset.for_create(:register_with_password, %{
               name: "Test User 3",
               email: "owner3@example.com",
               password: "password123",
               role: :admin
             })
             |> Ash.create!(domain: RivaAsh.Accounts)

      # Define minimal inputs
      inputs = %{
        business_info: %{
          name: "Minimal Business",
          description: "A business with minimal configuration"
        },
        plot_details: %{
          name: "Simple Plot",
          description: "A simple plot"
        },
        owner_id: user.id
      }

      # Run the reactor
      assert {:ok, result} = Reactor.run(BusinessSetupFlow, inputs)

      # Verify defaults were applied
      assert result.layout.grid_rows == 10  # Default grid size
      assert result.layout.grid_columns == 10

      # Verify default section was created
      assert length(result.sections) == 1
      assert hd(result.sections).name == "Main Section"

      # Verify default item types were created
      assert length(result.item_types) == 2
      item_type_names = Enum.map(result.item_types, & &1.name)
      assert "Standard Spot" in item_type_names
      assert "Premium Spot" in item_type_names

      # Verify default pricing was applied
      assert length(result.pricing_rules) == 2

      # All pricing should use default rate
      Enum.each(result.pricing_rules, fn pricing ->
        assert pricing.price_per_day == Decimal.new("50.00")
        assert pricing.currency == "USD"
      end)
    end
  end
end
