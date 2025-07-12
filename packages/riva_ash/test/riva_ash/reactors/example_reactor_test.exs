defmodule RivaAsh.Reactors.ExampleReactorTest do
  use ExUnit.Case, async: true

  alias RivaAsh.Reactors.ExampleReactor
  alias RivaAsh.Resources.{Business, Section, Item}

  describe "ExampleReactor" do
    test "successfully creates business, section, and item" do
      inputs = %{
        business_name: "Test Business",
        business_description: "A test business for reactor demo",
        section_name: "Test Section",
        section_description: "A test section for reactor demo",
        item_name: "Test Item",
        item_description: "A test item for reactor demo",
        item_capacity: 5
      }

      # Run the reactor
      assert {:ok, item} = Reactor.run(ExampleReactor, inputs)

      # Verify the item was created
      assert item.name == "Test Item"
      assert item.description == "A test item for reactor demo"
      assert item.capacity == 5

      # Verify the section was created and linked
      section = Ash.load!(item, :section).section
      assert section.name == "Test Section"
      assert section.description == "A test section for reactor demo"

      # Verify the business was created and linked
      business = Ash.load!(section, :business).business
      assert business.name == "Test Business"
      assert business.description == "A test business for reactor demo"
    end

    test "handles failure and rolls back properly" do
      inputs = %{
        business_name: "Test Business",
        business_description: "A test business for reactor demo",
        section_name: "Test Section",
        section_description: "A test section for reactor demo",
        # This should cause a failure
        item_name: nil,
        item_description: "A test item for reactor demo",
        item_capacity: 5
      }

      # Run the reactor and expect failure
      assert {:error, _reason} = Reactor.run(ExampleReactor, inputs)

      # Verify no resources were left behind
      assert Ash.count!(Business) == 0
      assert Ash.count!(Section) == 0
      assert Ash.count!(Item) == 0
    end
  end

  describe "using reactor via resource action" do
    test "can run reactor through Business.create_complete_setup action" do
      inputs = %{
        business_name: "Action Business",
        business_description: "A business created via action",
        section_name: "Action Section",
        section_description: "A section created via action",
        item_name: "Action Item",
        item_description: "An item created via action",
        item_capacity: 3
      }

      # Run the reactor via the resource action
      assert {:ok, item} = Ash.run_action(Business, :create_complete_setup, inputs, domain: RivaAsh.Domain)

      # Verify the item was created
      assert item.name == "Action Item"
      assert item.capacity == 3
    end
  end
end
