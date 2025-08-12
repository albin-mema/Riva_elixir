defmodule RivaAsh.PropertyTesting.DataManagerTest do
  use RivaAsh.DataCase
  alias RivaAsh.PropertyTesting.DataManager

  describe "initialize_test_data/1" do
    test "creates basic test data with proper relationships" do
      test_data = DataManager.initialize_test_data()

      # Verify that basic data types are created
      assert is_list(test_data.users)
      assert is_list(test_data.businesses)
      assert is_list(test_data.clients)
      assert is_list(test_data.items)

      # Verify that we have some data created
      assert length(test_data.users) > 0
      assert length(test_data.businesses) > 0
      assert length(test_data.clients) > 0
      assert length(test_data.items) > 0
    end
  end

  describe "get_random_id/1" do
    test "returns valid IDs for all resource types" do
      # Initialize test data first
      DataManager.initialize_test_data()

      # Test all supported resource types
      resource_types = [
        :user, :business, :client, :item, :employee,
        :plot, :section, :item_type, :layout, :item_position,
        :pricing, :payment, :reservation, :availability_exception,
        :recurring_reservation, :recurring_reservation_instance
      ]

      Enum.each(resource_types, fn resource_type ->
        id = DataManager.get_random_id(resource_type)
        assert is_integer(id) and id > 0
      end)
    end
  end

  describe "generate_realistic_data/2" do
    test "generates realistic data for all resource types" do
      resource_types = [
        :user, :business, :client, :item, :employee,
        :plot, :section, :item_type, :layout, :item_position,
        :pricing, :payment, :reservation, :availability_exception,
        :recurring_reservation, :recurring_reservation_instance
      ]

      Enum.each(resource_types, fn resource_type ->
        data = DataManager.generate_realistic_data(resource_type)
        assert is_map(data)
        assert map_size(data) > 0
      end)
    end
  end

  describe "cleanup_test_data/0" do
    test "cleans up test data properly" do
      # Initialize basic test data
      test_data = DataManager.initialize_test_data()
      
      # Verify data exists
      assert length(test_data.users) > 0
      
      # Clean up all test data
      DataManager.cleanup_all_test_data()
      
      # Verify cleanup completed (process should be clean)
      assert Process.get(:test_data_refs, []) == []
    end
  end
end