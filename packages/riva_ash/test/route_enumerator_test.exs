defmodule RouteEnumeratorTest do
  use RivaAsh.DataCase
  
  alias RivaAsh.PropertyTesting.RouteEnumerator
  
  describe "RouteEnumerator" do
    test "enumerates routes successfully" do
      # Test route enumeration
      all_routes = RouteEnumerator.enumerate_routes()
      assert is_map(all_routes)
      assert map_size(all_routes) > 0
      
      # Test parameterized routes
      parameterized_routes = RouteEnumerator.parameterized_routes()
      assert is_list(parameterized_routes)
      
      # Test parameter generation for a few routes
      if length(parameterized_routes) > 0 do
        test_routes = Enum.take(parameterized_routes, 3)
        
        Enum.each(test_routes, fn route ->
          params = RouteEnumerator.generate_route_params(route)
          assert is_map(params)
        end)
      end
    end
    
    test "generates valid route parameters" do
      # Test parameter generation for specific route types
      test_routes = [
        %{path: "/businesses/:id", resource_type: :business, param_types: %{id: :business}},
        %{path: "/clients/:id", resource_type: :client, param_types: %{id: :client}},
        %{path: "/items/:id", resource_type: :item, param_types: %{id: :item}}
      ]
      
      Enum.each(test_routes, fn route ->
        params = RouteEnumerator.generate_route_params(route)
        assert is_map(params)
        assert Map.has_key?(params, :id)
        assert params.id != nil
      end)
    end
    
    test "generates route parameters with existing data" do
      # Test that the route enumerator uses existing data when available
      # by calling the helper functions directly
      business_id = RouteEnumerator.get_existing_business_id()
      client_id = RouteEnumerator.get_existing_client_id()
      item_id = RouteEnumerator.get_existing_item_id()
      user_id = RouteEnumerator.get_existing_user_id()
      
      # These should either return valid IDs or fallback to reasonable defaults
      assert is_integer(business_id) or is_binary(business_id)
      assert is_integer(client_id) or is_binary(client_id)
      assert is_integer(item_id) or is_binary(item_id)
      assert is_integer(user_id) or is_binary(user_id)
    end
  end
end