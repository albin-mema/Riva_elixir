defmodule RivaAsh.PermissionsTest do
  use ExUnit.Case, async: true
  
  alias RivaAsh.Permissions.Constants
  alias RivaAsh.Policies.PermissionCheck

  describe "Constants module" do
    test "returns all permissions" do
      permissions = Constants.all_permissions()
      
      assert is_list(permissions)
      assert length(permissions) > 0
      assert "can_create_reservations" in permissions
      assert "can_update_pricing" in permissions
    end

    test "returns permissions by category" do
      permissions_by_category = Constants.permissions_by_category()
      
      assert is_map(permissions_by_category)
      assert Map.has_key?(permissions_by_category, :reservations)
      assert Map.has_key?(permissions_by_category, :business)
      assert Map.has_key?(permissions_by_category, :employees)
      
      # Check specific permissions are in correct categories
      assert Constants.can_create_reservations() in permissions_by_category.reservations
      assert Constants.can_update_pricing() in permissions_by_category.business
    end

    test "validates permissions correctly" do
      assert Constants.valid_permission?("can_create_reservations")
      assert Constants.valid_permission?("can_update_pricing")
      refute Constants.valid_permission?("invalid_permission")
      refute Constants.valid_permission?("")
    end

    test "returns permission metadata" do
      metadata = Constants.permission_metadata()
      
      assert is_map(metadata)
      assert Map.has_key?(metadata, "can_create_reservations")
      
      reservation_metadata = metadata["can_create_reservations"]
      assert reservation_metadata.category == :reservations
      assert is_binary(reservation_metadata.description)
    end

    test "permission constants are consistent" do
      # Test that function calls return the expected strings
      assert Constants.can_create_reservations() == "can_create_reservations"
      assert Constants.can_update_pricing() == "can_update_pricing"
      assert Constants.can_view_employees() == "can_view_employees"
    end
  end

  describe "PermissionCheck policy" do
    test "creates correct policy tuples" do
      policy = PermissionCheck.can_create_reservations()
      
      assert {RivaAsh.Policies.PermissionCheck, [permission: "can_create_reservations"]} = policy
    end

    test "validates permission names" do
      # This should not raise an error
      PermissionCheck.has_permission("can_create_reservations")
      
      # This should raise an error for invalid permission
      assert_raise ArgumentError, fn ->
        PermissionCheck.has_permission("invalid_permission")
      end
    end

    test "supports both atom and string permission names" do
      atom_policy = PermissionCheck.has_permission(:can_create_reservations)
      string_policy = PermissionCheck.has_permission("can_create_reservations")
      
      # Both should create equivalent policies
      assert {RivaAsh.Policies.PermissionCheck, [permission: "can_create_reservations"]} = atom_policy
      assert {RivaAsh.Policies.PermissionCheck, [permission: "can_create_reservations"]} = string_policy
    end
  end

  describe "permission categories" do
    test "all permissions have valid categories" do
      permissions_by_category = Constants.permissions_by_category()
      all_permissions = Constants.all_permissions()
      
      # Flatten all permissions from categories
      categorized_permissions = 
        permissions_by_category
        |> Map.values()
        |> List.flatten()
        |> Enum.sort()
      
      # Should match all permissions
      assert Enum.sort(all_permissions) == categorized_permissions
    end

    test "categories match expected structure" do
      categories = Constants.permissions_by_category()
      
      expected_categories = [:reservations, :employees, :business, :clients, :payments, :reports, :system]
      actual_categories = Map.keys(categories) |> Enum.sort()
      
      assert Enum.sort(expected_categories) == actual_categories
    end
  end

  describe "permission metadata consistency" do
    test "metadata covers all permissions" do
      all_permissions = Constants.all_permissions()
      metadata = Constants.permission_metadata()
      
      metadata_permissions = Map.keys(metadata) |> Enum.sort()
      
      assert Enum.sort(all_permissions) == metadata_permissions
    end

    test "metadata categories match permission categories" do
      metadata = Constants.permission_metadata()
      _permissions_by_category = Constants.permissions_by_category()
      
      # Check that each permission's metadata category matches its actual category
      Enum.each(metadata, fn {permission, meta} ->
        actual_category = Constants.category_for_permission(permission)
        assert meta.category == actual_category, 
          "Permission #{permission} has metadata category #{meta.category} but actual category #{actual_category}"
      end)
    end
  end
end
