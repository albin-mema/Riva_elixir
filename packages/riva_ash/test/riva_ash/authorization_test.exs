defmodule RivaAsh.AuthorizationTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Authorization

  describe "authorize_action/3" do
    test "authorizes valid action for user" do
      user_id = "user-123"
      resource = %{id: "resource-456", owner_id: "user-123"}
      action = :read

      assert :ok = Authorization.authorize_action(user_id, resource, action)
    end

    test "denies unauthorized action" do
      user_id = "user-123"
      resource = %{id: "resource-456", owner_id: "user-999"}
      action = :delete

      assert {:error, :unauthorized} = Authorization.authorize_action(user_id, resource, action)
    end
  end

  describe "check_role/2" do
    test "returns true for matching role" do
      user_id = "user-123"
      role = :admin

      assert {:ok, true} = Authorization.check_role(user_id, role)
    end

    test "returns false for non-matching role" do
      user_id = "user-123"
      role = :business_owner

      assert {:ok, false} = Authorization.check_role(user_id, role)
    end
  end

  describe "get_user_roles/1" do
    test "returns list of user roles" do
      user_id = "user-123"

      assert {:ok, roles} = Authorization.get_user_roles(user_id)
      assert is_list(roles)
    end

    test "returns empty list for user with no roles" do
      user_id = "user-no-roles"

      assert {:ok, []} = Authorization.get_user_roles(user_id)
    end
  end

  describe "assign_role/2" do
    test "assigns role to user" do
      user_id = "user-123"
      role = :business_owner

      assert {:ok, _} = Authorization.assign_role(user_id, role)
    end

    test "handles duplicate role assignment" do
      user_id = "user-123"
      role = :admin

      # First assignment
      assert {:ok, _} = Authorization.assign_role(user_id, role)
      # Second assignment should not fail
      assert {:ok, _} = Authorization.assign_role(user_id, role)
    end
  end

  describe "remove_role/2" do
    test "removes role from user" do
      user_id = "user-123"
      role = :business_owner

      assert :ok = Authorization.remove_role(user_id, role)
    end

    test "handles removing non-existent role" do
      user_id = "user-123"
      role = :nonexistent_role

      assert :ok = Authorization.remove_role(user_id, role)
    end
  end

  describe "check_resource_ownership/2" do
    test "returns true when user owns resource" do
      user_id = "user-123"
      resource = %{owner_id: "user-123"}

      assert {:ok, true} = Authorization.check_resource_ownership(user_id, resource)
    end

    test "returns false when user does not own resource" do
      user_id = "user-123"
      resource = %{owner_id: "user-456"}

      assert {:ok, false} = Authorization.check_resource_ownership(user_id, resource)
    end
  end

  describe "get_resource_permissions/2" do
    test "returns permissions for user on resource" do
      user_id = "user-123"
      resource = %{id: "resource-456", owner_id: "user-123"}

      assert {:ok, permissions} = Authorization.get_resource_permissions(user_id, resource)
      assert is_list(permissions)
    end
  end

  describe "validate_action/1" do
    test "validates known actions" do
      assert :ok = Authorization.validate_action(:read)
      assert :ok = Authorization.validate_action(:write)
      assert :ok = Authorization.validate_action(:delete)
      assert :ok = Authorization.validate_action(:create)
    end

    test "returns error for invalid actions" do
      assert {:error, :invalid_action} = Authorization.validate_action(:invalid_action)
    end
  end

  describe "get_authorization_context/2" do
    test "returns authorization context for user and resource" do
      user_id = "user-123"
      resource = %{id: "resource-456", owner_id: "user-123"}

      assert {:ok, context} = Authorization.get_authorization_context(user_id, resource)
      assert is_map(context)
      assert Map.has_key?(context, :user_roles)
      assert Map.has_key?(context, :resource_owner)
    end
  end
end
