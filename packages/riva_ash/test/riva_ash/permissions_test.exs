defmodule RivaAsh.PermissionsTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Permissions

  describe "can_access_resource?/2" do
    test "returns true when user has access to resource" do
      user_id = "user-123"
      resource_id = "resource-456"

      assert {:ok, boolean} = Permissions.can_access_resource?(user_id, resource_id)
      assert is_boolean(boolean)
    end

    test "returns false when user does not have access" do
      user_id = "user-123"
      resource_id = "resource-999"

      assert {:ok, false} = Permissions.can_access_resource?(user_id, resource_id)
    end
  end

  describe "check_permission/3" do
    test "returns :ok when user has specific permission" do
      user_id = "user-123"
      resource_id = "resource-456"
      permission = :read

      assert :ok = Permissions.check_permission(user_id, resource_id, permission)
    end

    test "returns {:error, :unauthorized} when permission denied" do
      user_id = "user-123"
      resource_id = "resource-456"
      permission = :delete

      assert {:error, :unauthorized} = Permissions.check_permission(user_id, resource_id, permission)
    end
  end

  describe "list_user_permissions/1" do
    test "returns list of permissions for user" do
      user_id = "user-123"

      assert {:ok, permissions} = Permissions.list_user_permissions(user_id)
      assert is_list(permissions)
    end

    test "returns empty list for user with no permissions" do
      user_id = "user-no-permissions"

      assert {:ok, []} = Permissions.list_user_permissions(user_id)
    end
  end

  describe "grant_permission/3" do
    test "grants permission to user" do
      user_id = "user-123"
      resource_id = "resource-456"
      permission = :write

      assert {:ok, _} = Permissions.grant_permission(user_id, resource_id, permission)
    end

    test "handles duplicate permission grants" do
      user_id = "user-123"
      resource_id = "resource-456"
      permission = :read

      # First grant
      assert {:ok, _} = Permissions.grant_permission(user_id, resource_id, permission)
      # Second grant should not fail
      assert {:ok, _} = Permissions.grant_permission(user_id, resource_id, permission)
    end
  end

  describe "revoke_permission/3" do
    test "revokes permission from user" do
      user_id = "user-123"
      resource_id = "resource-456"
      permission = :write

      assert :ok = Permissions.revoke_permission(user_id, resource_id, permission)
    end

    test "handles revoking non-existent permission" do
      user_id = "user-123"
      resource_id = "resource-456"
      permission = :nonexistent

      assert :ok = Permissions.revoke_permission(user_id, resource_id, permission)
    end
  end

  describe "list_resource_permissions/1" do
    test "returns list of users with access to resource" do
      resource_id = "resource-456"

      assert {:ok, users} = Permissions.list_resource_permissions(resource_id)
      assert is_list(users)
    end

    test "returns empty list for resource with no permissions" do
      resource_id = "resource-no-permissions"

      assert {:ok, []} = Permissions.list_resource_permissions(resource_id)
    end
  end

  describe "check_role_permission/2" do
    test "returns true when role has permission" do
      role = :admin
      permission = :delete

      assert {:ok, boolean} = Permissions.check_role_permission(role, permission)
      assert is_boolean(boolean)
    end

    test "handles unknown roles" do
      role = :unknown_role
      permission = :read

      assert {:ok, false} = Permissions.check_role_permission(role, permission)
    end
  end

  describe "get_permission_hierarchy/0" do
    test "returns permission hierarchy structure" do
      assert {:ok, hierarchy} = Permissions.get_permission_hierarchy()
      assert is_map(hierarchy)
      assert Map.has_key?(hierarchy, :roles)
      assert Map.has_key?(hierarchy, :permissions)
    end
  end

  describe "validate_permission/1" do
    test "validates known permissions" do
      assert :ok = Permissions.validate_permission(:read)
      assert :ok = Permissions.validate_permission(:write)
      assert :ok = Permissions.validate_permission(:delete)
    end

    test "returns error for invalid permissions" do
      assert {:error, :invalid_permission} = Permissions.validate_permission(:invalid_permission)
    end
  end
end
