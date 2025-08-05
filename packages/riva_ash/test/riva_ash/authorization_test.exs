defmodule RivaAsh.AuthorizationTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Authorization

  describe "authorize_action/3" do
    @spec test_authorizes_valid_action_for_user :: :ok
    test "authorizes valid action for user" do
      user_id = "user-123"
      resource = %{id: "resource-456", owner_id: "user-123"}
      action = :read

      assert :ok = Authorization.authorize_action(user_id, resource, action)
    end

    @spec test_denies_unauthorized_action :: :ok
    test "denies unauthorized action" do
      user_id = "user-123"
      resource = %{id: "resource-456", owner_id: "user-999"}
      action = :delete

      assert {:error, :unauthorized} = Authorization.authorize_action(user_id, resource, action)
    end
  end

  describe "check_role/2" do
    @spec test_returns_true_for_matching_role :: :ok
    test "returns true for matching role" do
      user_id = "user-123"
      role = :admin

      assert {:ok, true} = Authorization.check_role(user_id, role)
    end

    @spec test_returns_false_for_non_matching_role :: :ok
    test "returns false for non-matching role" do
      user_id = "user-123"
      role = :business_owner

      assert {:ok, false} = Authorization.check_role(user_id, role)
    end
  end

  describe "get_user_roles/1" do
    @spec test_returns_list_of_user_roles :: :ok
    test "returns list of user roles" do
      user_id = "user-123"

      assert {:ok, roles} = Authorization.get_user_roles(user_id)
      assert is_list(roles)
    end

    @spec test_returns_empty_list_for_user_with_no_roles :: :ok
    test "returns empty list for user with no roles" do
      user_id = "user-no-roles"

      assert {:ok, []} = Authorization.get_user_roles(user_id)
    end
  end

  describe "assign_role/2" do
    @spec test_assigns_role_to_user :: :ok
    test "assigns role to user" do
      user_id = "user-123"
      role = :business_owner

      assert {:ok, _} = Authorization.assign_role(user_id, role)
    end

    @spec test_handles_duplicate_role_assignment :: :ok
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
    @spec test_removes_role_from_user :: :ok
    test "removes role from user" do
      user_id = "user-123"
      role = :business_owner

      assert :ok = Authorization.remove_role(user_id, role)
    end

    @spec test_handles_removing_non_existent_role :: :ok
    test "handles removing non-existent role" do
      user_id = "user-123"
      role = :nonexistent_role

      assert :ok = Authorization.remove_role(user_id, role)
    end
  end

  describe "check_resource_ownership/2" do
    @spec test_returns_true_when_user_owns_resource :: :ok
    test "returns true when user owns resource" do
      user_id = "user-123"
      resource = %{owner_id: "user-123"}

      assert {:ok, true} = Authorization.check_resource_ownership(user_id, resource)
    end

    @spec test_returns_false_when_user_does_not_own_resource :: :ok
    test "returns false when user does not own resource" do
      user_id = "user-123"
      resource = %{owner_id: "user-456"}

      assert {:ok, false} = Authorization.check_resource_ownership(user_id, resource)
    end
  end

  describe "get_resource_permissions/2" do
    @spec test_returns_permissions_for_user_on_resource :: :ok
    test "returns permissions for user on resource" do
      user_id = "user-123"
      resource = %{id: "resource-456", owner_id: "user-123"}

      assert {:ok, permissions} = Authorization.get_resource_permissions(user_id, resource)
      assert is_list(permissions)
    end
  end

  describe "validate_action/1" do
    @spec test_validates_known_actions :: :ok
    test "validates known actions" do
      assert :ok = Authorization.validate_action(:read)
      assert :ok = Authorization.validate_action(:write)
      assert :ok = Authorization.validate_action(:delete)
      assert :ok = Authorization.validate_action(:create)
    end

    @spec test_returns_error_for_invalid_actions :: :ok
    test "returns error for invalid actions" do
      assert {:error, :invalid_action} = Authorization.validate_action(:invalid_action)
    end
  end

  describe "get_authorization_context/2" do
    @spec test_returns_authorization_context_for_user_and_resource :: :ok
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
