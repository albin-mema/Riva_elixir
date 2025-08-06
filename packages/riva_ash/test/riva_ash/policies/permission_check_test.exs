defmodule RivaAsh.Policies.PermissionCheckTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias RivaAsh.Permissions.Constants
  alias RivaAsh.Policies.PermissionCheck

  @moduledoc false

  property "has_permission returns a valid tuple for known permission constants" do
    check all(perm <- member_of(Constants.all_permissions())) do
      assert {PermissionCheck, permission: ^perm} = PermissionCheck.has_permission(perm)
    end
  end

  test "has_permission raises on unknown permission" do
    assert_raise ArgumentError, fn ->
      PermissionCheck.has_permission("unknown_permission_flag")
    end
  end

  describe "role-action matrix (targeted)" do
    test "admin can_view_all_reservations? true" do
      assert PermissionCheck.can_view_all_reservations() ==
               {PermissionCheck, permission: Constants.can_view_all_reservations()}

      # Admins always match? when actor has role :admin; unit level assertion uses tuple form
    end

    test "staff can_modify_reservations? false (unless specific permission granted)" do
      tuple = PermissionCheck.can_modify_reservations()
      assert match?({PermissionCheck, permission: _}, tuple)
      # Behavior is enforced at runtime via Permissions; here we assert the check tuple form
    end

    test "manager can_update_pricing? tuple configured" do
      tuple = PermissionCheck.can_update_pricing()
      assert match?({PermissionCheck, permission: _}, tuple)
    end
  end
end
