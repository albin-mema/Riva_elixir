defmodule RivaAsh.Policies.PermissionCheckTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias RivaAsh.Permissions.Constants
  alias RivaAsh.Policies.PermissionCheck

  @moduledoc false

  property "has_permission returns a valid tuple for known permission constants" do
    check all perm <- member_of(Constants.all_permissions()) do
      assert {PermissionCheck, permission: ^perm} = PermissionCheck.has_permission(perm)
    end
  end

  test "has_permission raises on unknown permission" do
    assert_raise ArgumentError, fn ->
      PermissionCheck.has_permission("unknown_permission_flag")
    end
  end
end