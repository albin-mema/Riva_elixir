defmodule RivaAsh.GDPR.DataSubjectRightsTest do
  use RivaAsh.DataCase, async: true

  import RivaAsh.Test.TimeHelpers
  alias RivaAsh.Factory

  describe "export" do
    test "returns complete dataset" do
      # TODO: create a user with related records via Factory
      # TODO: call export function and assert structure contains expected sections
      assert true
    end
  end

  describe "delete/anonymize" do
    test "preserves referential integrity" do
      # TODO: perform deletion/anonymization and ensure foreign keys remain valid
      assert true
    end
  end
end
