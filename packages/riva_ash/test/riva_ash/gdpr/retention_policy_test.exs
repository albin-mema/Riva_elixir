defmodule RivaAsh.GDPR.RetentionPolicyTest do
  use RivaAsh.DataCase, async: true

  import RivaAsh.Test.TimeHelpers
  alias RivaAsh.Factory

  describe "boundary times" do
    @spec test_records_exactly_at_threshold_are_handled_correctly :: :ok
    test "records exactly at threshold are handled correctly" do
      # TODO: freeze time and insert records at t-threshold, t-threshold-1s, t-threshold+1s
      # TODO: run retention evaluation and assert only beyond-threshold affected
      assert true
    end
  end

  describe "cascade" do
    @spec test_dependent_records_are_handled_per_policy :: :ok
    test "dependent records are handled per policy" do
      # TODO: ensure dependent/child entities are cascaded or preserved per policy
      assert true
    end
  end

  describe "dry-run" do
    @spec test_reports_candidates_without_deleting :: :ok
    test "reports candidates without deleting" do
      # TODO: invoke dry-run mode and assert no changes while collecting candidate list
      assert true
    end
  end
end
