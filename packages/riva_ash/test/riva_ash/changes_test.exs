defmodule RivaAsh.ChangesTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Changes

  describe "track_change/3" do
    test "tracks change with valid params" do
      resource_id = "resource-123"
      change_type = :update
      changes = %{name: "New Name", description: "Updated description"}

      assert {:ok, change} = Changes.track_change(resource_id, change_type, changes)
      assert change.resource_id == resource_id
      assert change.change_type == :update
    end

    test "returns error with invalid params" do
      resource_id = nil
      change_type = :invalid_type
      changes = %{}

      assert {:error, _changeset} = Changes.track_change(resource_id, change_type, changes)
    end
  end

  describe "get_change_history/1" do
    test "returns change history for resource" do
      resource_id = "resource-123"

      assert {:ok, changes} = Changes.get_change_history(resource_id)
      assert is_list(changes)
    end

    test "returns empty list for resource with no changes" do
      resource_id = "resource-no-changes"

      assert {:ok, []} = Changes.get_change_history(resource_id)
    end
  end

  describe "get_recent_changes/2" do
    test "returns recent changes for resource" do
      resource_id = "resource-123"
      limit = 10

      assert {:ok, changes} = Changes.get_recent_changes(resource_id, limit)
      assert is_list(changes)
      assert length(changes) <= limit
    end
  end

  describe "revert_change/1" do
    test "reverts specified change" do
      change_id = "change-123"

      assert {:ok, reverted_change} = Changes.revert_change(change_id)
      assert reverted_change.reverted_at != nil
    end

    test "returns error for non-existent change" do
      change_id = "nonexistent"

      assert {:error, :not_found} = Changes.revert_change(change_id)
    end
  end

  describe "get_changes_by_type/2" do
    test "returns changes filtered by type" do
      resource_id = "resource-123"
      change_type = :update

      assert {:ok, changes} = Changes.get_changes_by_type(resource_id, change_type)
      assert is_list(changes)

      Enum.each(changes, fn change ->
        assert change.change_type == :update
      end)
    end
  end

  describe "get_changes_by_date_range/3" do
    test "returns changes within date range" do
      resource_id = "resource-123"
      start_date = ~D[2024-01-01]
      end_date = ~D[2024-01-31]

      assert {:ok, changes} = Changes.get_changes_by_date_range(resource_id, start_date, end_date)
      assert is_list(changes)
    end
  end

  describe "get_changes_by_user/2" do
    test "returns changes made by specific user" do
      user_id = "user-123"
      resource_id = "resource-456"

      assert {:ok, changes} = Changes.get_changes_by_user(resource_id, user_id)
      assert is_list(changes)
    end
  end

  describe "create_change_summary/1" do
    test "creates summary of changes for resource" do
      resource_id = "resource-123"

      assert {:ok, summary} = Changes.create_change_summary(resource_id)
      assert is_map(summary)
      assert Map.has_key?(summary, :total_changes)
      assert Map.has_key?(summary, :recent_changes)
    end
  end

  describe "validate_change_data/2" do
    test "validates change data against schema" do
      change_type = :update
      changes = %{name: "Valid Name", description: "Valid description"}

      assert :ok = Changes.validate_change_data(change_type, changes)
    end

    test "returns error for invalid change data" do
      change_type = :update
      changes = %{name: nil, description: ""}

      assert {:error, _} = Changes.validate_change_data(change_type, changes)
    end
  end

  describe "get_change_impact/1" do
    test "calculates impact of change" do
      change_id = "change-123"

      assert {:ok, impact} = Changes.get_change_impact(change_id)
      assert is_map(impact)
      assert Map.has_key?(impact, :affected_fields)
      assert Map.has_key?(impact, :severity)
    end
  end

  describe "bulk_track_changes/2" do
    test "tracks multiple changes" do
      resource_id = "resource-123"

      changes_list = [
        %{type: :create, changes: %{name: "Item 1"}},
        %{type: :update, changes: %{name: "Item 2"}}
      ]

      assert {:ok, changes} = Changes.bulk_track_changes(resource_id, changes_list)
      assert is_list(changes)
      assert length(changes) == 2
    end
  end

  describe "get_change_statistics/1" do
    test "returns change statistics for resource" do
      resource_id = "resource-123"

      assert {:ok, stats} = Changes.get_change_statistics(resource_id)
      assert is_map(stats)
      assert Map.has_key?(stats, :total_changes)
      assert Map.has_key?(stats, :changes_by_type)
      assert Map.has_key?(stats, :most_active_user)
    end
  end
end
