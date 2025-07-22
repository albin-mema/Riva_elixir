defmodule RivaAsh.ResourceHelpersTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.ResourceHelpers

  describe "get_resource_by_id/2" do
    test "returns resource when found" do
      # This would need actual resource setup
      # For now, testing the interface
      assert {:ok, _resource} = ResourceHelpers.get_resource_by_id("resource-type", "123")
    end

    test "returns error when resource not found" do
      assert {:error, :not_found} = ResourceHelpers.get_resource_by_id("resource-type", "nonexistent")
    end
  end

  describe "filter_resources/3" do
    test "filters resources by criteria" do
      criteria = %{status: "active"}
      assert {:ok, resources} = ResourceHelpers.filter_resources("resource-type", criteria)
      assert is_list(resources)
    end

    test "handles empty criteria" do
      assert {:ok, resources} = ResourceHelpers.filter_resources("resource-type", %{})
      assert is_list(resources)
    end
  end

  describe "update_resource/3" do
    test "updates resource with valid attributes" do
      attrs = %{name: "Updated Name"}
      assert {:ok, _resource} = ResourceHelpers.update_resource("resource-type", "123", attrs)
    end

    test "returns error with invalid attributes" do
      attrs = %{name: ""}
      assert {:error, _changeset} = ResourceHelpers.update_resource("resource-type", "123", attrs)
    end
  end

  describe "delete_resource/2" do
    test "soft deletes resource" do
      assert {:ok, _resource} = ResourceHelpers.delete_resource("resource-type", "123")
    end

    test "handles non-existent resource" do
      assert {:error, :not_found} = ResourceHelpers.delete_resource("resource-type", "nonexistent")
    end
  end

  describe "list_resources/2" do
    test "lists resources with pagination" do
      opts = %{limit: 10, offset: 0}
      assert {:ok, resources} = ResourceHelpers.list_resources("resource-type", opts)
      assert is_list(resources)
    end

    test "handles empty list" do
      opts = %{limit: 10, offset: 0}
      assert {:ok, []} = ResourceHelpers.list_resources("empty-resource-type", opts)
    end
  end

  describe "count_resources/1" do
    test "returns count of resources" do
      assert {:ok, count} = ResourceHelpers.count_resources("resource-type")
      assert is_integer(count)
    end

    test "returns 0 for non-existent resource type" do
      assert {:ok, 0} = ResourceHelpers.count_resources("nonexistent-type")
    end
  end

  describe "validate_resource/2" do
    test "returns valid changeset for valid attributes" do
      attrs = %{name: "Valid Name", status: "active"}
      assert {:ok, _changeset} = ResourceHelpers.validate_resource("resource-type", attrs)
    end

    test "returns invalid changeset for invalid attributes" do
      attrs = %{name: ""}
      assert {:error, _changeset} = ResourceHelpers.validate_resource("resource-type", attrs)
    end
  end
end
