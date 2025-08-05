defmodule RivaAsh.ArchivalTest do
  @moduledoc """
  Test module for Ash archival functionality.
  Tests soft delete and archival behavior for business resources.
  """

  use RivaAsh.DataCase
  import Ash.Expr
  require Ash.Query
  import RivaAsh.TestHelpers
  alias RivaAsh.Resources.Business
  alias RivaAsh.Domain

  @doc """
  Tests that soft delete sets archived_at timestamp correctly.
  """
  @spec test_soft_delete_sets_archived_at :: :ok
  test "soft delete sets archived_at timestamp" do
    # Create a user to act as the business owner
    user = create_user!(%{role: :admin})

    # Create a business directly using Ash.create! with actor
    business =
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Test Business",
        description: "A test business for archival testing"
      })
      |> Ash.create!(actor: user, domain: Domain)

    # Verify the business was created and not archived
    assert business.name == "Test Business"
    assert is_nil(business.archived_at)

    # Archive the business using the archive action
    archived_business =
      business
      |> Ash.Changeset.for_destroy(:archive)
      |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

    # Verify archived_at is set
    refute is_nil(archived_business.archived_at)
    assert %DateTime{} = archived_business.archived_at
  end

  @doc """
  Tests that archived records are excluded from normal queries by default.
  """
  @spec test_archived_records_excluded_from_normal_queries :: :ok
  test "archived records are excluded from normal queries by default" do
    # Create a user to act as the business owner
    user = create_user!(%{role: :admin})

    # Create two businesses
    business1 =
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Active Business",
        description: "This business should remain active"
      })
      |> Ash.create!(actor: user, domain: Domain)

    business2 =
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Business to Archive",
        description: "This business will be archived"
      })
      |> Ash.create!(actor: user, domain: Domain)

    # Archive the second business
    business2
    |> Ash.Changeset.for_destroy(:archive)
    |> Ash.destroy!(actor: user, domain: Domain)

    # Normal query should only return the active business
    active_businesses = Business |> Ash.read!(actor: user, domain: Domain)

    assert length(active_businesses) >= 1
    assert Enum.any?(active_businesses, fn b -> b.id == business1.id end)
    refute Enum.any?(active_businesses, fn b -> b.id == business2.id end)
  end

  @doc """
  Tests that archived records can be queried explicitly.
  """
  @spec test_can_query_archived_records_explicitly :: :ok
  test "can query archived records explicitly" do
    # Create a user to act as the business owner
    user = create_user!(%{role: :admin})

    # Create a business
    business =
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Archive Test Business",
        description: "A business for testing explicit archived record queries"
      })
      |> Ash.create!(actor: user, domain: Domain)

    # Archive the business
    archived_business =
      business
      |> Ash.Changeset.for_destroy(:archive)
      |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

    # Query for archived records explicitly using the base query without filter
    # Since archived records are excluded by default, we need to explicitly include them
    all_records =
      Business
      |> Ash.Query.unset([:filter])
      |> Ash.read!(actor: user, domain: Domain)

    # Verify our archived business is in the results
    assert Enum.any?(all_records, fn b ->
             b.id == archived_business.id && not is_nil(b.archived_at)
           end)
  end

  @doc """
  Tests that hard delete removes record completely.
  """
  @spec test_hard_delete_removes_record_completely :: :ok
  test "hard delete removes record completely" do
    # Create a user to act as the business owner
    user = create_user!(%{role: :admin})

    # Create a business
    business =
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Business to Delete",
        description: "This business will be permanently deleted"
      })
      |> Ash.create!(actor: user, domain: Domain)

    business_id = business.id

    # Check if hard delete action exists, if not skip this test
    actions = Ash.Resource.Info.actions(Business)
    destroy_actions = Enum.filter(actions, &(&1.type == :destroy))

    if Enum.any?(destroy_actions, &(&1.name == :destroy_permanently)) do
      # Hard delete the business
      business
      |> Ash.Changeset.for_destroy(:destroy_permanently)
      |> Ash.destroy!(actor: user, domain: Domain)

      # Verify the record is completely gone (not just archived)
      all_records =
        Business
        |> Ash.Query.unset([:filter])
        |> Ash.read!(actor: user, domain: Domain)

      refute Enum.any?(all_records, fn b -> b.id == business_id end)
    else
      # If no hard delete action exists, use regular destroy which should hard delete
      business
      |> Ash.Changeset.for_destroy(:destroy)
      |> Ash.destroy!(actor: user, domain: Domain)

      # Check that it's gone from all records
      all_records =
        Business
        |> Ash.Query.unset([:filter])
        |> Ash.read!(actor: user, domain: Domain)

      refute Enum.any?(all_records, fn b -> b.id == business_id end)
    end
  end
end
    test "soft delete sets archived_at timestamp" do
      # Create a user to act as the business owner
      user = create_user!(%{role: :admin})

      # Create a business directly using Ash.create! with actor
      business =
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Test Business",
          description: "A test business for archival testing"
        })
        |> Ash.create!(actor: user, domain: Domain)

      # Verify the business was created and not archived
      assert business.name == "Test Business"
      assert is_nil(business.archived_at)

      # Archive the business using the archive action
      archived_business =
        business
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

      # Verify archived_at is set
      refute is_nil(archived_business.archived_at)
      assert %DateTime{} = archived_business.archived_at
    end

    test "archived records are excluded from normal queries by default" do
      # Create a user to act as the business owner
      user = create_user!(%{role: :admin})

      # Create two businesses
      business1 =
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Active Business",
          description: "This business should remain active"
        })
        |> Ash.create!(actor: user, domain: Domain)

      business2 =
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Business to Archive",
          description: "This business will be archived"
        })
        |> Ash.create!(actor: user, domain: Domain)

      # Archive the second business
      business2
      |> Ash.Changeset.for_destroy(:archive)
      |> Ash.destroy!(actor: user, domain: Domain)

      # Normal query should only return the active business
      active_businesses = Business |> Ash.read!(actor: user, domain: Domain)

      assert length(active_businesses) >= 1
      assert Enum.any?(active_businesses, fn b -> b.id == business1.id end)
      refute Enum.any?(active_businesses, fn b -> b.id == business2.id end)
    end

    test "can query archived records explicitly" do
      # Create a user to act as the business owner
      user = create_user!(%{role: :admin})

      # Create a business
      business =
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Archive Test Business",
          description: "A business for testing explicit archived record queries"
        })
        |> Ash.create!(actor: user, domain: Domain)

      # Archive the business
      archived_business =
        business
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

      # Query for archived records explicitly using the base query without filter
      # Since archived records are excluded by default, we need to explicitly include them
      all_records =
        Business
        |> Ash.Query.unset([:filter])
        |> Ash.read!(actor: user, domain: Domain)

      # Verify our archived business is in the results
      assert Enum.any?(all_records, fn b ->
               b.id == archived_business.id && not is_nil(b.archived_at)
             end)
    end

    test "hard delete removes record completely" do
      # Create a user to act as the business owner
      user = create_user!(%{role: :admin})

      # Create a business
      business =
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Business to Delete",
          description: "This business will be permanently deleted"
        })
        |> Ash.create!(actor: user, domain: Domain)

      business_id = business.id

      # Check if hard delete action exists, if not skip this test
      actions = Ash.Resource.Info.actions(Business)
      destroy_actions = Enum.filter(actions, &(&1.type == :destroy))

      if Enum.any?(destroy_actions, &(&1.name == :destroy_permanently)) do
        # Hard delete the business
        business
        |> Ash.Changeset.for_destroy(:destroy_permanently)
        |> Ash.destroy!(actor: user, domain: Domain)

        # Verify the record is completely gone (not just archived)
        all_records =
          Business
          |> Ash.Query.unset([:filter])
          |> Ash.read!(actor: user, domain: Domain)

        refute Enum.any?(all_records, fn b -> b.id == business_id end)
      else
        # If no hard delete action exists, use regular destroy which should hard delete
        business
        |> Ash.Changeset.for_destroy(:destroy)
        |> Ash.destroy!(actor: user, domain: Domain)

        # Check that it's gone from all records
        all_records =
          Business
          |> Ash.Query.unset([:filter])
          |> Ash.read!(actor: user, domain: Domain)

        refute Enum.any?(all_records, fn b -> b.id == business_id end)
      end
    end
  end
end
