defmodule RivaAsh.PaperTrailArchivalTest do
  @moduledoc """
  Tests for paper trail functionality with archival (soft delete) operations.
  Ensures audit trails are properly created when resources are archived.
  """

  use RivaAsh.DataCase
  import Ash.Expr
  require Ash.Query
  import RivaAsh.TestHelpers

  alias RivaAsh.Domain
  alias RivaAsh.Resources.{Business, Client, Employee, Permission, Payment, Reservation}

  describe "Paper trail with archival operations" do
    test "archiving a business creates a version record" do
      user = create_user!(%{role: :admin})

      business = create_test_business!(user)
      original_name = business.name

      # Archive the business
      archived_business =
        business
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

      # Check that a version was created
      versions = get_versions_for_resource(Business, business.id)

      assert length(versions) >= 1

      # Find the archive version
      archive_version = Enum.find(versions, &(&1.version_action_name == "archive"))
      assert archive_version, "Should have an archive version"

      # Verify version details
      assert archive_version.version_action_type == "destroy"
      assert archive_version.version_source_id == business.id
      refute is_nil(archive_version.changes)

      # Verify the changes include the archived_at field
      changes = archive_version.changes
      assert Map.has_key?(changes, "archived_at")
      refute is_nil(changes["archived_at"])
    end

    test "paper trail tracks all changes before archival" do
      user = create_user!(%{role: :admin})

      # Create a business
      business = create_test_business!(user)
      original_name = business.name

      # Update the business (this should create a version)
      updated_business =
        business
        |> Ash.Changeset.for_update(:update, %{
          name: "Updated Business Name",
          description: "Updated description"
        })
        |> Ash.update!(actor: user, domain: Domain)

      # Archive the business
      archived_business =
        updated_business
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

      # Check versions
      versions = get_versions_for_resource(Business, business.id)

      # Should have at least 2 versions: update and archive
      assert length(versions) >= 2

      # Verify we have both update and archive versions
      version_actions = Enum.map(versions, & &1.version_action_name)
      assert "update" in version_actions
      assert "archive" in version_actions

      # Verify the update version captured the name change
      update_version = Enum.find(versions, &(&1.version_action_name == "update"))
      assert update_version.changes["name"] == "Updated Business Name"
    end

    test "paper trail works with client archival" do
      user = create_user!(%{role: :admin})
      business = create_test_business!(user)

      client = create_test_client!(business)

      # Archive the client
      archived_client =
        client
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

      # Check that a version was created
      versions = get_versions_for_resource(Client, client.id)

      assert length(versions) >= 1

      archive_version = Enum.find(versions, &(&1.version_action_name == "archive"))
      assert archive_version
      assert archive_version.version_action_type == "destroy"
    end

    test "paper trail works with permission archival" do
      user = create_user!(%{role: :admin})

      permission = create_test_permission!()

      # Archive the permission
      archived_permission =
        permission
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

      # Check that a version was created
      versions = get_versions_for_resource(Permission, permission.id)

      assert length(versions) >= 1

      archive_version = Enum.find(versions, &(&1.version_action_name == "archive"))
      assert archive_version
      assert archive_version.version_action_type == "destroy"
    end

    test "version records contain proper metadata" do
      user = create_user!(%{role: :admin})
      business = create_test_business!(user)

      # Archive with some context
      archived_business =
        business
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

      versions = get_versions_for_resource(Business, business.id)
      archive_version = Enum.find(versions, &(&1.version_action_name == "archive"))

      # Verify metadata
      assert archive_version.version_action_name == "archive"
      assert archive_version.version_action_type == "destroy"
      assert archive_version.version_resource_identifier
      refute is_nil(archive_version.version_inserted_at)
      refute is_nil(archive_version.version_updated_at)

      # Verify action inputs are stored
      refute is_nil(archive_version.version_action_inputs)
    end
  end

  # Helper function to get versions for a resource
  defp get_versions_for_resource(resource, source_id) do
    version_resource = Module.concat(resource, Version)

    version_resource
    |> Ash.Query.filter(version_source_id == ^source_id)
    |> Ash.Query.sort(version_inserted_at: :asc)
    |> Ash.read!(domain: Domain)
  end

  # Helper functions to create test data
  defp create_test_business!(user) do
    Business
    |> Ash.Changeset.for_create(:create, %{
      name: "Test Business #{System.unique_integer()}",
      description: "A test business for paper trail testing"
    })
    |> Ash.create!(actor: user, domain: Domain)
  end

  defp create_test_client!(business) do
    Client
    |> Ash.Changeset.for_create(:create, %{
      business_id: business.id,
      name: "Test Client #{System.unique_integer()}",
      email: "test#{System.unique_integer()}@example.com",
      phone: "+1234567890"
    })
    |> Ash.create!(domain: Domain)
  end

  defp create_test_permission! do
    Permission
    |> Ash.Changeset.for_create(:create, %{
      name: "test_permission_#{System.unique_integer()}",
      description: "A test permission for paper trail testing",
      category: :system
    })
    |> Ash.create!(domain: Domain)
  end
end
