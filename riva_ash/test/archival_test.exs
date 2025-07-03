defmodule RivaAsh.ArchivalTest do
  use ExUnit.Case
  alias RivaAsh.Resources.Business

  describe "AshArchival functionality" do
    test "soft delete sets archived_at timestamp" do
      # Create a business
      business = 
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Test Business",
          address: "123 Test St",
          phone: "555-0123",
          email: "test@business.com"
        })
        |> Ash.create!()

      # Verify it exists and archived_at is nil
      assert business.archived_at == nil

      # Soft delete the business using archive action
      archived_business = 
        business
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!()

      # Verify archived_at is set
      assert archived_business.archived_at != nil
      assert is_struct(archived_business.archived_at, DateTime)
    end

    test "hard delete removes record completely" do
      # Create a business
      business = 
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Test Business 2",
          address: "456 Test Ave",
          phone: "555-0456",
          email: "test2@business.com"
        })
        |> Ash.create!()

      business_id = business.id

      # Hard delete the business using destroy action
      business
      |> Ash.Changeset.for_destroy(:destroy)
      |> Ash.destroy!()

      # Verify the record is completely gone
      assert_raise Ash.Error.Query.NotFound, fn ->
        Business
        |> Ash.Query.filter(id == ^business_id)
        |> Ash.read_one!()
      end
    end

    test "archived records are excluded from normal queries by default" do
      # Create two businesses
      business1 = 
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Active Business",
          address: "789 Active St",
          phone: "555-0789",
          email: "active@business.com"
        })
        |> Ash.create!()

      business2 = 
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "To Archive Business",
          address: "321 Archive Ave",
          phone: "555-0321",
          email: "archive@business.com"
        })
        |> Ash.create!()

      # Archive one business
      business2
      |> Ash.Changeset.for_destroy(:archive)
      |> Ash.destroy!()

      # Normal query should only return the active business
      active_businesses = Business |> Ash.read!()
      
      assert length(active_businesses) >= 1
      assert Enum.any?(active_businesses, fn b -> b.id == business1.id end)
      refute Enum.any?(active_businesses, fn b -> b.id == business2.id end)
    end

    test "can query archived records explicitly" do
      # Create a business
      business = 
        Business
        |> Ash.Changeset.for_create(:create, %{
          name: "Archive Test Business",
          address: "999 Archive St",
          phone: "555-0999",
          email: "archivetest@business.com"
        })
        |> Ash.create!()

      # Archive the business
      archived_business = 
        business
        |> Ash.Changeset.for_destroy(:archive)
        |> Ash.destroy!()

      # Query for archived records explicitly
      archived_records = 
        Business
        |> Ash.Query.filter(not is_nil(archived_at))
        |> Ash.read!()

      assert Enum.any?(archived_records, fn b -> b.id == archived_business.id end)
    end
  end
end
