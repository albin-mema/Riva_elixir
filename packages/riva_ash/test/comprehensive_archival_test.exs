defmodule RivaAsh.ComprehensiveArchivalTest do
  @moduledoc """
  Comprehensive tests for soft delete (archival) functionality across all resources.
  Tests standardized archival behavior for consistency.
  """

  use RivaAsh.DataCase
  import Ash.Expr
  require Ash.Query

  alias RivaAsh.Domain

  alias RivaAsh.Resources.{
    Business,
    Client,
    Employee,
    Item,
    ItemType,
    Section,
    Plot,
    Layout,
    ItemPosition,
    ItemHold,
    ItemSchedule,
    AvailabilityException,
    Reservation,
    Payment,
    Pricing,
    Permission,
    EmployeePermission,
    RecurringReservation,
    RecurringReservationInstance
  }

  # List of resources that should have archival functionality
  @archival_resources [
    {Business, :business},
    {Client, :client},
    {Employee, :employee},
    {Item, :item},
    {ItemType, :item_type},
    {Section, :section},
    {Plot, :plot},
    {Layout, :layout},
    {ItemPosition, :item_position},
    {ItemHold, :item_hold},
    {ItemSchedule, :item_schedule},
    {AvailabilityException, :availability_exception},
    {Reservation, :reservation},
    {Payment, :payment},
    {Pricing, :pricing},
    {Permission, :permission},
    {EmployeePermission, :employee_permission},
    {RecurringReservation, :recurring_reservation},
    {RecurringReservationInstance, :recurring_reservation_instance}
  ]

  @doc """
  Tests that all archival resources have archived_at attribute.
  """
  test "all archival resources have archived_at attribute" do
    for {resource, _name} <- @archival_resources do
      attributes = Ash.Resource.Info.attributes(resource)
      archived_at_attr = Enum.find(attributes, &(&1.name == :archived_at))

      assert archived_at_attr, "#{resource} should have archived_at attribute"

      assert archived_at_attr.type == :utc_datetime_usec,
             "#{resource} archived_at should be utc_datetime_usec"

      assert archived_at_attr.allow_nil?,
             "#{resource} archived_at should allow nil"
    end
  end

  @doc """
  Tests that all archival resources have archive action.
  """
  test "all archival resources have archive action" do
    for {resource, _name} <- @archival_resources do
      actions = Ash.Resource.Info.actions(resource)
      archive_action = Enum.find(actions, &(&1.name == :archive && &1.type == :destroy))

      assert archive_action, "#{resource} should have archive destroy action"
    end
  end

  @doc """
  Tests that all archival resources exclude archived records by default.
  """
  test "all archival resources exclude archived records by default" do
    user = create_user!(%{role: :admin})

    # Test with Business as representative example
    business = create_test_business!(user)

    # Create and archive a business
    archived_business =
      business
      |> Ash.Changeset.for_destroy(:archive)
      |> Ash.destroy!(actor: user, domain: Domain, return_destroyed?: true)

    # Normal query should exclude archived records
    active_businesses = Business |> Ash.read!(actor: user, domain: Domain)
    refute Enum.any?(active_businesses, &(&1.id == archived_business.id))

    # Query without filter should include archived records
    all_businesses =
      Business
      |> Ash.Query.unset([:filter])
      |> Ash.read!(actor: user, domain: Domain)

    assert Enum.any?(all_businesses, &(&1.id == archived_business.id))
  end

  describe "Resource-specific archival tests" do
    test "Business archival functionality" do
      user = create_user!(%{role: :admin})
      business = create_test_business!(user)

      test_archival_behavior(Business, business, user)
    end

    test "Client archival functionality" do
      user = create_user!(%{role: :admin})
      business = create_test_business!(user)

      client = create_test_client!(business)
      test_archival_behavior(Client, client, user)
    end

    test "Permission archival functionality" do
      user = create_user!(%{role: :admin})

      permission = create_test_permission!()
      test_archival_behavior(Permission, permission, user)
    end

    test "EmployeePermission archival functionality" do
      user = create_user!(%{role: :admin})
      business = create_test_business!(user)

      employee = create_test_employee!(business)
      permission = create_test_permission!()

      employee_permission = create_test_employee_permission!(employee, permission, user)
      test_archival_behavior(EmployeePermission, employee_permission, user)
    end
  end

  @doc """
  Helper function to test standard archival behavior.

  ## Parameters
  - resource: The Ash resource module to test
  - record: The record instance to test archival behavior on
  - actor: The actor performing the archival operation

  ## Returns
  - :ok when test passes
  """
  defp test_archival_behavior(resource, record, actor) do
    # Verify record starts unarchived
    assert is_nil(record.archived_at)

    # Archive the record
    archived_record =
      record
      |> Ash.Changeset.for_destroy(:archive)
      |> Ash.destroy!(actor: actor, domain: Domain, return_destroyed?: true)

    # Verify archived_at is set
    refute is_nil(archived_record.archived_at)
    assert %DateTime{} = archived_record.archived_at

    # Verify archived records are excluded from normal queries
    normal_query_results = resource |> Ash.read!(actor: actor, domain: Domain)
    refute Enum.any?(normal_query_results, &(&1.id == archived_record.id))

    # Verify archived records can be queried explicitly
    all_records =
      resource
      |> Ash.Query.unset([:filter])
      |> Ash.read!(actor: actor, domain: Domain)

    assert Enum.any?(all_records, &(&1.id == archived_record.id && not is_nil(&1.archived_at)))
  end

  @doc """
  Creates a test business for archival testing.

  ## Parameters
  - user: The user actor creating the business

  ## Returns
  - The created business struct
  """
  defp create_test_business!(user) do
    Business
    |> Ash.Changeset.for_create(:create, %{
      name: "Test Business #{System.unique_integer()}",
      description: "A test business for archival testing"
    })
    |> Ash.create!(actor: user, domain: Domain)
  end

  @doc """
  Creates a test client for archival testing.

  ## Parameters
  - business: The business to associate the client with

  ## Returns
  - The created client struct
  """
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

  @doc """
  Creates a test permission for archival testing.

  ## Returns
  - The created permission struct
  """
  defp create_test_permission! do
    Permission
    |> Ash.Changeset.for_create(:create, %{
      name: "test_permission_#{System.unique_integer()}",
      description: "A test permission for archival testing",
      category: :system
    })
    |> Ash.create!(domain: Domain)
  end

  @doc """
  Creates a test employee for archival testing.

  ## Parameters
  - business: The business to associate the employee with

  ## Returns
  - The created employee struct
  """
  defp create_test_employee!(business) do
    Employee
    |> Ash.Changeset.for_create(:create, %{
      business_id: business.id,
      name: "Test Employee #{System.unique_integer()}",
      email: "employee#{System.unique_integer()}@example.com",
      role: :staff
    })
    |> Ash.create!(domain: Domain)
  end

  @doc """
  Creates a test employee permission for archival testing.

  ## Parameters
  - employee: The employee to grant permission to
  - permission: The permission to grant
  - granter: The user granting the permission

  ## Returns
  - The created employee permission struct
  """
  defp create_test_employee_permission!(employee, permission, granter) do
    EmployeePermission
    |> Ash.Changeset.for_create(:create, %{
      employee_id: employee.id,
      permission_id: permission.id,
      granted_by_id: granter.id,
      notes: "Test permission grant"
    })
    |> Ash.create!(domain: Domain)
  end
end
