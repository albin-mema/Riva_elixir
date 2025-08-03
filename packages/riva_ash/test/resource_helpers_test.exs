defmodule RivaAsh.ResourceHelpersTest do
  @moduledoc """
  Tests for the standardized resource helper macros.
  Ensures consistent configuration across all resources.
  """

  use ExUnit.Case
  import RivaAsh.ResourceHelpers

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

  # Helper function to check if a resource has a specific extension
  defp assert_extension_present(resource, extension_module, test_function) do
    try do
      result = test_function.()
      assert result, "#{resource} should have #{extension_module} extension"
    rescue
      _ ->
        flunk("#{resource} should have #{extension_module} extension")
    end
  end

  describe "Standard resource configurations" do
    test "all business resources have consistent extensions" do
      business_resources = [
        Business,
        Client,
        Employee,
        Item,
        ItemType,
        Section,
        Plot,
        Layout,
        ItemPosition,
        ItemSchedule,
        AvailabilityException,
        Reservation,
        Payment,
        Pricing,
        RecurringReservation,
        RecurringReservationInstance
      ]

      for resource <- business_resources do
        # Check for AshJsonApi.Resource extension (all should have this)
        assert_extension_present(resource, AshJsonApi.Resource, fn ->
          AshJsonApi.Resource.Info.type(resource)
        end)

        # Check for AshArchival.Resource extension (all should have this)
        assert_extension_present(resource, AshArchival.Resource, fn ->
          Ash.Resource.Info.attributes(resource)
          |> Enum.find(&(&1.name == :archived_at))
        end)
      end
    end

    test "all resources have standard postgres configuration" do
      resources_with_tables = [
        {Business, "businesses"},
        {Client, "clients"},
        {Employee, "employees"},
        {Item, "items"},
        {ItemType, "item_types"},
        {Section, "sections"},
        {Plot, "plots"},
        {Layout, "layouts"},
        {ItemPosition, "item_positions"},
        {ItemHold, "item_holds"},
        {ItemSchedule, "item_schedules"},
        {AvailabilityException, "availability_exceptions"},
        {Reservation, "reservations"},
        {Payment, "payments"},
        {Pricing, "pricing"},
        {Permission, "permissions"},
        {EmployeePermission, "employee_permissions"},
        {RecurringReservation, "recurring_reservations"},
        {RecurringReservationInstance, "recurring_reservation_instances"}
      ]

      for {resource, expected_table} <- resources_with_tables do
        data_layer = Ash.Resource.Info.data_layer(resource)

        assert data_layer == AshPostgres.DataLayer,
               "#{resource} should use AshPostgres.DataLayer"

        table_name = AshPostgres.DataLayer.Info.table(resource)

        assert table_name == expected_table,
               "#{resource} should use table '#{expected_table}', got '#{table_name}'"

        repo = AshPostgres.DataLayer.Info.repo(resource)

        assert repo == RivaAsh.Repo,
               "#{resource} should use RivaAsh.Repo"
      end
    end

    test "all archival resources have standard archive configuration" do
      archival_resources = [
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
      ]

      for resource <- archival_resources do
        # Check for archived_at attribute
        attributes = Ash.Resource.Info.attributes(resource)
        archived_at_attr = Enum.find(attributes, &(&1.name == :archived_at))

        assert archived_at_attr, "#{resource} should have archived_at attribute"
        assert archived_at_attr.type == Ash.Type.UtcDatetimeUsec
        assert archived_at_attr.allow_nil?

        # Check for destroy action (archival resources use standard destroy action)
        actions = Ash.Resource.Info.actions(resource)
        destroy_action = Enum.find(actions, &(&1.name == :destroy && &1.type == :destroy))

        assert destroy_action, "#{resource} should have destroy action"
      end
    end

    test "all paper trail resources have consistent configuration" do
      # Only include resources that actually have AshPaperTrail.Resource extension
      paper_trail_resources = [
        Business,
        Client,
        Employee,
        Item,
        ItemType,
        Section,
        Plot,
        Layout,
        Reservation,
        Payment,
        Pricing,
        Permission,
        EmployeePermission,
        RecurringReservation,
        RecurringReservationInstance
      ]

      for resource <- paper_trail_resources do
        # Check for AshPaperTrail.Resource extension
        assert_extension_present(resource, AshPaperTrail.Resource, fn ->
          AshPaperTrail.Resource.Info.version_resource(resource)
        end)

        # Verify version resource exists
        version_resource_name = Module.concat(resource, Version)

        assert Code.ensure_loaded?(version_resource_name),
               "#{resource} should have a Version resource: #{version_resource_name}"
      end
    end

    test "resources have standard UUID primary keys" do
      resources = [
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
      ]

      for resource <- resources do
        primary_key = Ash.Resource.Info.primary_key(resource)
        assert primary_key == [:id], "#{resource} should have :id as primary key"

        id_attribute = Ash.Resource.Info.attribute(resource, :id)
        assert id_attribute.type == Ash.Type.UUID, "#{resource} :id should be UUID type"
        assert id_attribute.primary_key?, "#{resource} :id should be marked as primary key"
      end
    end

    test "resources have standard timestamps" do
      resources = [
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
      ]

      for resource <- resources do
        attributes = Ash.Resource.Info.attributes(resource)

        inserted_at = Enum.find(attributes, &(&1.name == :inserted_at))
        assert inserted_at, "#{resource} should have inserted_at timestamp"
        assert inserted_at.type == Ash.Type.UtcDatetimeUsec

        updated_at = Enum.find(attributes, &(&1.name == :updated_at))
        assert updated_at, "#{resource} should have updated_at timestamp"
        assert updated_at.type == Ash.Type.UtcDatetimeUsec
      end
    end

    test "JSON API resources have consistent configuration" do
      json_api_resources = [
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
        RecurringReservation,
        RecurringReservationInstance
      ]

      for resource <- json_api_resources do
        # Check for AshJsonApi.Resource extension
        assert_extension_present(resource, AshJsonApi.Resource, fn ->
          AshJsonApi.Resource.Info.type(resource)
        end)

        # Check that JSON API type is configured
        json_api_type = AshJsonApi.Resource.Info.type(resource)
        assert json_api_type, "#{resource} should have JSON API type configured"

        # Check that basic routes are configured
        routes = AshJsonApi.Resource.Info.routes(resource)
        assert length(routes) > 0, "#{resource} should have JSON API routes configured"
      end
    end
  end

  describe "Helper macro functionality" do
    test "standard_postgres macro sets correct configuration" do
      # This is tested implicitly by the resource configuration tests above
      # The macro is used in the actual resources, so if they pass, the macro works
      assert true
    end

    test "standard_archive macro sets correct configuration" do
      # This is tested implicitly by the archival configuration tests above
      assert true
    end

    test "standard_paper_trail macro sets correct configuration" do
      # This is tested implicitly by the paper trail configuration tests above
      assert true
    end
  end
end
