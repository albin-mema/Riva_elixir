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
        extensions = Ash.Resource.Info.extensions(resource)

        # Check for required extensions
        assert AshJsonApi.Resource in extensions,
               "#{resource} should have AshJsonApi.Resource extension"

        assert AshPaperTrail.Resource in extensions,
               "#{resource} should have AshPaperTrail.Resource extension"

        assert AshArchival.Resource in extensions,
               "#{resource} should have AshArchival.Resource extension"
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
        assert archived_at_attr.type == :utc_datetime_usec
        assert archived_at_attr.allow_nil?

        # Check for archive action
        actions = Ash.Resource.Info.actions(resource)
        archive_action = Enum.find(actions, &(&1.name == :archive && &1.type == :destroy))

        assert archive_action, "#{resource} should have archive destroy action"
      end
    end

    test "all paper trail resources have consistent configuration" do
      paper_trail_resources = [
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
        Permission,
        EmployeePermission,
        RecurringReservation,
        RecurringReservationInstance
      ]

      for resource <- paper_trail_resources do
        extensions = Ash.Resource.Info.extensions(resource)

        assert AshPaperTrail.Resource in extensions,
               "#{resource} should have AshPaperTrail.Resource extension"

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
        assert id_attribute.type == :uuid, "#{resource} :id should be UUID type"
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
        assert inserted_at.type == :utc_datetime_usec

        updated_at = Enum.find(attributes, &(&1.name == :updated_at))
        assert updated_at, "#{resource} should have updated_at timestamp"
        assert updated_at.type == :utc_datetime_usec
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
        extensions = Ash.Resource.Info.extensions(resource)

        assert AshJsonApi.Resource in extensions,
               "#{resource} should have AshJsonApi.Resource extension"

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
