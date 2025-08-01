defmodule RivaAshWeb.Components.Forms.RecurringReservationInstanceFormTest do
  @moduledoc """
  Property-based and unit tests for the RecurringReservationInstanceForm component.
  Tests component rendering, attribute validation, and form interaction logic.
  """
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.Component
  import Phoenix.LiveViewTest

  @moduletag :unit
  @moduletag :fast
  @moduletag :core
  @moduletag :pure

  # Import the component to test
  import RivaAshWeb.Components.Forms.RecurringReservationInstanceForm

  describe "RecurringReservationInstanceForm component" do
    test "renders form for creating new instance" do
      assigns = %{}

      # Create a mock form for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :create,
        __phoenix_refs: []
      }

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={false}
          loading={false}
          recurring_reservations={[]}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      assert html =~ "Add New Instance"
      assert html =~ "Fill in the details to add a new recurring reservation instance"
      assert html =~ "Scheduled Date"
      assert html =~ "Sequence Number"
      assert html =~ "Status"
      assert html =~ "Notes"
      assert html =~ "Skip Reason"
      refute html =~ "Edit Instance"
    end

    test "renders form for editing existing instance" do
      assigns = %{}

      # Create a mock form for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :update,
        __phoenix_refs: []
      }

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={true}
          loading={false}
          recurring_reservations={[]}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      assert html =~ "Edit Instance"
      assert html =~ "Update the recurring reservation instance information below"
      assert html =~ "Scheduled Date"
      assert html =~ "Sequence Number"
      assert html =~ "Status"
      assert html =~ "Notes"
      assert html =~ "Skip Reason"
      assert html =~ "Error Message"
      assert html =~ "Created At"
      assert html =~ "Failed At"
    end

    test "renders form with loading state" do
      assigns = %{}

      # Create a mock form for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :create,
        __phoenix_refs: []
      }

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={false}
          loading={true}
          recurring_reservations={[]}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      assert html =~ "Add Instance"
      # Check that the submit button has loading state
      assert html =~ "loading"
    end

    test "renders form with recurring reservations select" do
      assigns = %{}

      # Create a mock form for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :create,
        __phoenix_refs: []
      }

      recurring_reservations = [
        %{id: "rr-1", name: "Recurring Reservation 1"},
        %{id: "rr-2", name: "Recurring Reservation 2"}
      ]

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={false}
          loading={false}
          recurring_reservations={recurring_reservations}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      assert html =~ "Select a recurring reservation"
      assert html =~ "rr-1"
      assert html =~ "rr-2"
    end

    test "renders form with reservations select when editing" do
      assigns = %{}

      # Create a mock form for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :update,
        __phoenix_refs: []
      }

      reservations = [
        %{id: "res-1", name: "Reservation 1"},
        %{id: "res-2", name: "Reservation 2"}
      ]

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={true}
          loading={false}
          recurring_reservations={[]}
          reservations={reservations}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      assert html =~ "Select a reservation"
      assert html =~ "res-1"
      assert html =~ "res-2"
      assert html =~ "No reservation"
    end

    test "renders form with field errors" do
      assigns = %{}

      # Create a mock form with errors for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :create,
        __phoenix_refs: [],
        scheduled_date: %{
          id: "scheduled_date",
          name: "scheduled_date",
          errors: [{"can't be blank", []}],
          value: nil
        },
        sequence_number: %{
          id: "sequence_number",
          name: "sequence_number",
          errors: [{"can't be blank", []}],
          value: nil
        },
        status: %{
          id: "status",
          name: "status",
          errors: [{"can't be blank", []}],
          value: nil
        }
      }

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={false}
          loading={false}
          recurring_reservations={[]}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      assert html =~ "can't be blank"
      # Check that error fields have proper aria attributes
      assert html =~ "aria-invalid"
      assert html =~ "aria-describedby"
    end
  end

  describe "Property-based tests for RecurringReservationInstanceForm component" do
    property "form renders with random boolean values for editing and loading" do
      check all(
              editing <- boolean(),
              loading <- boolean(),
              max_runs: 100
            ) do
        # Create a mock form for testing
        form = %{
          source: %{
            data: %{},
            errors: []
          },
          id: "recurring-reservation-instance-form",
          name: "form",
          errors: [],
          data: %{},
          params: %{},
          hidden: [],
          options: [],
          action: if(editing, do: :update, else: :create),
          __phoenix_refs: []
        }

        html =
          rendered_to_string(~H"""
          <.recurring_reservation_instance_form
            form={form}
            editing={editing}
            loading={loading}
            recurring_reservations={[]}
            reservations={[]}
            on_submit="save_instance"
            on_change="validate_instance"
            on_cancel="cancel_form"
          />
          """)

        # Basic assertions that should always pass
        assert html =~ "Instance"
        assert html =~ "scheduled_date"
        assert html =~ "sequence_number"
        assert html =~ "status"

        # Conditional assertions based on editing state
        if editing do
          assert html =~ "Edit Instance"
          assert html =~ "Error Message"
          assert html =~ "Created At"
          assert html =~ "Failed At"
        else
          assert html =~ "Add New Instance"
        end
      end
    end

    property "form renders with random lists of recurring reservations" do
      check all(
              recurring_reservations <- list_of(map(:string, :string)),
              max_runs: 50
            ) do
        # Create a mock form for testing
        form = %{
          source: %{
            data: %{},
            errors: []
          },
          id: "recurring-reservation-instance-form",
          name: "form",
          errors: [],
          data: %{},
          params: %{},
          hidden: [],
          options: [],
          action: :create,
          __phoenix_refs: []
        }

        html =
          rendered_to_string(~H"""
          <.recurring_reservation_instance_form
            form={form}
            editing={false}
            loading={false}
            recurring_reservations={recurring_reservations}
            reservations={[]}
            on_submit="save_instance"
            on_change="validate_instance"
            on_cancel="cancel_form"
          />
          """)

        # Basic assertions
        assert html =~ "Add New Instance"
        assert html =~ "scheduled_date"
        assert html =~ "sequence_number"
        assert html =~ "status"
      end
    end

    property "form renders with random lists of reservations when editing" do
      check all(
              reservations <- list_of(map(:string, :string)),
              max_runs: 50
            ) do
        # Create a mock form for testing
        form = %{
          source: %{
            data: %{},
            errors: []
          },
          id: "recurring-reservation-instance-form",
          name: "form",
          errors: [],
          data: %{},
          params: %{},
          hidden: [],
          options: [],
          action: :update,
          __phoenix_refs: []
        }

        html =
          rendered_to_string(~H"""
          <.recurring_reservation_instance_form
            form={form}
            editing={true}
            loading={false}
            recurring_reservations={[]}
            reservations={reservations}
            on_submit="save_instance"
            on_change="validate_instance"
            on_cancel="cancel_form"
          />
          """)

        # Basic assertions
        assert html =~ "Edit Instance"
        assert html =~ "scheduled_date"
        assert html =~ "sequence_number"
        assert html =~ "status"
        assert html =~ "Error Message"
        assert html =~ "Created At"
        assert html =~ "Failed At"
      end
    end
  end

  describe "Component accessibility" do
    test "form includes proper ARIA attributes for accessibility" do
      assigns = %{}

      # Create a mock form for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :create,
        __phoenix_refs: [],
        scheduled_date: %{
          id: "scheduled_date",
          name: "scheduled_date",
          errors: [],
          value: nil
        },
        sequence_number: %{
          id: "sequence_number",
          name: "sequence_number",
          errors: [],
          value: nil
        },
        status: %{
          id: "status",
          name: "status",
          errors: [],
          value: nil
        }
      }

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={false}
          loading={false}
          recurring_reservations={[]}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      # Check for basic accessibility attributes
      assert html =~ "role=\"form\""
      # Check that form fields have proper labels
      assert html =~ "for=\"scheduled_date\""
      assert html =~ "for=\"sequence_number\""
      assert html =~ "for=\"status\""
    end

    test "form includes proper ARIA attributes when fields have errors" do
      assigns = %{}

      # Create a mock form with errors for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :create,
        __phoenix_refs: [],
        scheduled_date: %{
          id: "scheduled_date",
          name: "scheduled_date",
          errors: [{"can't be blank", []}],
          value: nil
        },
        sequence_number: %{
          id: "sequence_number",
          name: "sequence_number",
          errors: [{"must be greater than 0", []}],
          value: nil
        },
        status: %{
          id: "status",
          name: "status",
          errors: [{"can't be blank", []}],
          value: nil
        }
      }

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={false}
          loading={false}
          recurring_reservations={[]}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      # Check that error fields have proper aria attributes
      assert html =~ "aria-invalid=\"true\""
      assert html =~ "aria-describedby"
      # Check that error messages are associated with fields
      assert html =~ "scheduled_date-error"
      assert html =~ "sequence_number-error"
      assert html =~ "status-error"
    end
  end

  describe "Component error handling" do
    test "form handles nil form gracefully" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={nil}
          editing={false}
          loading={false}
          recurring_reservations={[]}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      # Should not crash and should render something
      assert is_binary(html)
    end

    test "form handles empty recurring reservations list gracefully" do
      assigns = %{}

      # Create a mock form for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :create,
        __phoenix_refs: []
      }

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={false}
          loading={false}
          recurring_reservations={[]}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      # Should render without crashing
      assert html =~ "Add New Instance"
      assert html =~ "scheduled_date"
    end

    test "form handles nil recurring reservations gracefully" do
      assigns = %{}

      # Create a mock form for testing
      form = %{
        source: %{
          data: %{},
          errors: []
        },
        id: "recurring-reservation-instance-form",
        name: "form",
        errors: [],
        data: %{},
        params: %{},
        hidden: [],
        options: [],
        action: :create,
        __phoenix_refs: []
      }

      html =
        rendered_to_string(~H"""
        <.recurring_reservation_instance_form
          form={form}
          editing={false}
          loading={false}
          recurring_reservations={nil}
          reservations={[]}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
        """)

      # Should render without crashing
      assert html =~ "Add New Instance"
      assert html =~ "scheduled_date"
    end
  end
end
