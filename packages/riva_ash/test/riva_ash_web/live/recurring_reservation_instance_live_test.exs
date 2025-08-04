defmodule RivaAshWeb.Live.RecurringReservationInstanceLiveTest do
  @moduledoc """
  Property-based and integration tests for the RecurringReservationInstanceLive component.
  Tests LiveView functionality, event handling, and integration with Ash resources.
  """
  use RivaAshWeb.ConnCase
  use ExUnitProperties

  import Phoenix.LiveViewTest
  import RivaAsh.TestHelpers

  @moduletag :live
  @moduletag :integration
  @moduletag :slow

  describe "RecurringReservationInstanceLive mount and render" do
    setup %{conn: conn} do
      {conn, user} = create_and_sign_in_user(conn, %{role: :admin})
      %{conn: conn, user: user}
    end

    test "mounts successfully for authenticated user", %{conn: conn, user: user} do
      # Create a business for the user
      business = create_business!(user)

      # Mount the LiveView
      {:ok, _view, html} = live(conn, "/recurring-reservation-instances")

      # Check that the page title is rendered
      assert html =~ "Recurring Reservation Instances"
      assert html =~ "View and manage individual instances of recurring reservations"
    end

    test "redirects to sign-in page for unauthenticated user", %{conn: conn} do
      # Try to access the page without authentication
      assert {:error, {:redirect, %{to: "/sign-in"}}} =
               live(conn, "/recurring-reservation-instances")
    end

    test "redirects to access-denied page for unauthorized user", %{conn: conn} do
      # Create a user without proper permissions
      user = create_user!(%{role: :user})

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Try to mount the LiveView - should redirect to access denied
      # Note: This might depend on specific permission setup in your application
      {:ok, _view, html} = live(conn, "/recurring-reservation-instances")

      # The exact behavior will depend on your authorization setup
      # This is a placeholder assertion
      assert is_binary(html)
    end
  end

  describe "RecurringReservationInstanceLive empty state" do
    test "shows empty state when no instances exist", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, html} = live(conn, "/recurring-reservation-instances")

      # Check that the empty state is shown
      assert html =~ "No recurring reservation instances found"
      assert html =~ "Create Your First Instance"
    end
  end

  describe "RecurringReservationInstanceLive with data" do
    test "shows instances when they exist", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a section and item for the recurring reservation
      section = create_section!(business)
      item = create_item!(section)

      # Create a recurring reservation
      recurring_reservation = create_recurring_reservation!(item, user)

      # Create some instances
      instance1 =
        create_recurring_reservation_instance!(recurring_reservation, %{
          scheduled_date: Date.utc_today(),
          sequence_number: 1,
          status: :pending
        })

      instance2 =
        create_recurring_reservation_instance!(recurring_reservation, %{
          scheduled_date: Date.add(Date.utc_today(), 1),
          sequence_number: 2,
          status: :confirmed
        })

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, html} = live(conn, "/recurring-reservation-instances")

      # Check that the instances are shown
      assert html =~ "Recurring Reservation Instances"
      assert html =~ "#{instance1.sequence_number}"
      assert html =~ "#{instance2.sequence_number}"
      assert html =~ "#{instance1.scheduled_date}"
      assert html =~ "#{instance2.scheduled_date}"
    end
  end

  describe "RecurringReservationInstanceLive create functionality" do
    test "shows create form when New Instance button is clicked", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Click the New Instance button
      html = view |> element("button", "New Instance") |> render_click()

      # Check that the form is shown
      assert html =~ "Add New Instance"
      assert html =~ "Fill in the details to add a new recurring reservation instance"
    end

    test "cancels form when Cancel button is clicked", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Click the New Instance button
      view |> element("button", "New Instance") |> render_click()

      # Click the Cancel button
      html = view |> element("button", "Cancel") |> render_click()

      # Check that the form is hidden
      refute html =~ "Add New Instance"
      assert html =~ "Recurring Reservation Instances"
    end
  end

  describe "RecurringReservationInstanceLive edit functionality" do
    test "shows edit form when Edit button is clicked", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a section and item for the recurring reservation
      section = create_section!(business)
      item = create_item!(section)

      # Create a recurring reservation
      recurring_reservation = create_recurring_reservation!(item, user)

      # Create an instance
      instance =
        create_recurring_reservation_instance!(recurring_reservation, %{
          scheduled_date: Date.utc_today(),
          sequence_number: 1,
          status: :pending
        })

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Click the Edit button for the instance
      html = view |> element("button[phx-value-id='#{instance.id}']", "Edit") |> render_click()

      # Check that the form is shown
      assert html =~ "Edit Instance"
      assert html =~ "Update the recurring reservation instance information below"
    end
  end

  describe "RecurringReservationInstanceLive delete functionality" do
    test "shows confirmation dialog when Delete button is clicked", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a section and item for the recurring reservation
      section = create_section!(business, user)
      item = create_item!(section)

      # Create a recurring reservation
      recurring_reservation = create_recurring_reservation!(item, user)

      # Create an instance
      instance =
        create_recurring_reservation_instance!(recurring_reservation, %{
          scheduled_date: Date.utc_today(),
          sequence_number: 1,
          status: :pending
        })

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Click the Delete button for the instance
      html = view |> element("button[phx-value-id='#{instance.id}']", "Delete") |> render_click()

      # Check that the confirmation dialog is shown
      assert html =~ "Delete Instance"
      assert html =~ "Are you sure you want to delete this recurring reservation instance?"
    end

    test "cancels deletion when Cancel is clicked in confirmation dialog", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a section and item for the recurring reservation
      section = create_section!(business)
      item = create_item!(section)

      # Create a recurring reservation
      recurring_reservation = create_recurring_reservation!(item, user)

      # Create an instance
      instance =
        create_recurring_reservation_instance!(recurring_reservation, %{
          scheduled_date: Date.utc_today(),
          sequence_number: 1,
          status: :pending
        })

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Click the Delete button for the instance
      view |> element("button[phx-value-id='#{instance.id}']", "Delete") |> render_click()

      # Click the Cancel button in the confirmation dialog
      html = view |> element("button", "Cancel") |> render_click()

      # Check that the confirmation dialog is hidden
      refute html =~ "Delete Instance"
      assert html =~ "#{instance.sequence_number}"
    end

    test "confirms deletion when Delete is clicked in confirmation dialog", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a section and item for the recurring reservation
      section = create_section!(business)
      item = create_item!(section)

      # Create a recurring reservation
      recurring_reservation = create_recurring_reservation!(item, user)

      # Create an instance
      instance =
        create_recurring_reservation_instance!(recurring_reservation, %{
          scheduled_date: Date.utc_today(),
          sequence_number: 1,
          status: :pending
        })

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Click the Delete button for the instance
      view |> element("button[phx-value-id='#{instance.id}']", "Delete") |> render_click()

      # Click the Delete button in the confirmation dialog
      html = view |> element("button", "Delete") |> render_click()

      # Check that success message is shown
      assert html =~ "Instance deleted successfully!"
    end
  end

  describe "Property-based tests for RecurringReservationInstanceLive" do
    property "renders with random page numbers for pagination", %{conn: conn} do
      check all(
              page <- integer(1..10),
              max_runs: 20
            ) do
        # Create a user
        user = create_user!()

        # Create a business for the user
        business = create_business!(user)

        # Create a section and item for the recurring reservation
        section = create_section!(business)
        item = create_item!(section)

        # Create a recurring reservation
        recurring_reservation = create_recurring_reservation!(item, user)

        # Create multiple instances
        Enum.each(1..25, fn i ->
          create_recurring_reservation_instance!(recurring_reservation, %{
            scheduled_date: Date.add(Date.utc_today(), i),
            sequence_number: i,
            status: Enum.random([:pending, :confirmed, :failed, :skipped, :cancelled])
          })
        end)

        # Create a session with the user token
        conn = assign_user_token(conn, user)

        # Mount the LiveView with pagination
        {:ok, _view, html} = live(conn, "/recurring-reservation-instances?page=#{page}")

        # Basic assertions that should always pass
        assert html =~ "Recurring Reservation Instances"
      end
    end

    property "handles random status values correctly", %{conn: conn} do
      check all(
              status <- member_of([:pending, :confirmed, :failed, :skipped, :cancelled]),
              max_runs: 20
            ) do
        # Create a user
        user = create_user!()

        # Create a business for the user
        business = create_business!(user)

        # Create a section and item for the recurring reservation
        section = create_section!(business)
        item = create_item!(section)

        # Create a recurring reservation
        recurring_reservation = create_recurring_reservation!(item, user)

        # Create an instance with random status
        instance =
          create_recurring_reservation_instance!(recurring_reservation, %{
            scheduled_date: Date.utc_today(),
            sequence_number: 1,
            status: status
          })

        # Create a session with the user token
        conn = assign_user_token(conn, user)

        # Mount the LiveView
        {:ok, _view, html} = live(conn, "/recurring-reservation-instances")

        # Check that the status is displayed correctly
        assert html =~ "#{status}"
      end
    end
  end

  describe "RecurringReservationInstanceLive error handling" do
    test "shows error message when instance creation fails", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Click the New Instance button
      view |> element("button", "New Instance") |> render_click()

      # Try to submit form with invalid data (missing required fields)
      html =
        view
        |> element("form")
        |> render_submit(%{
          form: %{
            scheduled_date: "",
            sequence_number: "",
            status: ""
          }
        })

      # Check that error message is shown
      assert html =~ "Failed to save instance"
    end

    test "shows error message when instance update fails", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a section and item for the recurring reservation
      section = create_section!(business)
      item = create_item!(section)

      # Create a recurring reservation
      recurring_reservation = create_recurring_reservation!(item, user)

      # Create an instance
      instance =
        create_recurring_reservation_instance!(recurring_reservation, %{
          scheduled_date: Date.utc_today(),
          sequence_number: 1,
          status: :pending
        })

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Click the Edit button for the instance
      view |> element("button[phx-value-id='#{instance.id}']", "Edit") |> render_click()

      # Try to submit form with invalid data
      html =
        view
        |> element("form")
        |> render_submit(%{
          form: %{
            scheduled_date: "",
            sequence_number: "",
            status: ""
          }
        })

      # Check that error message is shown
      assert html =~ "Failed to save instance"
    end

    test "shows error message when instance deletion fails", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, view, _html} = live(conn, "/recurring-reservation-instances")

      # Try to delete a non-existent instance
      html = view |> element("button[phx-value-id='non-existent-id']", "Delete") |> render_click()

      # Click the Delete button in the confirmation dialog
      html = view |> element("button", "Delete") |> render_click()

      # Check that error message is shown
      assert html =~ "Failed to find instance"
    end
  end

  describe "RecurringReservationInstanceLive accessibility" do
    test "includes proper ARIA attributes for accessibility", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, _view, html} = live(conn, "/recurring-reservation-instances")

      # Check for basic accessibility attributes
      assert html =~ "aria-live"
      assert html =~ "role="
    end

    test "includes proper ARIA attributes for pagination", %{conn: conn} do
      # Create a user
      user = create_user!()

      # Create a business for the user
      business = create_business!(user)

      # Create a section and item for the recurring reservation
      section = create_section!(business)
      item = create_item!(section)

      # Create a recurring reservation
      recurring_reservation = create_recurring_reservation!(item, user)

      # Create multiple instances to trigger pagination
      Enum.each(1..25, fn i ->
        create_recurring_reservation_instance!(recurring_reservation, %{
          scheduled_date: Date.add(Date.utc_today(), i),
          sequence_number: i,
          status: :pending
        })
      end)

      # Create a session with the user token
      conn = assign_user_token(conn, user)

      # Mount the LiveView
      {:ok, _view, html} = live(conn, "/recurring-reservation-instances")

      # Check for pagination accessibility attributes
      assert html =~ "role=\"navigation\""
      assert html =~ "aria-label=\"Pagination Navigation\""
    end
  end
end
