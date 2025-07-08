defmodule RivaAshWeb.BusinessManagementFeatureTest do
  @moduledoc """
  Comprehensive UI tests for business management using phoenix_test.
  
  This test suite demonstrates the capabilities of phoenix_test for testing
  LiveView applications with authentication, form interactions, and UI flows.
  """
  use RivaAshWeb.FeatureCase, async: true

  describe "Business Management UI Flow" do
    setup %{conn: conn} do
      {conn, user} = create_and_sign_in_user(conn, %{role: :admin})
      %{conn: conn, user: user}
    end

    test "complete business management workflow", %{conn: conn, user: user} do
      # Visit the business management page
      conn
      |> visit("/businesses")
      |> assert_has("h1", text: "Business Management")
      |> assert_has("p", text: "Manage your business entities")
      |> assert_has("button", text: "Add Business")
      
      # Initially should show empty state
      |> assert_has("p", text: "No businesses found")
      |> assert_has("p", text: "Create your first business to get started")
      
      # Form should not be visible initially
      |> refute_has("form")
      
      # Click Add Business to show form
      |> click_button("Add Business")
      |> assert_has("form")
      |> assert_has("input[name='form[name]']")
      |> assert_has("textarea[name='form[description]']")
      |> assert_has("button", text: "Create Business")
      |> assert_has("button", text: "Cancel")
      
      # Test form validation - submit empty form
      |> click_button("Create Business")
      |> assert_has(".text-destructive") # Should show validation errors
      
      # Fill out the form with valid data
      |> fill_in("Name", with: "Test Restaurant")
      |> fill_in("Description", with: "A cozy family restaurant")
      |> click_button("Create Business")
      
      # Should show success message and the new business
      |> assert_has(".alert", text: "Business \"Test Restaurant\" created successfully!")
      |> assert_has("h3", text: "Test Restaurant")
      |> assert_has("p", text: "A cozy family restaurant")
      |> assert_has("button", text: "Edit")
      |> assert_has("button", text: "Delete")
      
      # Form should be hidden after successful creation
      |> refute_has("form")
      
      # Should no longer show empty state
      |> refute_has("p", text: "No businesses found")
    end

    test "business editing workflow", %{conn: conn, user: user} do
      # Create a business first
      _business = create_business!(%{name: "Original Cafe", description: "Original description"}, user)

      conn
      |> visit("/businesses")
      |> assert_has("h3", text: "Original Cafe")
      |> assert_has("p", text: "Original description")
      
      # Click edit button
      |> click_button("Edit")
      |> assert_has("form")
      |> assert_has("input[value='Original Cafe']")
      |> assert_has("textarea", text: "Original description")
      |> assert_has("button", text: "Update Business")
      
      # Update the business
      |> fill_in("Name", with: "Updated Cafe")
      |> fill_in("Description", with: "Updated description with more details")
      |> click_button("Update Business")
      
      # Should show success message and updated content
      |> assert_has(".alert", text: "Business \"Updated Cafe\" updated successfully!")
      |> assert_has("h3", text: "Updated Cafe")
      |> assert_has("p", text: "Updated description with more details")
      |> refute_has("form")
    end

    test "business deletion workflow", %{conn: conn, user: user} do
      # Create a business to delete
      _business = create_business!(%{name: "Cafe to Delete", description: "Will be removed"}, user)

      conn
      |> visit("/businesses")
      |> assert_has("h3", text: "Cafe to Delete")
      |> assert_has("p", text: "Will be removed")
      
      # Delete the business
      |> click_button("Delete")
      |> assert_has(".alert", text: "Business \"Cafe to Delete\" deleted successfully!")
      |> refute_has("h3", text: "Cafe to Delete")
      |> refute_has("p", text: "Will be removed")
      
      # Should show empty state again
      |> assert_has("p", text: "No businesses found")
    end

    test "form validation and error handling", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> click_button("Add Business")
      
      # Test name too short
      |> fill_in("Name", with: "A")
      |> fill_in("Description", with: "Valid description")
      |> click_button("Create Business")
      |> assert_has(".text-destructive") # Should show validation error
      
      # Test name too long
      |> fill_in("Name", with: String.duplicate("A", 101))
      |> click_button("Create Business")
      |> assert_has(".text-destructive") # Should show validation error
      
      # Test invalid characters in name
      |> fill_in("Name", with: "Invalid@#$%Name")
      |> click_button("Create Business")
      |> assert_has(".text-destructive") # Should show validation error
      
      # Test valid input
      |> fill_in("Name", with: "Valid Business Name")
      |> fill_in("Description", with: "A valid business description")
      |> click_button("Create Business")
      |> assert_has(".alert", text: "Business \"Valid Business Name\" created successfully!")
    end

    test "cancel form functionality", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> refute_has("form")
      
      # Open form
      |> click_button("Add Business")
      |> assert_has("form")
      
      # Fill in some data
      |> fill_in("Name", with: "Test Business")
      |> fill_in("Description", with: "Test description")
      
      # Cancel should hide form and clear data
      |> click_button("Cancel")
      |> refute_has("form")
      
      # Open form again - should be empty
      |> click_button("Add Business")
      |> assert_has("form")
      |> assert_has("input[name='form[name]'][value='']")
      |> assert_has("textarea[name='form[description]']", text: "")
    end

    test "multiple businesses display", %{conn: conn, user: user} do
      # Create multiple businesses
      _business1 = create_business!(%{name: "First Business", description: "First description"}, user)
      _business2 = create_business!(%{name: "Second Business", description: "Second description"}, user)
      _business3 = create_business!(%{name: "Third Business", description: "Third description"}, user)

      conn
      |> visit("/businesses")
      |> refute_has("p", text: "No businesses found")
      
      # Should display all businesses
      |> assert_has("h3", text: "First Business")
      |> assert_has("p", text: "First description")
      |> assert_has("h3", text: "Second Business")
      |> assert_has("p", text: "Second description")
      |> assert_has("h3", text: "Third Business")
      |> assert_has("p", text: "Third description")
      
      # Each should have edit and delete buttons
      |> assert_has("button", text: "Edit", count: 3)
      |> assert_has("button", text: "Delete", count: 3)
    end

    test "business timestamps display", %{conn: conn, user: user} do
      _business = create_business!(%{name: "Timestamped Business", description: "Has timestamps"}, user)

      conn
      |> visit("/businesses")
      |> assert_has("h3", text: "Timestamped Business")
      |> assert_has("span", text: "Created:")
      # For new businesses, created and updated times are the same, so updated might not show
    end
  end

  describe "Authentication Requirements" do
    test "redirects unauthenticated users", %{conn: conn} do
      # Don't sign in a user
      conn
      |> visit("/businesses")
      # Should redirect to sign in page or show authentication error
      |> assert_path("/sign_in") # or whatever your sign in path is
    end
  end
end
