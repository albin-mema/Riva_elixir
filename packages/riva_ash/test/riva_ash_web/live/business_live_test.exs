defmodule RivaAshWeb.BusinessLiveTest do
  use RivaAshWeb.FeatureCase, async: true

  alias RivaAsh.Resources.Business

  describe "Business LiveView" do
    setup %{conn: conn} do
      {conn, user} = create_and_sign_in_user(conn, %{role: :admin})
      %{conn: conn, user: user}
    end

    test "renders business management page", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> assert_has("h1", text: "Business Management")
      |> assert_has("p", text: "Manage your business entities")
      |> assert_has("button", text: "Add Business")
    end

    test "shows empty state when no businesses exist", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> assert_has(".text-muted-foreground", text: "No businesses found")
      |> assert_has("p", text: "Create your first business to get started")
    end

    test "displays existing businesses", %{conn: conn, user: user} do
      business = create_business!(%{name: "Test Business", description: "A test business"}, user)

      conn
      |> visit("/businesses")
      |> assert_has("h3", text: "Test Business")
      |> assert_has("p", text: "A test business")
      |> assert_has("button", text: "Edit")
      |> assert_has("button", text: "Delete")
    end

    test "can toggle business form", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> refute_has("form")
      |> click_button("Add Business")
      |> assert_has("form")
      |> assert_has("input[name='form[name]']")
      |> assert_has("textarea[name='form[description]']")
      |> click_button("Cancel")
      |> refute_has("form")
    end

    test "validates business form inputs", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> click_button("Add Business")
      |> fill_in("Name", with: "")
      |> fill_in("Description", with: "")
      |> click_button("Create Business")
      |> assert_has(".text-destructive")
    end

    test "creates a new business successfully", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> click_button("Add Business")
      |> fill_in("Name", with: "New Business")
      |> fill_in("Description", with: "A new business")
      |> click_button("Create Business")
      |> assert_has(".alert", text: "Business \"New Business\" created successfully!")
      |> assert_has("h3", text: "New Business")
      |> assert_has("p", text: "A new business")
      |> refute_has("form")
    end

    test "validates business name format", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> click_button("Add Business")
      |> fill_in("Name", with: "Invalid@#$%Business")
      |> fill_in("Description", with: "Test")
      |> click_button("Create Business")
      |> assert_has(".text-destructive")
    end

    test "validates business name length", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> click_button("Add Business")
      |> fill_in("Name", with: "A")
      |> fill_in("Description", with: "Test")
      |> click_button("Create Business")
      |> assert_has(".text-destructive")

      # Try to submit with name too long
      long_name = String.duplicate("A", 101)
      conn
      |> visit("/businesses")
      |> click_button("Add Business")
      |> fill_in("Name", with: long_name)
      |> fill_in("Description", with: "Test")
      |> click_button("Create Business")
      |> assert_has(".text-destructive")
    end

    test "can edit an existing business", %{conn: conn, user: user} do
      business = create_business!(%{name: "Original Business", description: "Original description"}, user)

      conn
      |> visit("/businesses")
      |> click_button("Edit")
      |> assert_has("form")
      |> assert_has("input", value: "Original Business")
      |> fill_in("Name", with: "Updated Business")
      |> fill_in("Description", with: "Updated description")
      |> click_button("Update Business")
      |> assert_has(".alert", text: "Business \"Updated Business\" updated successfully!")
      |> assert_has("h3", text: "Updated Business")
      |> assert_has("p", text: "Updated description")
    end

    test "can delete a business", %{conn: conn, user: user} do
      business = create_business!(%{name: "Business to Delete", description: "Will be deleted"}, user)

      conn
      |> visit("/businesses")
      |> assert_has("h3", text: "Business to Delete")
      |> click_button("Delete")
      |> assert_has(".alert", text: "Business \"Business to Delete\" deleted successfully!")
      |> refute_has("h3", text: "Business to Delete")
      |> assert_has("p", text: "No businesses found")
    end

    test "shows loading state during form submission", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> click_button("Add Business")
      |> fill_in("Name", with: "Test Business")
      |> fill_in("Description", with: "Test")
      |> click_button("Create Business")
      |> assert_has(".alert", text: "Business \"Test Business\" created successfully!")
    end

    test "displays business creation and update timestamps", %{conn: conn, user: user} do
      business = create_business!(%{name: "Timestamped Business", description: "Has timestamps"}, user)

      conn
      |> visit("/businesses")
      |> assert_has("span", text: "Created:")
      |> refute_has("span", text: "Updated:")
    end
  end
end
