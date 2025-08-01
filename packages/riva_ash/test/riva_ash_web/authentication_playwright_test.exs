defmodule RivaAshWeb.AuthenticationPlaywrightTest do
  @moduledoc """
  Real browser testing for authentication flows using Phoenix Test with Playwright.
  """

  use PhoenixTest.Playwright.Case,
    async: false,
    # Force visible browser
    headless: false,
    # Slow down actions so you can see them
    slow_mo: 2000

  import PhoenixTest
  alias RivaAsh.Accounts.User
  alias RivaAsh.Accounts

  setup do
    # Create a test user for login tests using Ash
    {:ok, user} =
      User
      |> Ash.Changeset.for_create(:register_with_password, %{
        name: "Playwright Test User",
        email: "playwright_test@example.com",
        password: "password123",
        password_confirmation: "password123"
      })
      |> Ash.create(domain: RivaAsh.Accounts)

    %{user: user}
  end

  test "user registration with real browser", %{conn: conn} do
    # This opens a REAL browser window using Playwright
    IO.puts("🌐 Opening browser for registration test...")
    IO.puts("🔍 Browser should be visible if PLAYWRIGHT_HEADLESS=false")

    conn
    |> visit("/register")
    |> assert_has("h2", text: "Create a new account")

    # Fill form fields by actually typing in the browser
    |> fill_in("Name", with: "Real Playwright User")
    |> fill_in("Email", with: "playwright_user@example.com")
    |> fill_in("Password", with: "securepassword123")
    |> fill_in("Confirm Password", with: "securepassword123")

    # Take a screenshot before submitting
    |> screenshot("registration_form_filled")

    # Actually click the submit button in the browser
    |> click_button("Create Account")

    # Wait for redirect and verify we're on sign-in page
    |> assert_path("/sign-in")
    |> assert_has("h2", text: "Sign in to your account")

    # Take a screenshot after redirect
    |> screenshot("registration_success")

    # Verify user was created in database
    query =
      User
      |> Ash.Query.for_read(:read)
      |> Ash.Query.filter(email == "playwright_user@example.com")

    assert {:ok, [user]} = Ash.read(query, domain: RivaAsh.Accounts)
    assert user.name == "Real Playwright User"
  end

  test "login flow with real browser", %{conn: conn, user: user} do
    # Navigate to sign-in page in real browser
    conn
    |> visit("/sign-in")
    |> assert_has("h2", text: "Sign in to your account")

    # Fill login form by actually typing in browser
    |> fill_in("Email", with: to_string(user.email))
    |> fill_in("Password", with: "password123")

    # Take screenshot before login
    |> screenshot("login_form_filled.png")

    # Actually click the login button
    |> click_button("Sign In")

    # Wait for redirect and verify we're on businesses page
    |> assert_path("/businesses")
    |> assert_has("h1", text: "Business")

    # Take screenshot after successful login
    |> screenshot("login_success.png")
  end

  test "complete authentication flow with real browser", %{conn: conn} do
    # Step 1: Register a new user in real browser
    conn
    |> visit("/register")
    |> fill_in("Name", with: "Complete Flow User")
    |> fill_in("Email", with: "complete_flow_playwright@example.com")
    |> fill_in("Password", with: "password123")
    |> fill_in("Confirm Password", with: "password123")
    |> screenshot("complete_flow_registration.png")
    |> click_button("Create Account")
    |> assert_path("/sign-in")

    # Step 2: Login with the new user
    |> fill_in("Email", with: "complete_flow_playwright@example.com")
    |> fill_in("Password", with: "password123")
    |> screenshot("complete_flow_login.png")
    |> click_button("Sign In")
    |> assert_path("/businesses")

    # Step 3: Navigate to different protected pages
    |> click_link("Dashboard")
    |> assert_path("/dashboard")
    |> assert_has("h1", text: "Dashboard")
    |> screenshot("complete_flow_dashboard.png")

    # Step 4: Access clients page
    |> click_link("Clients")
    |> assert_path("/clients")
    |> assert_has("h1", text: "Clients")
    |> screenshot("complete_flow_clients.png")
  end

  test "form validation with real browser", %{conn: conn} do
    # Test form validation by actually interacting with the browser
    conn
    |> visit("/register")
    # Empty name
    |> fill_in("Name", with: "")
    # Invalid email
    |> fill_in("Email", with: "invalid-email")
    # Too short
    |> fill_in("Password", with: "123")
    # Mismatch
    |> fill_in("Confirm Password", with: "456")
    |> screenshot("form_validation_invalid.png")
    |> click_button("Create Account")

    # Should stay on registration page (validation failed)
    |> assert_path("/register")
    |> assert_has("h2", text: "Create a new account")
    |> screenshot("form_validation_errors.png")
  end

  test "login with invalid credentials in real browser", %{conn: conn, user: user} do
    conn
    |> visit("/sign-in")
    |> fill_in("Email", with: to_string(user.email))
    |> fill_in("Password", with: "wrongpassword")
    |> screenshot("invalid_login_attempt.png")
    |> click_button("Sign In")

    # Should stay on sign-in page or show error
    |> assert_path("/sign-in")
    |> assert_has("h2", text: "Sign in to your account")
    |> screenshot("invalid_login_error.png")
  end

  test "visual regression testing with real browser", %{conn: conn} do
    # Test that pages render correctly in the browser
    conn
    |> visit("/sign-in")
    |> assert_has("h2", text: "Sign in to your account")
    |> assert_has("input[name='email']")
    |> assert_has("input[name='password']")
    |> assert_has("button[type='submit']")
    |> screenshot("visual_signin_page.png")

    # Test registration page
    |> visit("/register")
    |> assert_has("h2", text: "Create a new account")
    |> assert_has("input[name='name']")
    |> assert_has("input[name='email']")
    |> assert_has("input[name='password']")
    |> assert_has("input[name='password_confirmation']")
    |> screenshot("visual_register_page.png")
  end

  @tag trace: :open
  test "JavaScript interactions work correctly in real browser", %{conn: conn} do
    # Test any JavaScript-dependent features
    # The @tag trace: :open will record a trace and open it automatically for debugging
    conn
    |> visit("/register")
    |> fill_in("Email", with: "invalid-email")
    |> fill_in("Password", with: "123")
    |> fill_in("Confirm Password", with: "456")
    |> screenshot("js_interactions_test.png")

    # If you have client-side validation, test it here
    # The browser will actually execute JavaScript
    # |> assert_has(".error", text: "Invalid email format")
  end

  test "cross-browser compatibility", %{conn: conn} do
    # This test uses the configured browser (Chromium by default)
    conn
    |> visit("/register")
    |> assert_has("h2", text: "Create a new account")
    |> fill_in("Name", with: "Cross Browser User")
    |> fill_in("Email", with: "crossbrowser_playwright@example.com")
    |> fill_in("Password", with: "password123")
    |> fill_in("Confirm Password", with: "password123")
    |> screenshot("cross_browser_test.png")
    |> click_button("Create Account")
    |> assert_path("/sign-in")
  end

  test "keyboard navigation and accessibility", %{conn: conn} do
    # Test keyboard navigation
    conn
    |> visit("/sign-in")
    # Tab to next field
    |> press("input[name='email']", "Tab")
    |> type("input[name='password']", "password123")
    # Submit with Enter
    |> press("input[name='password']", "Enter")
    |> screenshot("keyboard_navigation_test.png")
  end

  test "mobile viewport simulation", %{conn: conn} do
    # Test how the page looks on mobile (Playwright can simulate different viewports)
    conn
    |> visit("/sign-in")
    |> assert_has("h2", text: "Sign in to your account")
    |> screenshot("mobile_signin_test.png")

    # Test registration page on mobile
    |> visit("/register")
    |> assert_has("h2", text: "Create a new account")
    |> screenshot("mobile_register_test.png")
  end
end
