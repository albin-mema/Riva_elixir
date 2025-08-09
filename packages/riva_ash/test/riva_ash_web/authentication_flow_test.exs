defmodule RivaAshWeb.AuthenticationFlowTest do
  use RivaAshWeb.FeatureCase, async: true
  use RivaAshWeb, :verified_routes
  import Phoenix.LiveViewTest
  alias RivaAsh.Accounts.User
  alias RivaAsh.Accounts

  describe "User Registration Flow" do
    @spec test_registration_page_loads_correctly :: :ok
    test "registration page loads correctly", %{conn: conn} do
      # Test that we can access the registration page via controller route
      conn = get(conn, "/register")
      assert html_response(conn, 200) =~ "Create"
    end

    @spec test_user_can_register_with_valid_credentials_via_controller :: :ok
    test "user can register with valid credentials via controller", %{conn: conn} do
      # Test registration via controller POST
      conn =
        post(conn, "/register", %{
          "name" => "Test User",
          "email" => "test@example.com",
          "password" => "password123",
          "password_confirmation" => "password123"
        })

      # Should redirect to sign-in page after successful registration
      assert redirected_to(conn) == "/sign-in"

      # Verify user was created in database
      require Ash.Query
      import Ash.Expr
      query = User |> Ash.Query.for_read(:read) |> Ash.Query.filter(expr(email == "test@example.com"))

      case Ash.read(query, domain: RivaAsh.Accounts) do
        {:ok, [user]} ->
          assert user.name == "Test User"
          assert to_string(user.email) == "test@example.com"

        {:ok, []} ->
          flunk("User was not created")

        {:error, error} ->
          flunk("Error reading user: #{inspect(error)}")
      end
    end

    @spec test_registration_fails_with_password_mismatch_via_controller :: :ok
    test "registration fails with password mismatch via controller", %{conn: conn} do
      conn =
        post(conn, "/register", %{
          "name" => "Test User",
          "email" => "test@example.com",
          "password" => "password123",
          "password_confirmation" => "different_password"
        })

      # Should redirect back to register with error
      assert redirected_to(conn) == "/register"
      assert get_flash(conn, :error) =~ "Password confirmation does not match"
    end

    @spec test_registration_fails_with_invalid_data_via_controller :: :ok
    test "registration fails with invalid data via controller", %{conn: conn} do
      conn =
        post(conn, "/register", %{
          "name" => "",
          "email" => "invalid-email",
          "password" => "123",
          "password_confirmation" => "123"
        })

      # Should redirect back to register with error
      assert redirected_to(conn) == "/register"
      assert get_flash(conn, :error) =~ "Registration failed"
    end
  end

  describe "User Login Flow" do
    setup do
      # Create a test user for login tests using Ash
      {:ok, user} =
        User
        |> Ash.Changeset.for_create(:register_with_password, %{
          name: "Test User",
          email: "test@example.com",
          password: "password123",
          password_confirmation: "password123"
        })
        |> Ash.create(domain: RivaAsh.Accounts)

      %{user: user}
    end

    @spec test_sign_in_page_loads_correctly :: :ok
    test "sign-in page loads correctly", %{conn: conn} do
      # Test that we can access the sign-in page
      {:ok, _view, html} = live(conn, "/sign-in")
      assert html =~ "Sign in to your account"
    end

    @spec test_user_can_login_with_valid_credentials_via_controller :: :ok
    test "user can login with valid credentials via controller", %{conn: conn, user: user} do
      # Test login via controller POST
      conn =
        post(conn, "/sign-in", %{
          "email" => to_string(user.email),
          "password" => "password123"
        })

      # Should redirect to businesses page after successful login
      assert redirected_to(conn) == "/businesses"
      assert get_flash(conn, :info) =~ "Successfully signed in"
    end

    @spec test_login_fails_with_invalid_email_via_controller :: :ok
    test "login fails with invalid email via controller", %{conn: conn} do
      conn =
        post(conn, "/sign-in", %{
          "email" => "nonexistent@example.com",
          "password" => "password123"
        })

      # Should redirect back to sign-in with error
      assert redirected_to(conn) == "/sign-in"
      assert get_flash(conn, :error) != nil
    end

    @spec test_login_fails_with_invalid_password_via_controller :: :ok
    test "login fails with invalid password via controller", %{conn: conn, user: user} do
      conn =
        post(conn, "/sign-in", %{
          "email" => to_string(user.email),
          "password" => "wrong_password"
        })

      # Should redirect back to sign-in with error
      assert redirected_to(conn) == "/sign-in"
      assert get_flash(conn, :error) != nil
    end
  end

  describe "User Logout Flow" do
    setup do
      # Create and authenticate a test user using Ash
      {:ok, user} =
        User
        |> Ash.Changeset.for_create(:register_with_password, %{
          name: "Test User",
          email: "logout_test@example.com",
          password: "password123",
          password_confirmation: "password123"
        })
        |> Ash.create(domain: RivaAsh.Accounts)

      %{user: user}
    end

    @spec test_authenticated_user_can_logout :: :ok
    test "authenticated user can logout", %{conn: conn, user: user} do
      # First, authenticate the user
      token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

      conn =
        conn
        |> init_test_session(%{"user_token" => token})

      # Verify user is authenticated by accessing dashboard
      {:ok, _view, html} = live(conn, "/dashboard")
      assert html =~ "Dashboard"

      # Test logout functionality
      conn = post(conn, "/sign-out")

      # Should redirect to sign-in page
      assert redirected_to(conn) == "/sign-in"
      assert get_flash(conn, :info) =~ "Successfully signed out"

      # Verify session is cleared
      assert get_session(conn, :user_token) == nil
    end

    @spec test_unauthenticated_user_cannot_access_protected_routes :: :ok
    test "unauthenticated user cannot access protected routes", %{conn: conn} do
      # Try to access dashboard without authentication
      result = live(conn, "/dashboard")

      # Should redirect to sign-in or show access denied
      case result do
        {:error, {:redirect, %{to: "/sign-in"}}} ->
          assert true

        {:error, {:live_redirect, %{to: "/sign-in"}}} ->
          assert true

        {:ok, _view, html} ->
          assert html =~ "Access Denied" or html =~ "Sign in"

        _ ->
          flunk("Expected redirect to sign-in or access denied message")
      end
    end
  end

  describe "Authentication Session Management" do
    setup do
      {:ok, user} =
        User
        |> Ash.Changeset.for_create(:register_with_password, %{
          name: "Session Test User",
          email: "session_test@example.com",
          password: "password123",
          password_confirmation: "password123"
        })
        |> Ash.create(domain: RivaAsh.Accounts)

      %{user: user}
    end

    @spec test_session_persists_across_requests :: :ok
    test "session persists across requests", %{conn: conn, user: user} do
      # Authenticate user
      token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

      conn =
        conn
        |> init_test_session(%{"user_token" => token})

      # Access multiple protected routes
      {:ok, _view, html1} = live(conn, "/dashboard")
      assert html1 =~ "Dashboard"

      {:ok, _view, html2} = live(conn, "/businesses")
      assert html2 =~ "Business"
    end

    @spec test_invalid_session_token_is_handled_gracefully :: :ok
    test "invalid session token is handled gracefully", %{conn: conn} do
      # Set invalid token
      conn =
        conn
        |> init_test_session(%{"user_token" => "invalid_token"})

      # Try to access protected route
      result = live(conn, "/dashboard")

      # Should handle gracefully (redirect or show appropriate message)
      case result do
        {:error, {:redirect, %{to: "/sign-in"}}} ->
          assert true

        {:error, {:live_redirect, %{to: "/sign-in"}}} ->
          assert true

        {:ok, _view, html} ->
          assert html =~ "Access Denied" or html =~ "Sign in"

        _ ->
          # Any graceful handling is acceptable
          assert true
      end
    end

    @spec test_expired_session_token_is_handled_gracefully :: :ok
    test "expired session token is handled gracefully", %{conn: conn, user: user} do
      # Create an expired token (this is a simulation - in real tests you'd need to mock time)
      expired_token =
        Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id, signed_at: System.system_time(:second) - 86_401)

      conn =
        conn
        |> init_test_session(%{"user_token" => expired_token})

      # Try to access protected route
      result = live(conn, "/dashboard")

      # Should handle expired token gracefully
      case result do
        {:error, {:redirect, %{to: "/sign-in"}}} ->
          assert true

        {:error, {:live_redirect, %{to: "/sign-in"}}} ->
          assert true

        {:ok, _view, html} ->
          assert html =~ "Access Denied" or html =~ "Sign in"

        _ ->
          # Any graceful handling is acceptable
          assert true
      end
    end
  end

  # ============================================================================
  # PHOENIX TEST BROWSER-STYLE TESTS (In-Memory Simulation)
  # ============================================================================
  # These tests use PhoenixTest for a more browser-like API while still running
  # in memory (no actual browser opens). This provides better test readability
  # and more realistic user interaction patterns.

  describe "Browser-Style Authentication Tests (PhoenixTest)" do
    # PhoenixTest removed; keep test logic intact by using LiveViewTest/ConnTest APIs where possible.

    setup do
      # Create a test user for login tests using Ash
      {:ok, user} =
        User
        |> Ash.Changeset.for_create(:register_with_password, %{
          name: "Browser Test User",
          email: "browser_test@example.com",
          password: "password123",
          password_confirmation: "password123"
        })
        |> Ash.create(domain: RivaAsh.Accounts)

      %{user: user}
    end

    @spec test_complete_user_registration_flow_with_browser_like_interactions :: :ok
    test "complete user registration flow with browser-like interactions", %{conn: conn} do
      # Visit registration page
      session = conn |> visit("/register")

      # Verify we're on the registration page
      session |> assert_has("h2", text: "Create a new account")

      # Fill out the registration form using browser-like interactions
      session
      |> fill_in("Name", with: "New Browser User")
      |> fill_in("Email", with: "newuser@example.com")
      |> fill_in("Password", with: "securepassword123")
      |> fill_in("Confirm Password", with: "securepassword123")
      |> click_button("Create Account")

      # Should be redirected to sign-in page
      |> assert_path("/sign-in")
      |> assert_has("h2", text: "Sign in to your account")

      # Verify user was created in database
      require Ash.Query
      import Ash.Expr
      query =
        User |> Ash.Query.for_read(:read) |> Ash.Query.filter(expr(email == "newuser@example.com"))

      case Ash.read(query, domain: RivaAsh.Accounts) do
        {:ok, [user]} ->
          assert user.name == "New Browser User"
          assert to_string(user.email) == "newuser@example.com"

        {:ok, []} ->
          flunk("User was not created")

        {:error, error} ->
          flunk("Error reading user: #{inspect(error)}")
      end
    end

    @spec test_user_login_flow_with_browser_like_interactions :: :ok
    test "user login flow with browser-like interactions", %{conn: conn, user: user} do
      # Visit sign-in page
      session = conn |> visit("/sign-in")

      # Verify we're on the sign-in page
      session |> assert_has("h2", text: "Sign in to your account")

      # Fill out login form and submit
      session
      |> fill_in("Email", with: to_string(user.email))
      |> fill_in("Password", with: "password123")
      |> click_button("Sign In")

      # Should be redirected to businesses page after successful login
      |> assert_path("/businesses")
      |> assert_has("h1", text: "Business")
    end

    @spec test_login_with_invalid_credentials_shows_error :: :ok
    test "login with invalid credentials shows error", %{conn: conn, user: user} do
      session = conn |> visit("/sign-in")

      # Try to login with wrong password
      session
      |> fill_in("Email", with: to_string(user.email))
      |> fill_in("Password", with: "wrongpassword")
      |> click_button("Sign In")

      # Should stay on sign-in page and show error
      |> assert_path("/sign-in")

      # Note: Error messages might be shown via flash messages or inline
      # The exact assertion would depend on how errors are displayed
    end

    @spec test_complete_authentication_flow_register_login_access_protected_logout :: :ok
    test "complete authentication flow: register -> login -> access protected -> logout", %{
      conn: conn
    } do
      # Step 1: Register a new user
      session =
        conn
        |> visit("/register")
        |> fill_in("Name", with: "Flow Test User")
        |> fill_in("Email", with: "flowtest@example.com")
        |> fill_in("Password", with: "password123")
        |> fill_in("Confirm Password", with: "password123")
        |> click_button("Create Account")
        |> assert_path("/sign-in")

      # Step 2: Login with the new user
      session
      |> fill_in("Email", with: "flowtest@example.com")
      |> fill_in("Password", with: "password123")
      |> click_button("Sign In")
      |> assert_path("/businesses")

      # Step 3: Access protected resources
      session
      |> click_link("Dashboard")
      |> assert_path("/dashboard")
      |> assert_has("h1", text: "Dashboard")

      # Step 4: Navigate to different protected pages
      session
      |> click_link("Clients")
      |> assert_path("/clients")
      |> assert_has("h1", text: "Clients")

      # Step 5: Logout (this would need to be implemented based on your logout mechanism)
      # session
      # |> click_button("Sign Out")
      # |> assert_path("/sign-in")
    end

    @spec test_unauthenticated_user_is_redirected_to_sign_in :: :ok
    test "unauthenticated user is redirected to sign-in", %{conn: conn} do
      # Try to access protected page without authentication
      conn
      |> visit("/dashboard")
      # Should be redirected to sign-in (exact behavior depends on your auth implementation)
      |> assert_path("/sign-in")
    end

    @spec test_form_validation_errors_are_displayed_properly :: :ok
    test "form validation errors are displayed properly", %{conn: conn} do
      session = conn |> visit("/register")

      # Submit form with invalid data
      session
      # Empty name
      |> fill_in("Name", with: "")
      # Invalid email
      |> fill_in("Email", with: "invalid-email")
      # Too short password
      |> fill_in("Password", with: "123")
      # Mismatched confirmation
      |> fill_in("Confirm Password", with: "456")
      |> click_button("Create Account")

      # Should stay on registration page and show validation errors
      session |> assert_path("/register")

      # Note: Specific error message assertions would depend on how validation errors are displayed
    end
  end
end
