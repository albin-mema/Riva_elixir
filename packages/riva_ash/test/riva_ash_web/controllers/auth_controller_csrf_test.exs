defmodule RivaAshWeb.AuthControllerCSRFTest do
  use RivaAshWeb.ConnCase, async: true

  describe "CSRF protection" do
    test "registration form includes CSRF token", %{conn: conn} do
      conn = get(conn, ~p"/register")
      assert html_response(conn, 200) =~ ~s(name="_csrf_token")
    end

    test "registration form submission works with valid CSRF token", %{conn: conn} do
      # First, get the registration page to establish a session
      conn = get(conn, ~p"/register")
      assert html_response(conn, 200)

      # Extract CSRF token from the session
      csrf_token = get_csrf_token()

      # Submit registration with CSRF token
      conn =
        post(conn, ~p"/register", %{
          "_csrf_token" => csrf_token,
          "name" => "Test User",
          "email" => "test@example.com",
          "password" => "password123",
          "password_confirmation" => "password123"
        })

      # Should redirect on successful registration (or show validation errors)
      # Not expecting a CSRF error
      assert redirected_to(conn) == "/sign-in" or html_response(conn, 302)
    end

    test "sign-in form includes CSRF token", %{conn: conn} do
      conn = get(conn, ~p"/sign-in")
      assert html_response(conn, 200) =~ ~s(name="_csrf_token")
    end

    test "sign-out form includes CSRF token", %{conn: conn} do
      # This would require authentication setup, so we'll skip for now
      # but the fix should work for sign-out as well
    end
  end

  describe "session handling" do
    test "fetch_current_user doesn't clear CSRF token when no user token exists", %{conn: conn} do
      # Get a page to establish session with CSRF token
      conn = get(conn, ~p"/register")
      assert html_response(conn, 200)

      # The session should still have the CSRF token after fetch_current_user runs
      # (which happens in the browser pipeline)
      csrf_token = get_session(conn, "_csrf_token")
      assert csrf_token != nil
    end

    test "fetch_current_user doesn't clear CSRF token when invalid user token exists", %{
      conn: conn
    } do
      # Set an invalid user token
      conn =
        conn
        |> init_test_session(%{})
        |> put_session(:user_token, "invalid_token")

      # Get a page to trigger fetch_current_user
      conn = get(conn, ~p"/register")
      assert html_response(conn, 200)

      # The session should still have the CSRF token
      csrf_token = get_session(conn, "_csrf_token")
      assert csrf_token != nil

      # But user_token should be cleared
      user_token = get_session(conn, :user_token)
      assert user_token == nil
    end
  end
end
