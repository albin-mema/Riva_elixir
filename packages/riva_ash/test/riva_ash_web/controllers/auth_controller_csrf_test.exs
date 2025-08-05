defmodule RivaAshWeb.AuthControllerCsrfTest do
  use RivaAshWeb.ConnCase, async: false
  use RivaAshWeb, :verified_routes

  describe "CSRF guard for login" do
    @describetag :controller
    @spec test_post_sign_in_without_csrf_token_is_rejected :: :ok
    test "POST /sign-in without CSRF token is rejected", %{conn: conn} do
      # Build a bare connection without fetching CSRF token
      params = %{"email" => "user@example.com", "password" => "badpass"}

      # Using browser pipeline path as defined in router
      conn = post(conn, ~p"/sign-in", params)

      # Phoenix protect_from_forgery will 403 on missing token by default
      assert conn.status in [400, 403]
    end

    @spec test_post_sign_in_with_csrf_token_succeeds_with_valid_credentials :: :ok
    test "POST /sign-in with CSRF token succeeds with valid credentials", %{conn: conn} do
      # Create a valid user via Accounts/Ash
      user =
        RivaAsh.Accounts.User
        |> Ash.Changeset.for_create(:register_with_password, %{
          name: "Auth #{System.unique_integer([:positive])}",
          email: "auth#{System.unique_integer([:positive])}@example.com",
          password: "password123",
          password_confirmation: "password123",
          role: :admin
        })
        |> Ash.create!(domain: RivaAsh.Accounts)

      # Prepare a form POST including a valid CSRF token
      conn =
        conn
        |> Phoenix.ConnTest.recycle()
        |> Phoenix.ConnTest.init_test_session(%{})
        |> Plug.Conn.fetch_session()
        |> Plug.CSRFProtection.init([])
        |> Plug.Conn.put_req_header("content-type", "application/x-www-form-urlencoded")

      token = Plug.CSRFProtection.get_csrf_token_for(~p"/sign-in")

      params = %{
        "_csrf_token" => token,
        "email" => user.email,
        "password" => "password123"
      }

      conn = post(conn, ~p"/sign-in", params)

      # Successful sign in redirects to /businesses per controller implementation
      assert conn.status in [302, 303]
      location = get_resp_header(conn, "location") |> List.first()
      assert location in [~p"/businesses", "/businesses"]

      # Session should contain :user_token
      assert Plug.Conn.get_session(conn, :user_token)
    end
  end
end
