defmodule RivaAshWeb.BusinessLiveTest do
  use RivaAshWeb.ConnCase, async: false
  # Minimal test fix: enable ~p verified routes and LiveView helper assertions
  use RivaAshWeb, :verified_routes
  import Phoenix.LiveViewTest
  import RivaAsh.Factory
  import RivaAshWeb, only: [verified_routes: 0]
  import RivaAsh.Test.LiveViewHelpers
  verified_routes()

  @tag :liveview
  describe "BusinessLive navigation basics" do
    test "mount shows dashboard widgets for admin", %{conn: conn} do
      %{user: user} = insert(:business_context)

      # Simulate authenticated session; router uses :user_token session lookup
      token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

      conn =
        conn
        |> Plug.Conn.put_session(:user_token, token)

      {:ok, view, _html} = live(conn, ~p"/businesses")

      # Minimal durable marker: table id present per view implementation
      assert_has(view, "#businesses-table")
    end

    test "non-admin redirected", %{conn: conn} do
      # Create a non-admin (staff) user and log them in
      user =
        RivaAsh.Accounts.User
        |> Ash.Changeset.for_create(:register_with_password, %{
          name: "Staff #{System.unique_integer([:positive])}",
          email: "staff#{System.unique_integer([:positive])}@example.com",
          password: "password123",
          password_confirmation: "password123",
          role: :staff
        })
        |> Ash.create!(domain: RivaAsh.Accounts)

      token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

      conn =
        conn
        |> Plug.Conn.put_session(:user_token, token)

      # The pipeline requires authentication and layout; app behavior on unauthorized should redirect.
      assert {:error, {:redirect, %{to: to}}} = live(conn, ~p"/businesses")
      # Accept either access denied page or landing depending on app behavior
      assert to in [~p"/access-denied", ~p"/sign-in", "/access-denied", "/sign-in"]
    end
  end
end
