defmodule RivaAshWeb.RouteAuthorizationTest do
  @moduledoc """
  Tests to verify that all main routes handle authorization correctly
  and don't throw Ash.Error.Forbidden exceptions.
  """
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias RivaAsh.Accounts.User
  alias RivaAsh.Resources.Business

  describe "authenticated routes" do
    setup do
      # Create a test user
      {:ok, user} =
        User.create(%{
          email: "test@example.com",
          password: "password123",
          role: :user
        })

      # Create a test business owned by the user
      {:ok, business} =
        Business.create(
          %{
            name: "Test Business",
            description: "A test business",
            owner_id: user.id
          },
          actor: user
        )

      # Create a session token
      token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

      %{user: user, business: business, token: token}
    end

    test "dashboard loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/dashboard")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "businesses page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/businesses")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "item-types page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/item-types")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "items page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/items")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "sections page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/sections")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "plots page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/plots")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "layouts page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/layouts")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "reservations page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/reservations")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "payments page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/payments")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end

    test "employees page loads without authorization errors", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/employees")
      refute html =~ "Forbidden"
      refute html =~ "Access Denied"
    end
  end

  describe "unauthenticated routes" do
    test "dashboard redirects to sign-in when not authenticated" do
      conn = build_conn()

      assert {:error, {:redirect, %{to: "/sign-in"}}} = live(conn, "/dashboard")
    end

    test "businesses redirects to sign-in when not authenticated" do
      conn = build_conn()

      assert {:error, {:redirect, %{to: "/sign-in"}}} = live(conn, "/businesses")
    end

    test "item-types redirects to sign-in when not authenticated" do
      conn = build_conn()

      assert {:error, {:redirect, %{to: "/sign-in"}}} = live(conn, "/item-types")
    end
  end

  describe "access denied page" do
    test "access denied page loads correctly" do
      conn = build_conn()

      assert {:ok, _view, html} = live(conn, "/access-denied")
      assert html =~ "Access Denied"
      assert html =~ "You need to be signed in"
    end

    test "access denied page shows user info when authenticated", %{token: token} do
      conn =
        build_conn()
        |> init_test_session(%{"user_token" => token})

      assert {:ok, _view, html} = live(conn, "/access-denied")
      assert html =~ "Access Denied"
      # Should show the user's role
      assert html =~ "User"
    end
  end
end
