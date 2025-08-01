defmodule RivaAshWeb.AuthFlowTest do
  use RivaAshWeb.ConnCase, async: true
  use PhoenixTest

  @moduletag :auth

  describe "Authentication Flow" do
    test "user can register, sign in, and sign out", %{conn: conn} do
      # Test user registration
      conn
      |> visit("/register")
      |> assert_has("h1", text: "Register")
      |> fill_in("Email", with: "test@example.com")
      |> fill_in("Password", with: "password123")
      |> fill_in("Password confirmation", with: "password123")
      |> click_button("Register")
      |> assert_has(".alert", text: "User registered successfully")

      # Test user sign in
      conn
      |> visit("/sign-in")
      |> assert_has("h1", text: "Sign In")
      |> fill_in("Email", with: "test@example.com")
      |> fill_in("Password", with: "password123")
      |> click_button("Sign In")
      |> assert_has(".alert", text: "Welcome back!")

      # Test user sign out
      conn
      |> visit("/")
      |> click_link("Sign Out")
      |> assert_has(".alert", text: "You have been signed out")
    end

    test "registration with invalid data shows errors", %{conn: conn} do
      conn
      |> visit("/register")
      |> fill_in("Email", with: "invalid-email")
      |> fill_in("Password", with: "123")
      |> fill_in("Password confirmation", with: "456")
      |> click_button("Register")
      |> assert_has(".error", text: "must be a valid email")
      |> assert_has(".error", text: "should be at least 6 character(s)")
      |> assert_has(".error", text: "does not match password")
    end

    test "sign in with invalid credentials shows error", %{conn: conn} do
      conn
      |> visit("/sign-in")
      |> fill_in("Email", with: "nonexistent@example.com")
      |> fill_in("Password", with: "wrongpassword")
      |> click_button("Sign In")
      |> assert_has(".alert", text: "Invalid email or password")
    end

    test "accessing protected pages redirects to sign in", %{conn: conn} do
      conn
      |> visit("/businesses")
      |> assert_path("/sign-in")
      |> assert_has(".alert", text: "You must be signed in to access this page")
    end
  end
end
