defmodule RivaAshWeb.DevTools.DevAuthController do
  @moduledoc """
  Dev-only controller to impersonate and sign out users via normal session flow.
  """
  use RivaAshWeb, :controller

  alias RivaAshWeb.AuthHelpers

  plug :ensure_dev

  def impersonate(conn, %{"user_id" => id}) do
    case Ash.get(RivaAsh.Accounts.User, id, domain: RivaAsh.Accounts) do
      {:ok, user} ->
        conn
        |> AuthHelpers.sign_in_user(user)
        |> put_flash(:info, "Impersonating #{user.email}")
        |> redirect(to: "/app/dashboard")

      {:error, _} ->
        conn
        |> put_flash(:error, "User not found")
        |> redirect(to: "/dev/user-session")
    end
  end

  def sign_out(conn, _params) do
    conn
    |> AuthHelpers.sign_out_user()
    |> put_flash(:info, "Signed out")
    |> redirect(to: "/dev/user-session")
  end

  defp ensure_dev(conn, _opts) do
    if Mix.env() == :dev, do: conn, else: redirect(conn, to: "/") |> halt()
  end
end

