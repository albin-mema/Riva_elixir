defmodule RivaAshWeb.AuthHelpers do
  import Plug.Conn
  import Phoenix.Controller
  alias RivaAsh.ErrorHelpers

  # Plug behaviour implementation
  def init(opts), do: opts

  def call(conn, :fetch_current_user) do
    fetch_current_user(conn, [])
  end

  def call(conn, :require_authenticated_user) do
    require_authenticated_user(conn, [])
  end

  # This function can be used to require authentication for certain routes
  def require_authenticated_user(conn, _opts) do
    if conn.assigns[:current_user] do
      conn
    else
      conn
      |> put_flash(:error, "You must be logged in to access this page.")
      |> redirect(to: "/sign-in")
      |> halt()
    end
  end

  # This function can be used to fetch the current user from the session
  def fetch_current_user(conn, _opts) do
    user_token = get_session(conn, :user_token)

    with {:ok, token} <- (if user_token, do: ErrorHelpers.success(user_token), else: ErrorHelpers.failure(:no_token)),
         {:ok, user_id} <- Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", token, max_age: 86_400) |> ErrorHelpers.to_result(),
         {:ok, user} <- Ash.get(RivaAsh.Accounts.User, user_id, domain: RivaAsh.Accounts) |> ErrorHelpers.to_result() do
      assign(conn, :current_user, user)
    else
      _ ->
        conn
        |> clear_session()
        |> assign(:current_user, nil)
    end
  end

  # Helper function to sign in a user
  def sign_in_user(conn, user) do
    case ErrorHelpers.required(user, :user_required) do
      {:ok, validated_user} ->
        token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", validated_user.id)

        conn
        |> put_session(:user_token, token)
        |> assign(:current_user, validated_user)
      {:error, _} -> conn
    end
  end

  # Helper function to sign out a user
  def sign_out_user(conn) do
    conn
    |> clear_session()
    |> assign(:current_user, nil)
  end
end
