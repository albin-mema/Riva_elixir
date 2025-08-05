defmodule RivaAshWeb.AuthHelpers do
  @moduledoc """
  Authentication helper functions and plugs.

  Provides reusable authentication functionality including:
  - User session management
  - Authentication verification
  - Token handling
  - Authentication plugs
  """

  import Plug.Conn
  import Phoenix.Controller
  alias RivaAsh.ErrorHelpers
  alias RivaAsh.Accounts.User

  @type conn :: Plug.Conn.t()
  @type opts :: keyword()
  @type user :: User.t()
  @type token :: String.t()
  @type result :: {:ok, any()} | {:error, any()}

  @doc """
  Initializes the plug with given options.
  """
  @spec init(opts()) :: opts()
  def init(opts), do: opts

  @doc """
  Fetches the current user from the session and assigns to connection.
  """
  @spec call(conn(), :fetch_current_user) :: conn()
  def call(conn, :fetch_current_user) do
    fetch_current_user(conn, [])
  end

  @doc """
  Requires authenticated user, redirects to sign-in if not authenticated.
  """
  @spec call(conn(), :require_authenticated_user) :: conn()
  def call(conn, :require_authenticated_user) do
    require_authenticated_user(conn, [])
  end

  @doc """
  Requires authentication for certain routes.
  """
  @spec require_authenticated_user(conn(), opts()) :: conn()
  def require_authenticated_user(conn, _opts) do
    case get_current_user(conn) do
      {:authenticated, user_conn} -> user_conn
      {:unauthenticated, unauth_conn} -> redirect_to_sign_in(unauth_conn)
    end
  end

  @doc """
  Fetches the current user from the session.
  """
  @spec fetch_current_user(conn(), opts()) :: conn()
  def fetch_current_user(conn, _opts) do
    with {:ok, user} <- load_user_from_session(conn) do
      assign(conn, :current_user, user)
    else
      {:error, _reason} -> clear_user_session(conn)
    end
  end

  @doc """
  Signs in a user by establishing their session.
  """
  @spec sign_in_user(conn(), user()) :: conn()
  def sign_in_user(conn, user) do
    with {:ok, validated_user} <- validate_user(user),
         {:ok, token} <- generate_auth_token(validated_user.id) do
      establish_user_session(conn, validated_user, token)
    else
      {:error, _reason} -> conn
    end
  end

  @doc """
  Signs out a user by clearing their session.
  """
  @spec sign_out_user(conn()) :: conn()
  def sign_out_user(conn) do
    conn
    |> delete_session(:user_token)
    |> assign(:current_user, nil)
  end

  # Private helper functions

  defp get_current_user(conn) do
    case conn.assigns[:current_user] do
      nil -> {:unauthenticated, conn}
      user -> {:authenticated, conn}
    end
  end

  defp redirect_to_sign_in(conn) do
    conn
    |> put_flash(:error, "You must be logged in to access this page.")
    |> redirect(to: "/sign-in")
    |> halt()
  end

  defp load_user_from_session(conn) do
    user_token = get_session(conn, :user_token)

    with {:ok, token} <- validate_token_exists(user_token),
         {:ok, user_id} <- verify_token(token),
         {:ok, user} <- fetch_user_by_id(user_id) do
      {:ok, user}
    end
  end

  defp validate_token_exists(nil), do: {:error, :no_token}
  defp validate_token_exists(token), do: {:ok, token}

  defp verify_token(token) do
    case Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", token, max_age: 86_400) do
      {:ok, user_id} -> {:ok, user_id}
      {:error, reason} -> {:error, reason}
    end
  end

  defp fetch_user_by_id(user_id) do
    case Ash.get(RivaAsh.Accounts.User, user_id, action: :seed_read, domain: RivaAsh.Accounts) do
      {:ok, user} -> {:ok, user}
      {:error, reason} -> {:error, reason}
    end
  end

  defp clear_user_session(conn) do
    conn
    |> delete_session(:user_token)
    |> assign(:current_user, nil)
  end

  defp validate_user(user) when is_struct(user, User) do
    {:ok, user}
  end

  defp validate_user(_), do: {:error, :invalid_user}

  defp generate_auth_token(user_id) do
    token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user_id)
    {:ok, token}
  end

  defp establish_user_session(conn, user, token) do
    conn
    |> put_session(:user_token, token)
    |> assign(:current_user, user)
  end
end
