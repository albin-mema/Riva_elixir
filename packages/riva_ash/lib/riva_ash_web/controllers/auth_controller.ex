defmodule RivaAshWeb.AuthController do
  @moduledoc """
  Authentication controller for handling user authentication flows.

  Provides endpoints for:
  - User sign in and sign out
  - User registration
  - Authentication token management
  - Session management
  """

  use RivaAshWeb, :controller
  alias RivaAsh.Accounts
  alias RivaAshWeb.AuthHelpers
  alias RivaAsh.ErrorHelpers

  plug(:put_layout, {RivaAshWeb.Layouts, :app})

  @type conn :: Plug.Conn.t()
  @type params :: map()
  @type user :: struct()
  @type token :: String.t()
  @type result :: {:ok, user()} | {:error, String.t() | Ecto.Changeset.t()}

  @doc """
  Redirects users based on their authentication status.
  """
  @spec redirect_to_dashboard(conn(), params()) :: conn()
  def redirect_to_dashboard(conn, _params) do
    conn
    |> check_authentication_status()
    |> case do
      {:authenticated, user_conn} -> redirect(user_conn, to: "/dashboard")
      {:unauthenticated, unauth_conn} -> redirect(unauth_conn, to: "/sign-in")
    end
  end

  @doc """
  Redirects unauthenticated users to sign-in page.
  """
  @spec redirect_to_sign_in(conn(), params()) :: conn()
  def redirect_to_sign_in(conn, _params) do
    conn
    |> check_authentication_status()
    |> case do
      {:authenticated, user_conn} -> redirect(user_conn, to: "/dashboard")
      {:unauthenticated, unauth_conn} -> redirect(unauth_conn, to: "/sign-in")
    end
  end

  @doc """
  Renders the sign-in page.
  """
  @spec sign_in(conn(), params()) :: conn()
  def sign_in(conn, _params) do
    render(conn, :sign_in)
  end

  @doc """
  Handles sign-in form submission.
  """
  @spec sign_in_submit(conn(), params()) :: conn()
  def sign_in_submit(conn, %{"email" => email, "password" => password}) do
    case authenticate_user(email, password) do
      {:ok, user, token} ->
        conn
        |> establish_user_session(user, token)
        |> redirect_to_dashboard_after_auth()
      {:error, reason} ->
        handle_sign_in_error(conn, reason)
    end
  end

  @doc """
  Completes sign-in process with token verification.
  """
  @spec complete_sign_in(conn(), params()) :: conn()
  def complete_sign_in(conn, %{"token" => token, "user_id" => user_id}) do
    case {verify_token(token), get_user_by_id(user_id)} do
      {{:ok, ^user_id}, {:ok, user}} ->
        conn
        |> establish_user_session(user, token)
        |> redirect_to_dashboard_after_auth()
      {:error, _reason} ->
        handle_invalid_token(conn)
    end
  end

  @doc """
  Handles user sign-out.
  """
  @spec sign_out(conn(), params()) :: conn()
  def sign_out(conn, _params) do
    conn
    |> AuthHelpers.sign_out_user()
    |> put_flash(:info, "Successfully signed out!")
    |> redirect(to: "/sign-in")
  end

  @doc """
  Renders the registration page.
  """
  @spec register(conn(), params()) :: conn()
  def register(conn, _params) do
    render(conn, :register)
  end

  @doc """
  Handles registration form submission.
  """
  @spec register_submit(conn(), params()) :: conn()
  def register_submit(conn, %{
        "name" => name,
        "email" => email,
        "password" => password,
        "password_confirmation" => password_confirmation
      }) do
    with :ok <- validate_password_confirmation(password, password_confirmation),
         {:ok, _user} <- create_user_account(name, email, password) do
      conn
      |> put_flash(:info, "Registration successful! Please sign in.")
      |> redirect(to: "/sign-in")
    else
      {:error, reason} -> handle_registration_error(conn, reason)
    end
  end

  # Private helper functions

  defp check_authentication_status(conn) do
    if conn.assigns[:current_user] do
      {:authenticated, conn}
    else
      {:unauthenticated, conn}
    end
  end

  defp authenticate_user(email, password) do
    case Accounts.sign_in(email, password) do
      {:ok, %{resource: user, token: token}} -> {:ok, user, token}
      {:ok, user} when is_struct(user) -> {:ok, user, generate_token(user.id)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp generate_token(user_id) do
    Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user_id)
  end

  defp verify_token(token) do
    case Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", token, max_age: 300) do
      {:ok, user_id} -> {:ok, user_id}
      {:error, _reason} -> {:error, "Invalid or expired token"}
    end
  end

  defp get_user_by_id(user_id) do
    case Ash.get(RivaAsh.Accounts.User, user_id, action: :seed_read, domain: RivaAsh.Accounts) do
      {:ok, user} -> {:ok, user}
      {:error, _reason} -> {:error, "User not found"}
    end
  end

  defp establish_user_session(conn, user, token) do
    conn
    |> put_session(:user_token, token)
    |> assign(:current_user, user)
    |> put_flash(:info, "Successfully signed in!")
  end

  defp redirect_to_dashboard_after_auth(conn) do
    redirect(conn, to: "/businesses")
  end

  defp handle_sign_in_error(conn, reason) when is_binary(reason) do

    conn
    |> put_flash(:error, reason)
    |> redirect(to: "/sign-in")
  end

  defp handle_sign_in_error(conn, _error) do
    conn
    |> put_flash(:error, "An error occurred during sign in. Please try again.")
    |> redirect(to: "/sign-in")
  end

  defp handle_invalid_token(conn) do
    conn
    |> put_flash(:error, "Invalid or expired sign-in token")
    |> redirect(to: "/sign-in")
  end

  defp validate_password_confirmation(password, password_confirmation) do
    if password == password_confirmation do
      :ok
    else
      {:error, "Password confirmation does not match"}
    end
  end

  defp create_user_account(name, email, password) do
    Accounts.register(%{
      "name" => name,
      "email" => email,
      "password" => password
    })
  end

  defp handle_registration_error(conn, reason) do
    error_messages = format_changeset_errors(reason)

    conn
    |> put_flash(:error, "Registration failed: #{error_messages}")
    |> redirect(to: "/register")
  end

  # Error formatting functions

  defp format_changeset_errors(%Ash.Error.Invalid{} = error) do
    Enum.map_join(error.errors, ", ", &format_ash_error/1)
  end

  defp format_changeset_errors(changeset) when is_struct(changeset) do
    case Map.get(changeset, :errors) do
      errors when is_list(errors) ->
        errors
        |> Enum.map(fn
          {field, {message, _opts}} -> "#{field} #{message}"
          error -> format_ash_error(error)
        end)
        |> Enum.join(", ")

      _ ->
        "Registration failed"
    end
  end

  defp format_ash_error(%{message: message, field: field}) when is_atom(field),
    do: "#{field} #{message}"

  defp format_ash_error(%{message: message}), do: message
  defp format_ash_error(%{input: input}), do: "Invalid input: #{input}"
  defp format_ash_error(%{field: field}) when is_atom(field), do: "Invalid field: #{field}"
  defp format_ash_error(error) when is_binary(error), do: error
  defp format_ash_error(_), do: "Registration failed"
end
