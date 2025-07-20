defmodule RivaAshWeb.AuthController do
  use RivaAshWeb, :controller
  alias RivaAsh.Accounts
  alias RivaAshWeb.AuthHelpers
  alias RivaAsh.ErrorHelpers

  plug(:put_layout, {RivaAshWeb.Layouts, :app})

  def redirect_to_sign_in(conn, _params) do
    if conn.assigns[:current_user] do
      redirect(conn, to: "/businesses")
    else
      redirect(conn, to: "/sign-in")
    end
  end

  def sign_in(conn, _params) do
    render(conn, :sign_in)
  end

  def sign_in_submit(conn, %{"email" => email, "password" => password}) do
    case Accounts.sign_in(email, password) do
      {:ok, %{resource: user, token: token}} ->
        conn
        |> put_session(:user_token, token)
        |> assign(:current_user, user)
        |> put_flash(:info, "Successfully signed in!")
        |> redirect(to: "/businesses")
        |> ErrorHelpers.success()

      {:ok, user} when is_struct(user) ->
        # Handle case where AshAuthentication returns user without token wrapper
        token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

        conn
        |> put_session(:user_token, token)
        |> assign(:current_user, user)
        |> put_flash(:info, "Successfully signed in!")
        |> redirect(to: "/businesses")
        |> ErrorHelpers.success()

      {:error, reason} when is_binary(reason) ->
        IO.inspect(reason, label: "Sign in error")

        conn
        |> put_flash(:error, reason)
        |> redirect(to: "/sign-in")
        |> ErrorHelpers.failure(reason)

      {:error, error} ->
        IO.inspect(error, label: "Unexpected authentication error")

        conn
        |> put_flash(:error, "An error occurred during sign in. Please try again.")
        |> redirect(to: "/sign-in")
        |> ErrorHelpers.failure(error)
    end
  end

  def sign_out(conn, _params) do
    conn
    |> AuthHelpers.sign_out_user()
    |> put_flash(:info, "Successfully signed out!")
    |> redirect(to: "/sign-in")
  end

  def register(conn, _params) do
    render(conn, :register)
  end

  def register_submit(conn, %{
        "name" => name,
        "email" => email,
        "password" => password,
        "password_confirmation" => password_confirmation
      }) do
    # Validate password confirmation on the client side
    if password != password_confirmation do
      conn
      |> put_flash(:error, "Password confirmation does not match")
      |> redirect(to: "/register")
    else
      case Accounts.register(%{
        "name" => name,
        "email" => email,
        "password" => password
      }) do
        {:ok, _user} ->
          conn
          |> put_flash(:info, "Registration successful! Please sign in.")
          |> redirect(to: "/sign-in")
          |> ErrorHelpers.success()
        {:error, changeset} ->
          error_messages = format_changeset_errors(changeset)

          conn
          |> put_flash(:error, "Registration failed: #{error_messages}")
          |> redirect(to: "/register")
          |> ErrorHelpers.failure(changeset)
      end
    end
  end

  defp format_changeset_errors(%Ash.Error.Invalid{} = error) do
    error.errors
    |> Enum.map(&format_ash_error/1)
    |> Enum.join(", ")
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
