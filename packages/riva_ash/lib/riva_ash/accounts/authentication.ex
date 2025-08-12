defmodule RivaAsh.Accounts.Authentication do
  @moduledoc """
  Authentication service for user login and verification.
  
  This module provides core authentication functionality including
  user lookup, password verification, and session management.
  """

  alias RivaAsh.Accounts.User
  alias RivaAsh.ErrorHelpers

  @type email :: String.t()
  @type password :: String.t()
  @type context :: map()
  @type result :: {:ok, map()} | {:error, String.t()}

  @doc """
  Authenticates a user with the provided credentials.

  ## Parameters
    - email: User's email address
    - password: User's password
    - context: Additional authentication context (e.g., request metadata)
    - opts: Additional authentication options

  ## Returns
    - {:ok, user}: Successfully authenticated user with token
    - {:error, reason}: Authentication failed
  """
  @spec authenticate(email, password, context, keyword()) :: result
  def authenticate(email, password, context \\ %{}, opts \\ []) do
    with {:ok, user} <- find_user_by_email(email),
         {:ok, verified} <- verify_password(user, password),
         {:ok, auth_result} <- generate_auth_token(verified, context, opts) do
      {:ok, auth_result}
    else
      {:error, :user_not_found} ->
        handle_timing_attack()
        {:error, "Invalid email or password"}

      {:error, :invalid_password} ->
        handle_timing_attack()
        {:error, "Invalid email or password"}

      {:error, reason} ->
        handle_auth_error(reason, context)
    end
  end

  # Private functions for single level of abstraction

  defp find_user_by_email(email) when is_binary(email) do
    case User
         |> Ash.Query.filter(email: email)
         |> Ash.Query.load([:password_hash])
         |> Ash.read(domain: RivaAsh.Domain) do
      {:ok, [user | _]} -> {:ok, user}
      {:ok, []} -> {:error, :user_not_found}
      {:error, error} -> {:error, error}
    end
  end

  defp find_user_by_email(_email) do
    {:error, :invalid_email_format}
  end

  defp verify_password(user, password) when is_binary(password) do
    case Bcrypt.verify_pass(password, user.password_hash) do
      true -> {:ok, user}
      false -> {:error, :invalid_password}
    end
  end

  defp verify_password(_user, _password) do
    {:error, :invalid_password_format}
  end

  defp generate_auth_token(user, context, opts) do
    token_opts = Keyword.get(opts, :token_opts, [])
    expires_in = Keyword.get(token_opts, :expires_in, 24 * 60 * 60) # 24 hours

    case AshAuthentication.Token.sign(RivaAsh.Accounts.User, user, token_opts) do
      {:ok, token} ->
        auth_result = %{
          user: Map.take(user, [:id, :email, :name]),
          token: token,
          expires_at: DateTime.add(DateTime.utc_now(), expires_in, :second),
          context: context
        }
        {:ok, auth_result}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_timing_attack do
    # Add a small delay to prevent timing attacks
    :crypto.hash_equals(<<0>>, <<0>>)
  end

  defp handle_auth_error(:user_not_found, _context) do
    {:error, "Authentication failed"}
  end

  defp handle_auth_error(:invalid_password, _context) do
    {:error, "Authentication failed"}
  end

  defp handle_auth_error({:validation_failed, changeset}, _context) do
    error_messages = Ash.Changeset.errors(changeset)
    |> Enum.map(&format_validation_error/1)
    {:error, "Validation failed: #{Enum.join(error_messages, ", ")}"}
  end

  defp handle_auth_error(error, context) do
    Logger.error("Authentication error: #{inspect(error)}, context: #{inspect(context)}")
    {:error, "Authentication failed due to an internal error"}
  end

  defp format_validation_error({message, vars}) when is_list(vars) do
    String.replace(message, "%{", "%%{") |> :io_lib.format(vars) |> IO.iodata_to_binary()
  end

  defp format_validation_error(message) when is_binary(message) do
    message
  end

  defp format_validation_error(error) do
    inspect(error)
  end
end