defmodule RivaAsh.Accounts do
  @moduledoc """
  Accounts domain for user authentication and management.
  """

  use Ash.Domain

  resources do
    resource(RivaAsh.Accounts.User) do
      define(:get_user_by_id, action: :seed_read, get_by: [:id])
    end

    resource(RivaAsh.Accounts.Token)
  end

  alias RivaAsh.ErrorHelpers

  @type email :: String.t()
  @type password :: String.t()
  @type user_params :: map()
  @type result :: {:ok, any()} | {:error, any()}

  @doc """
  Signs in a user with email and password.

  ## Parameters
    - email: User's email address
    - password: User's password

  ## Returns
    - {:ok, result}: Successful authentication
    - {:error, reason}: Authentication failed
  """
  @spec sign_in(email, password) :: result
  def sign_in(email, password) do
    with {:ok, strategy} <- get_authentication_strategy(),
         {:ok, result} <- execute_sign_in(strategy, email, password) do
      {:ok, result}
    else
      {:error, :strategy_not_found} ->
        handle_timing_attack()
        {:error, "Invalid email or password"}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Registers a new user with the provided parameters.

  ## Parameters
    - params: User registration parameters

  ## Returns
    - {:ok, user}: Successfully created user
    - {:error, changeset}: Registration failed
  """
  @spec register(user_params) :: result
  def register(params) do
    params
    |> validate_registration_params()
    |> build_registration_changeset()
    |> create_user()
  end

  @doc """
  Gets the current authenticated user from the connection.

  ## Parameters
    - conn: The connection containing user authentication

  ## Returns
    - {:ok, user}: Current user
    - {:error, reason}: User not found or authentication error
  """
  @spec current_user(Plug.Conn.t()) :: result
  def current_user(conn) do
    conn
    |> Ash.PlugHelpers.get_actor()
    |> ErrorHelpers.required(:user_not_found)
  end

  # Private functions for single level of abstraction

  defp get_authentication_strategy do
    case AshAuthentication.Info.strategy(RivaAsh.Accounts.User, :password) do
      nil -> {:error, :strategy_not_found}
      strategy -> {:ok, strategy}
    end
  end

  defp execute_sign_in(strategy, email, password) do
    case AshAuthentication.Strategy.action(
           strategy,
           :sign_in,
           %{"email" => email, "password" => password}
         ) do
      {:ok, result} -> {:ok, result}
      {:error, error} -> {:error, error}
    end
  end

  defp handle_timing_attack do
    # Add a small delay to prevent timing attacks
    :crypto.hash_equals(<<0>>, <<0>>)
  end

  defp validate_registration_params(params) when is_map(params) do
    required_fields = [:email, :name, :password]

    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      params
    else
      {:error, "Missing required registration fields: #{inspect(required_fields)}"}
    end
  end

  defp validate_registration_params(_params) do
    {:error, "Registration parameters must be a map"}
  end

  defp build_registration_changeset(params) do
    RivaAsh.Accounts.User
    |> Ash.Changeset.for_create(:register_with_password, params, domain: RivaAsh.Domain)
  end

  defp create_user(changeset) do
    changeset
    |> Ash.create(domain: RivaAsh.Domain)
  end
end
