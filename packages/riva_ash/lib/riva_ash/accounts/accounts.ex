defmodule RivaAsh.Accounts do
  use Ash.Domain

  resources do
    resource(RivaAsh.Accounts.User)
    resource(RivaAsh.Accounts.Token)
  end

  # Add any additional configuration or functions needed for authentication
  def sign_in(email, password) do
    try do
      strategy = AshAuthentication.Info.strategy!(RivaAsh.Accounts.User, :password)

      case AshAuthentication.Strategy.action(
             strategy,
             :sign_in,
             %{"email" => email, "password" => password}
           ) do
        {:ok, result} -> {:ok, result}
        {:error, error} -> {:error, error}
      end
    rescue
      _ ->
        # Add a small delay to prevent timing attacks
        :crypto.hash_equals(<<0>>, <<0>>)
        {:error, "Invalid email or password"}
    end
  end

  def register(params) do
    # Use the register action created by AshAuthentication
    RivaAsh.Accounts.User
    |> Ash.Changeset.for_create(:register_with_password, params, domain: RivaAsh.Domain)
    |> Ash.create(domain: RivaAsh.Domain)
  end

  alias RivaAsh.ErrorHelpers

  def current_user(conn) do
    conn
    |> Ash.PlugHelpers.get_actor()
    |> ErrorHelpers.required(:user_not_found)
  end
end
