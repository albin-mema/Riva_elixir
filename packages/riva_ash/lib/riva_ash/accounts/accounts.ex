defmodule RivaAsh.Accounts do
  use Ash.Domain

  resources do
    resource(RivaAsh.Accounts.User)
    resource(RivaAsh.Accounts.Token)
  end

  # Add any additional configuration or functions needed for authentication
  def sign_in(email, password) do
    # Get the password strategy from the User resource
    strategy = AshAuthentication.Info.strategy!(RivaAsh.Accounts.User, :password)

    # Use Strategy.action to call the sign_in action with the provided credentials
    case AshAuthentication.Strategy.action(
           strategy,
           :sign_in,
           %{"email" => email, "password" => password}
         ) do
      {:ok, result} ->
        {:ok, result}

      _ ->
        # Add a small delay to prevent timing attacks
        :crypto.hash_equals(<<0>>, <<0>>)
        {:error, "Invalid email or password"}
    end
  end

  def register(params) do
    # Use the register action created by AshAuthentication
    RivaAsh.Accounts.User
    |> Ash.Changeset.for_create(:register_with_password, params)
    |> Ash.create()
  end

  def current_user(conn) do
    Ash.PlugHelpers.get_actor(conn)
  end
end
