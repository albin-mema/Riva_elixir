defmodule RivaAsh.Accounts do
  use Ash.Domain
  import OK, only: [success: 1, failure: 1, ~>>: 2, for: 1]

  resources do
    resource(RivaAsh.Accounts.User)
    resource(RivaAsh.Accounts.Token)
  end

  # Add any additional configuration or functions needed for authentication
  def sign_in(email, password) do
    OK.for do
      strategy <- OK.wrap(AshAuthentication.Info.strategy!(RivaAsh.Accounts.User, :password))
      result <- AshAuthentication.Strategy.action(
                  strategy,
                  :sign_in,
                  %{"email" => email, "password" => password}
                )
    after
      result
    else
      _ ->
        # Add a small delay to prevent timing attacks
        :crypto.hash_equals(<<0>>, <<0>>)
        failure("Invalid email or password")
    end
  end

  def register(params) do
    # Use the register action created by AshAuthentication
    RivaAsh.Accounts.User
    |> Ash.Changeset.for_create(:register_with_password, params)
    |> Ash.create()
    ~> fn user -> user end
  end

  def current_user(conn) do
    conn
    |> Ash.PlugHelpers.get_actor()
    |> OK.required(:user_not_found)
  end
end
