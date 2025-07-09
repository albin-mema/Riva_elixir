defmodule RivaAsh.Accounts do
  use Ash.Domain

  resources do
    resource RivaAsh.Accounts.User
    resource RivaAsh.Accounts.Token
  end

  # Add any additional configuration or functions needed for authentication
  def sign_in(email, password) do
    RivaAsh.Accounts.User
    |> Ash.Changeset.for_action(:sign_in, %{"email" => email, "password" => password})
    |> Ash.create()
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
