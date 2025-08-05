defmodule RivaAsh.Accounts.Token do
  use Ash.Resource,
    domain: RivaAsh.Accounts,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication.TokenResource]

  postgres do
    table("user_tokens")
    repo(RivaAsh.Repo)
  end

  token do
    domain(RivaAsh.Domain)
  end

  # Private helper functions for Single Level of Abstraction
  defp configure_token_domain() do
    domain(RivaAsh.Domain)
  end

  # If using policies, add authorization for the token resource
  # policies do
  #   policy always() do
  #     authorize_if always()
  #   end
  # end
end
