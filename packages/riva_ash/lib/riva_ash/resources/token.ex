alias RivaAsh.Accounts, as: Accounts

defmodule RivaAsh.Accounts.Token do
  @moduledoc """
  Token resource for user authentication and session management.

  This module defines the Token resource using AshAuthentication.TokenResource
  for handling user authentication tokens and sessions.
  """

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

  # If using policies, add authorization for the token resource
  # policies do
  #   policy always() do
  #     authorize_if always()
  #   end
  # end
end
