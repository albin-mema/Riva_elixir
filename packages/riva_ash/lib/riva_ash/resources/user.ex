defmodule RivaAsh.Accounts.User do
  alias AshAuthentication.Strategy.Password.SignInPreparation

  use Ash.Resource,
    domain: RivaAsh.Accounts,
    data_layer: AshPostgres.DataLayer,
    extensions: [
      AshAuthentication,
      AshAuthentication.Strategy.Password
    ]

  postgres do
    table("users")
    repo(RivaAsh.Repo)
  end

  attributes do
    uuid_primary_key(:id)

    attribute(:email, :ci_string, allow_nil?: false, public?: true)
    attribute(:hashed_password, :string, allow_nil?: false, sensitive?: true)

    # Profile attributes
    attribute(:name, :string)
    attribute(:role, :atom, constraints: [one_of: [:admin, :user]], default: :user)

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  identities do
    identity(:unique_email, [:email])
  end

  @signing_secret Application.compile_env(
                    :riva_ash,
                    :signing_secret,
                    "default_secret_change_me_in_prod"
                  )

  authentication do
    domain(RivaAsh.Accounts)
    session_identifier(:jti)

    strategies do
      password :password do
        identity_field(:email)
        hashed_password_field(:hashed_password)
        sign_in_tokens_enabled?(true)
        confirmation_required?(false)
        register_action_accept([:name, :email, :role])

        # The sign_in action is automatically defined by AshAuthentication
        # No need to manually define it here
      end
    end

    tokens do
      enabled?(true)
      token_resource(RivaAsh.Accounts.Token)
      signing_secret(@signing_secret)
      require_token_presence_for_authentication?(true)
    end
  end

  # Define the actions for the User resource
  actions do
    defaults([:read, :update, :destroy])

    read :me do
      # Only returns the current user
      filter(expr(id == ^actor(:id)))
    end
  end

  # Define the relationships
  relationships do
    # Add any relationships here
  end

  # Define the validations
  validations do
    validate(present([:email]))
  end

  # Define the calculations for the User resource
  calculations do
    calculate(:is_admin, :boolean, expr(role == :admin))
  end
end
