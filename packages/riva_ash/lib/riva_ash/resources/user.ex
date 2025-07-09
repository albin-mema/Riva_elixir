defmodule RivaAsh.Accounts.User do
  use Ash.Resource,
    domain: RivaAsh.Accounts,
    data_layer: AshPostgres.DataLayer,
    extensions: [
      AshAuthentication,
      AshAuthentication.Strategy.Password
    ]

  postgres do
    table "users"
    repo RivaAsh.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :email, :ci_string, allow_nil?: false, public?: true
    attribute :hashed_password, :string, allow_nil?: false, sensitive?: true

    # Profile attributes
    attribute :name, :string
    attribute :role, :atom, constraints: [one_of: [:admin, :user]], default: :user

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  identities do
    identity :unique_email, [:email]
  end

  authentication do
    domain RivaAsh.Accounts

    strategies do
      password :password do
        identity_field :email
        hashed_password_field :hashed_password
        sign_in_tokens_enabled? false
        confirmation_required? false
        register_action_accept [:name, :email, :role]
      end
    end

    tokens do
      enabled? false
      token_resource RivaAsh.Accounts.Token
    end
  end

  # Define the actions for the User resource
  actions do
    defaults [:read, :update, :destroy]

    # Add the sign_in action
    create :sign_in do
      # The sign_in action is provided by AshAuthentication
      # and does not need any additional configuration here.
      # It will automatically use the password strategy.
      # For more information, see:
      # https://ash-hq.org/docs/ash_authentication/latest/ash-authentication-password/sign-in-action
    end

    read :me do
      # Only returns the current user
      filter expr(id == ^actor(:id))
    end
  end

  # Define the relationships
  relationships do
    # Add any relationships here
  end

  # Define the validations
  validations do
    validate present([:email])
  end

  # Define the calculations for the User resource
  calculations do
    calculate :is_admin, :boolean, expr(role == :admin)
  end


end
