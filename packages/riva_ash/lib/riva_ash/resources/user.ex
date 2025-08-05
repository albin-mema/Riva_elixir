defmodule RivaAsh.Accounts.User do
  use Ash.Resource,
    domain: RivaAsh.Accounts,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [AshAuthentication]

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
    attribute(:role, :string, default: "user")

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

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      filter(expr(id == ^arg(:id)))
    end

    read :seed_read do
      # Special action for seeding - allows reading without authentication
      description("Read action for seeding purposes - bypasses normal authorization")
    end
  end

  # Define the relationships
  relationships do
    has_many :owned_businesses, RivaAsh.Resources.Business do
      destination_attribute(:owner_id)
      public?(true)
      description("Businesses owned by this user")
    end
  end

  # Define the validations
  validations do
    validate present([:email])
    validate one_of(:role, ["admin", "user", "superadmin"])
  end

  # Define authorization policies
  policies do
    # Superadmins can do everything (bypass all other policies)
    bypass actor_attribute_equals(:role, "superadmin") do
      authorize_if always()
    end

    # Allow seeding read action without authentication (bypass all other policies)
    bypass action(:seed_read) do
      authorize_if always()
    end

    # Allow public registration without authentication
    policy action(:register_with_password) do
      authorize_if always()
    end

    # Users can read their own profile
    policy action_type(:read) do
      authorize_if expr(id == ^actor(:id))
    end

    # Users can update their own profile (except role)
    policy action_type(:update) do
      authorize_if expr(id == ^actor(:id))
      forbid_if changing_attributes([:role])
    end

    # Block destroy actions (only superadmins can destroy users)
    policy action_type(:destroy) do
      forbid_if always()
    end
  end

  # Define the calculations for the User resource
  calculations do
    calculate(:is_admin, :boolean, expr(role == "admin"))
    calculate(:is_superadmin, :boolean, expr(role == "superadmin"))
  end
end
