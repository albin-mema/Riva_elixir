defmodule RivaAsh.Accounts.User do
  alias AshAuthentication.Strategy.Password.SignInPreparation

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshAuthentication,
      AshAuthentication.Strategy.Password,
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource,
      AshAdmin.Resource
    ]

  postgres do
    table("users")
    repo(RivaAsh.Repo)
  end

  # Configure soft delete functionality
  archive do
    attribute(:archived_at)
    base_filter?(false)
  end

  attributes do
    uuid_primary_key(:id)

    attribute(:email, :ci_string, allow_nil?: false, public?: true)
    attribute(:hashed_password, :string, allow_nil?: false, sensitive?: true)

    # Profile attributes
    attribute(:name, :string)
    attribute(:role, :atom, constraints: [one_of: [:admin, :user, :superadmin]], default: :user)

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
    domain(RivaAsh.Domain)
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
    has_many :owned_businesses, RivaAsh.Resources.Business do
      destination_attribute(:owner_id)
      public?(true)
      description("Businesses owned by this user")
    end
  end

  # Define the validations
  validations do
    validate(present([:email]))
  end

  # Define authorization policies
  policies do
    # Superadmins can do everything
    policy action_type([:create, :read, :update, :destroy]) do
      authorize_if expr(actor(:role) == :superadmin)
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

    # Only superadmins can create users or destroy them
    policy action_type([:create, :destroy]) do
      authorize_if expr(actor(:role) == :superadmin)
    end
  end

  # Define the calculations for the User resource
  calculations do
    calculate(:is_admin, :boolean, expr(role == :admin))
    calculate(:is_superadmin, :boolean, expr(role == :superadmin))
  end
end
