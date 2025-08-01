defmodule RivaAsh.GDPR.ConsentRecord do
  @moduledoc """
  Tracks user consent for various data processing purposes as required by GDPR.

  This resource maintains a complete audit trail of consent given, withdrawn,
  or modified by users for different processing purposes such as marketing,
  analytics, or other non-essential data processing activities.

  GDPR Articles: 6 (Lawfulness), 7 (Conditions for consent), 13-14 (Information)
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshPaperTrail.Resource,
      AshAdmin.Resource
    ]

  import RivaAsh.ResourceHelpers

  standard_postgres("consent_records")
  standard_paper_trail()

  # Authorization policies
  policies do
    # Admin bypass
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Users can only access their own consent records
    policy action_type([:read, :create, :update]) do
      authorize_if(expr(user_id == ^actor(:id)))
    end

    # Business owners can view consent records for their business context
    policy action_type(:read) do
      authorize_if(expr(business_id == ^actor(:business_id)))
    end
  end

  json_api do
    type("consent_record")

    routes do
      base("/consent-records")
      get(:read)
      index(:read)
      post(:give_consent)
      patch(:update)
    end
  end

  actions do
    defaults([:read, :create, :update])

    create :give_consent do
      accept([:user_id, :business_id, :purpose, :consent_version, :ip_address, :user_agent])
      primary?(true)

      change(fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:consent_given, true)
        |> Ash.Changeset.force_change_attribute(:consent_date, DateTime.utc_now())
      end)

      description("Record user consent for a specific processing purpose")
    end

    create :withdraw_consent do
      accept([:user_id, :business_id, :purpose, :ip_address, :user_agent])

      change(fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:consent_given, false)
        |> Ash.Changeset.force_change_attribute(:consent_date, DateTime.utc_now())
        |> Ash.Changeset.force_change_attribute(:withdrawal_date, DateTime.utc_now())
      end)

      description("Record user consent withdrawal")
    end

    read :by_user do
      argument(:user_id, :uuid, allow_nil?: false)
      filter(expr(user_id == ^arg(:user_id)))
      description("Get all consent records for a user")
    end

    read :by_purpose do
      argument(:purpose, :string, allow_nil?: false)
      filter(expr(purpose == ^arg(:purpose)))
      description("Get consent records for a specific purpose")
    end

    read :active_consents do
      filter(expr(consent_given == true and is_nil(withdrawal_date)))
      description("Get all active consent records")
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :user_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("The user who gave or withdrew consent")
    end

    attribute :business_id, :uuid do
      allow_nil?(true)
      public?(true)
      description("Business context for the consent (if applicable)")
    end

    attribute :purpose, :atom do
      allow_nil?(false)
      public?(true)

      constraints(
        one_of: [
          :marketing_emails,
          :analytics_tracking,
          :performance_monitoring,
          :third_party_integrations,
          :data_sharing,
          :automated_decision_making,
          :profiling,
          :cross_border_transfer
        ]
      )

      description("The specific purpose for which consent was given")
    end

    attribute :consent_given, :boolean do
      allow_nil?(false)
      public?(true)
      description("Whether consent was given (true) or withdrawn (false)")
    end

    attribute :consent_date, :utc_datetime do
      allow_nil?(false)
      public?(true)
      description("When the consent was given or withdrawn")
    end

    attribute :withdrawal_date, :utc_datetime do
      allow_nil?(true)
      public?(true)
      description("When consent was withdrawn (if applicable)")
    end

    attribute :consent_version, :string do
      allow_nil?(false)
      public?(true)
      description("Version of privacy policy/terms when consent was given")
    end

    attribute :ip_address, :string do
      allow_nil?(true)
      public?(false)
      description("IP address when consent was given (for audit purposes)")
    end

    attribute :user_agent, :string do
      allow_nil?(true)
      public?(false)
      description("User agent when consent was given (for audit purposes)")
    end

    attribute :consent_method, :atom do
      allow_nil?(false)
      default(:web_form)
      public?(true)
      constraints(one_of: [:web_form, :api, :email, :phone, :in_person, :import])
      description("How the consent was collected")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :user, RivaAsh.Accounts.User do
      allow_nil?(false)
      public?(true)
      description("The user who gave consent")
    end

    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(true)
      public?(true)
      description("Business context for the consent")
    end
  end

  identities do
    # Ensure we can track consent history properly
    identity(:unique_user_purpose_date, [:user_id, :purpose, :consent_date])
  end

  calculations do
    calculate :is_active, :boolean, expr(consent_given == true and is_nil(withdrawal_date)) do
      public?(true)
      description("Whether this consent record represents active consent")
    end

    calculate :days_since_consent,
              :integer,
              expr(fragment("EXTRACT(DAY FROM (NOW() - ?))", consent_date)) do
      public?(true)
      description("Number of days since consent was given")
    end
  end

  # Code interface for easy programmatic access
  code_interface do
    define(:give_consent, action: :give_consent)
    define(:withdraw_consent, action: :withdraw_consent)
    define(:by_user, args: [:user_id], action: :by_user)
    define(:by_purpose, args: [:purpose], action: :by_purpose)
    define(:active_consents, action: :active_consents)
  end

  # Flop configuration for admin interface
  @derive {
    Flop.Schema,
    filterable: [:user_id, :business_id, :purpose, :consent_given, :consent_date],
    sortable: [:consent_date, :purpose, :user_id],
    default_order: %{
      order_by: [:consent_date],
      order_directions: [:desc]
    },
    default_limit: 50,
    max_limit: 200
  }
end
