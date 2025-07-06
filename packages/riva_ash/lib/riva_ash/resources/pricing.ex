defmodule RivaAsh.Resources.Pricing do
  @moduledoc """
  Represents pricing rules for full-day reservations.
  Pricing is generally constant but can have business-specific exceptions.

  Pricing types:
  - base: Standard pricing for an item type
  - exception: Special pricing for specific dates or date ranges
  - seasonal: Pricing for specific seasons or periods
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [AshJsonApi.Resource, AshGraphql.Resource, AshArchival.Resource]

  postgres do
    table("pricing")
    repo(RivaAsh.Repo)
  end

  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Permission-based authorization for pricing management
    policy action_type([:create, :update, :destroy]) do
      authorize_if(RivaAsh.Policies.PermissionCheck.can_update_pricing())
    end

    # View pricing - either have view permission or update permission
    policy action_type(:read) do
      authorize_if(RivaAsh.Policies.PermissionCheck.can_view_pricing())
      authorize_if(RivaAsh.Policies.PermissionCheck.can_update_pricing())
    end
  end

  # Configure soft delete functionality
  archive do
    # Use archived_at field for soft deletes
    attribute(:archived_at)
    # Allow both soft and hard deletes
    base_filter?(false)
  end

  json_api do
    type("pricing")

    routes do
      base("/pricing")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for pricing specific actions
      get(:by_business, route: "/by-business/:business_id")
      get(:by_item_type, route: "/by-item-type/:item_type_id")
      get(:base_pricing, route: "/base")
      get(:exceptions, route: "/exceptions")
      get(:active, route: "/active")
      get(:for_date, route: "/for-date/:date")
    end
  end

  graphql do
    type(:pricing)

    queries do
      get(:get_pricing, :read)
      list(:list_pricing, :read)
      list(:pricing_by_business, :by_business)
      list(:pricing_by_item_type, :by_item_type)
      list(:base_pricing, :base_pricing)
      list(:pricing_exceptions, :exceptions)
      list(:active_pricing, :active)
      list(:pricing_for_date, :for_date)
    end

    mutations do
      create(:create_pricing, :create)
      update(:update_pricing, :update)
      destroy(:delete_pricing, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_business, args: [:business_id], action: :by_business)
    define(:by_item_type, args: [:item_type_id], action: :by_item_type)
    define(:base_pricing, action: :base_pricing)
    define(:exceptions, action: :exceptions)
    define(:active, action: :active)
    define(:for_date, args: [:date], action: :for_date)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([
        :business_id,
        :item_type_id,
        :pricing_type,
        :price_per_day,
        :currency,
        :effective_from,
        :effective_until,
        :is_active,
        :name,
        :description
      ])
      primary?(true)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(business_id == ^arg(:business_id)))
    end

    read :by_item_type do
      argument(:item_type_id, :uuid, allow_nil?: false)
      filter(expr(item_type_id == ^arg(:item_type_id)))
    end

    read :base_pricing do
      filter(expr(pricing_type == :base))
    end

    read :exceptions do
      filter(expr(pricing_type in [:exception, :seasonal]))
    end

    read :active do
      filter(expr(is_active == true and is_nil(archived_at)))
    end

    read :for_date do
      argument(:date, :date, allow_nil?: false)
      filter(expr(
        is_active == true and
        is_nil(archived_at) and
        (is_nil(effective_from) or effective_from <= ^arg(:date)) and
        (is_nil(effective_until) or effective_until >= ^arg(:date))
      ))
    end

    # Action to get effective pricing for a specific item type and date
    read :effective_pricing do
      argument(:business_id, :uuid, allow_nil?: false)
      argument(:item_type_id, :uuid, allow_nil?: false)
      argument(:date, :date, allow_nil?: false)

      filter(expr(
        business_id == ^arg(:business_id) and
        item_type_id == ^arg(:item_type_id) and
        is_active == true and
        is_nil(archived_at) and
        (is_nil(effective_from) or effective_from <= ^arg(:date)) and
        (is_nil(effective_until) or effective_until >= ^arg(:date))
      ))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(true)
      public?(true)
      description("Optional name for this pricing rule")
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("Description of this pricing rule")
    end

    attribute :pricing_type, :atom do
      constraints(one_of: [:base, :exception, :seasonal])
      default(:base)
      public?(true)
      description("Type of pricing: base (standard), exception (special dates), or seasonal")
    end

    attribute :price_per_day, :decimal do
      allow_nil?(false)
      public?(true)
      constraints(min: 0)
      description("Price for a full day reservation")
    end

    attribute :currency, :string do
      allow_nil?(false)
      default("USD")
      public?(true)
      constraints(max_length: 3)
      description("Currency code (ISO 4217, e.g., USD, EUR)")
    end

    attribute :effective_from, :date do
      allow_nil?(true)
      public?(true)
      description("Date from which this pricing is effective (null means always)")
    end

    attribute :effective_until, :date do
      allow_nil?(true)
      public?(true)
      description("Date until which this pricing is effective (null means indefinite)")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this pricing rule is currently active")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The business this pricing belongs to")
    end

    belongs_to :item_type, RivaAsh.Resources.ItemType do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The item type this pricing applies to")
    end

    has_many :payments, RivaAsh.Resources.Payment do
      destination_attribute(:pricing_id)
      public?(true)
      description("Payments that used this pricing")
    end
  end

  identities do
    # Ensure only one base pricing per business/item_type combination
    identity(:unique_base_pricing, [:business_id, :item_type_id, :pricing_type])
  end

  validations do
    validate(compare(:price_per_day, greater_than_or_equal_to: 0),
      message: "Price per day must be non-negative"
    )

    validate(compare(:effective_until, greater_than_or_equal_to: :effective_from),
      message: "Effective until date must be after effective from date"
    )

    validate(match(:currency, ~r/^[A-Z]{3}$/),
      message: "Currency must be a valid 3-letter ISO code (e.g., USD, EUR)"
    )
  end

  # TODO: Add calculations for pricing effectiveness
  # calculations do
  #   calculate :is_currently_effective, :boolean, expr(is_active == true) do
  #     public?(true)
  #     description("Whether this pricing is currently effective")
  #   end
  # end
end
