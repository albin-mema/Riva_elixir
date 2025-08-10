defmodule RivaAsh.Resources.Pricing do
  @moduledoc """
  Pricing resource for managing pricing rules and configurations.
  
  This resource handles various pricing strategies including:
  - Base pricing
  - Dynamic pricing
  - Promotional pricing
  - Tiered pricing
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource,
      AshAdmin.Resource
    ]

  import RivaAsh.ResourceHelpers

  # Define the type at the module level
  @type t :: %__MODULE__{
          id: String.t(),
          business_id: String.t(),
          item_id: String.t(),
          item_type_id: String.t(),
          base_price: float(),
          currency: String.t(),
          pricing_type: :base | :dynamic | :promotional | :tiered,
          name: String.t(),
          description: String.t() | nil,
          is_active: boolean(),
          start_date: Date.t() | nil,
          end_date: Date.t() | nil,
          demand_factor: float() | nil,
          time_factor: float() | nil,
          seasonal_factor: float() | nil,
          original_price: float() | nil,
          discount_type: :percentage | :fixed | :buy_x_get_y | nil,
          discount_value: float() | map() | nil,
          tiers: [map()] | nil,
          pricing_rules: [map()] | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t()
        }

  standard_postgres("pricings")
  standard_paper_trail()

  policies do
    # Business owners can manage their own pricing
    policy action_type([:create, :update, :destroy]) do
      authorize_if(expr(business_id == ^actor(:business_id)))
    end

    # Business owners can read their own pricing
    policy action_type(:read) do
      authorize_if(expr(business_id == ^actor(:business_id)))
    end

    # Admins can access all pricing
    policy action_type([:create, :read, :update, :destroy]) do
      authorize_if(actor_attribute_equals(:role, :admin))
    end
  end

  json_api do
    type("pricing")

    routes do
      base("/pricings")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes
      get(:active, route: "/active")
      get(:by_item_type, route: "/by-item-type")
      get(:by_business, route: "/by-business")
    end
  end

  graphql do
    type(:pricing)

    queries do
      get(:get_pricing, :read)
      list(:list_pricings, :read)
      list(:active_pricings, :active)
      list(:pricing_by_item_type, :by_item_type)
      list(:pricing_by_business, :by_business)
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
    define(:active, action: :active)
    define(:by_item_type, args: [:item_type_id], action: :by_item_type)
    define(:by_business, args: [:business_id], action: :by_business)
  end

  actions do
    defaults([:read, :destroy])

    create :create do
      accept([
        :business_id,
        :item_id,
        :item_type_id,
        :base_price,
        :currency,
        :pricing_type,
        :name,
        :description,
        :is_active,
        :start_date,
        :end_date,
        :demand_factor,
        :time_factor,
        :seasonal_factor,
        :original_price,
        :discount_type,
        :discount_value,
        :tiers,
        :pricing_rules
      ])

      primary?(true)

      validate(present([:business_id, :item_id, :base_price, :pricing_type, :name]),
        message: "Business ID, item ID, base price, pricing type, and name are required"
      )


      validate(one_of(:pricing_type, [:base, :dynamic, :promotional, :tiered]),
        message: "Invalid pricing type"
      )
    end

    update :update do
      accept([
        :base_price,
        :currency,
        :pricing_type,
        :name,
        :description,
        :is_active,
        :start_date,
        :end_date,
        :demand_factor,
        :time_factor,
        :seasonal_factor,
        :original_price,
        :discount_type,
        :discount_value,
        :tiers,
        :pricing_rules
      ])

      primary?(true)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :active do
      filter(expr(is_active == true and is_nil(archived_at)))
    end

    read :by_item_type do
      argument(:item_type_id, :uuid, allow_nil?: false)
      filter(expr(item_type_id == ^arg(:item_type_id) and is_active == true and is_nil(archived_at)))
    end

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(business_id == ^arg(:business_id) and is_nil(archived_at)))
    end

    # Custom action to validate pricing configuration
    action :validate_pricing_config do
      argument(:pricing_params, :map, allow_nil?: false)

      run(fn input, _context ->
        pricing_params = input.arguments.pricing_params
        
        case validate_pricing_configuration(pricing_params) do
          :ok -> {:ok, input}
          {:error, reason} -> {:error, reason}
        end
      end)
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :business_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("The ID of the business this pricing belongs to")
    end

    attribute :item_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("The ID of the item this pricing applies to")
    end

    attribute :item_type_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("The ID of the item type this pricing applies to")
    end

    attribute :base_price, :decimal do
      allow_nil?(false)
      public?(true)
      description("The base price for this item")
      constraints(scale: 2, precision: 10)
    end

    attribute :currency, :string do
      allow_nil?(false)
      default("USD")
      public?(true)
      description("Currency code for the pricing")
      constraints(match: ~r/^[A-Z]{3}$/)
    end

    attribute :pricing_type, :atom do
      allow_nil?(false)
      default(:base)
      public?(true)
      description("Type of pricing strategy")
      constraints(one_of: [:base, :dynamic, :promotional, :tiered])
    end

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("Name of the pricing rule")
      constraints(max_length: 100, trim?: true)
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("Description of the pricing rule")
      constraints(max_length: 500, trim?: true)
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this pricing rule is currently active")
    end

    attribute :start_date, :date do
      allow_nil?(true)
      public?(true)
      description("Start date for this pricing rule")
    end

    attribute :end_date, :date do
      allow_nil?(true)
      public?(true)
      description("End date for this pricing rule")
    end

    attribute :demand_factor, :decimal do
      allow_nil?(true)
      public?(true)
      description("Demand factor for dynamic pricing")
      constraints(scale: 2, precision: 5)
    end

    attribute :time_factor, :decimal do
      allow_nil?(true)
      public?(true)
      description("Time factor for dynamic pricing")
      constraints(scale: 2, precision: 5)
    end

    attribute :seasonal_factor, :decimal do
      allow_nil?(true)
      public?(true)
      description("Seasonal factor for dynamic pricing")
      constraints(scale: 2, precision: 5)
    end

    attribute :original_price, :decimal do
      allow_nil?(true)
      public?(true)
      description("Original price before discount (for promotional pricing)")
      constraints(scale: 2, precision: 10)
    end

    attribute :discount_type, :atom do
      allow_nil?(true)
      public?(true)
      description("Type of discount for promotional pricing")
      constraints(one_of: [:percentage, :fixed, :buy_x_get_y])
    end

    attribute :discount_value, :map do
      allow_nil?(true)
      public?(true)
      description("Discount value for promotional pricing")
    end

    attribute :tiers, :map do
      allow_nil?(true)
      public?(true)
      description("Tier configuration for tiered pricing")
    end

    attribute :pricing_rules, :map do
      allow_nil?(true)
      public?(true)
      description("Additional pricing rules")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      domain(RivaAsh.Domain)
      destination_attribute(:owner_id)
      public?(true)
    end

    belongs_to :item, RivaAsh.Resources.Item do
      domain(RivaAsh.Domain)
      destination_attribute(:id)
      public?(true)
    end

    belongs_to :item_type, RivaAsh.Resources.ItemType do
      domain(RivaAsh.Domain)
      destination_attribute(:id)
      public?(true)
    end

    has_many :payments, RivaAsh.Resources.Payment do
      domain(RivaAsh.Domain)
      destination_attribute(:pricing_id)
      public?(true)
    end
  end

  identities do
    identity(:unique_business_item, [:business_id, :item_id])
  end

  # Validation functions
  defp validate_pricing_configuration(changeset) do
    pricing_type = Ash.Changeset.get_attribute(changeset, :pricing_type)
    
    case pricing_type do
      :dynamic ->
        validate_dynamic_pricing(changeset)
      :promotional ->
        validate_promotional_pricing(changeset)
      :tiered ->
        validate_tiered_pricing(changeset)
      :base ->
        :ok
      _ ->
        {:error, "Invalid pricing type"}
    end
  end

  defp validate_dynamic_pricing(changeset) do
    demand_factor = Ash.Changeset.get_attribute(changeset, :demand_factor)
    time_factor = Ash.Changeset.get_attribute(changeset, :time_factor)
    
    cond do
      is_nil(demand_factor) or demand_factor < 0 ->
        {:error, "Demand factor must be a non-negative number"}
      is_nil(time_factor) or time_factor < 0 ->
        {:error, "Time factor must be a non-negative number"}
      true ->
        :ok
    end
  end

  defp validate_promotional_pricing(changeset) do
    original_price = Ash.Changeset.get_attribute(changeset, :original_price)
    discount_type = Ash.Changeset.get_attribute(changeset, :discount_type)
    discount_value = Ash.Changeset.get_attribute(changeset, :discount_value)
    
    cond do
      is_nil(original_price) or original_price <= 0 ->
        {:error, "Original price must be a positive number"}
      is_nil(discount_type) ->
        {:error, "Discount type is required for promotional pricing"}
      is_nil(discount_value) ->
        {:error, "Discount value is required for promotional pricing"}
      discount_type == :percentage and (discount_value < 0 or discount_value > 100) ->
        {:error, "Percentage discount must be between 0 and 100"}
      discount_type == :fixed and discount_value < 0 ->
        {:error, "Fixed discount cannot be negative"}
      true ->
        :ok
    end
  end

  defp validate_tiered_pricing(changeset) do
    tiers = Ash.Changeset.get_attribute(changeset, :tiers)
    
    cond do
      is_nil(tiers) or not is_list(tiers) ->
        {:error, "Tiers must be a list"}
      length(tiers) == 0 ->
        {:error, "At least one tier is required"}
      true ->
        validate_tiers_structure(tiers)
    end
  end

  defp validate_tiers_structure(tiers) do
    case Enum.all?(tiers, &validate_tier/1) do
      true -> :ok
      false -> {:error, "Invalid tier structure"}
    end
  end

  defp validate_tier(tier) do
    case tier do
      %{min_quantity: min_quantity, price_per_unit: price_per_unit} 
      when is_number(min_quantity) and is_number(price_per_unit) and min_quantity >= 0 and price_per_unit >= 0 ->
        true
      _ ->
        false
    end
  end


  @doc """
  Validates pricing parameters.
  
  ## Parameters
  - params: Parameters to validate
  
  ## Returns
  :ok or {:error, reason}
  """
  @spec validate_pricing_params(map()) :: :ok | {:error, String.t()}
  def validate_pricing_params(params) do
    required_fields = [:business_id, :item_id, :base_price, :pricing_type, :name]
    
    case validate_required_fields(params, required_fields) do
      :ok ->
        with :ok <- validate_base_price(params),
             :ok <- validate_currency(params),
             :ok <- validate_pricing_type(params),
             :ok <- validate_pricing_period(params) do
          :ok
        else
          {:error, reason} -> {:error, reason}
        end
      
      {:error, reason} -> {:error, reason}
    end
  end

  # Private helper functions
  defp validate_required_fields(params, required_fields) do
    missing_fields = Enum.filter(required_fields, &(!Map.has_key?(params, &1)))
    
    case length(missing_fields) do
      0 -> :ok
      _ -> {:error, "Missing required fields: #{inspect(missing_fields)}"}
    end
  end

  defp validate_base_price(params) do
    case Map.get(params, :base_price) do
      base_price when is_number(base_price) and base_price > 0 -> :ok
      _ -> {:error, "Base price must be a positive number"}
    end
  end

  defp validate_currency(params) do
    case Map.get(params, :currency) do
      currency when is_binary(currency) and byte_size(currency) == 3 -> :ok
      nil -> :ok
      _ -> {:error, "Currency must be a 3-letter code"}
    end
  end

  defp validate_pricing_type(params) do
    case Map.get(params, :pricing_type) do
      type when type in [:base, :dynamic, :promotional, :tiered] -> :ok
      _ -> {:error, "Invalid pricing type"}
    end
  end

  defp validate_pricing_period(params) do
    start_date = Map.get(params, :start_date)
    end_date = Map.get(params, :end_date)
    
    case {start_date, end_date} do
      {nil, nil} -> :ok
      {start_date, end_date} when is_struct(start_date, Date) and is_struct(end_date, Date) ->
        if Date.compare(start_date, end_date) in [:lt, :eq] do
          :ok
        else
          {:error, "Start date must be before or equal to end date"}
        end
      _ ->
        {:error, "Invalid date format"}
    end
  end
end