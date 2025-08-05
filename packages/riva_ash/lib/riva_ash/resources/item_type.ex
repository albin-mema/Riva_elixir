defmodule RivaAsh.Resources.ItemType do
  @moduledoc """
  Represents a type/category of items that can be reserved.
  Item types help categorize and organize different kinds of reservable resources.

  Examples:
  - Table (for restaurants)
  - Room (for hotels/meeting spaces)
  - Equipment (for rental businesses)
  - Court (for sports facilities)
  - Workstation (for coworking spaces)
  """

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          description: String.t() | nil,
          business_id: String.t(),
          color: String.t() | nil,
          icon: String.t() | nil,
          is_active: boolean(),
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource,
      AshAdmin.Resource
    ]

  import RivaAsh.ResourceHelpers

  standard_postgres("item_types")
  standard_archive()
  standard_admin([:name, :business, :description, :color, :icon, :is_active])

  # Authorization policies
  policies do
    # Admin bypass (works for both User and Employee actors)
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Business owner has full access to their business data
    policy action_type([:read, :create, :update, :destroy]) do
      authorize_if(expr(business.owner_id == ^actor(:id)))
    end

    # Employees with manager role can manage item types
    policy action_type([:create, :update]) do
      authorize_if(actor_attribute_equals(:role, :manager))
    end

    # Employees can read item types
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :employee))
    end

    # Staff employees can read item types
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :staff))
    end

    # Regular users can read item types (for business owners who are Users, not Employees)
    # This policy needs to check if the user owns businesses that have these item types
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :user))
    end
  end

  json_api do
    type("item_type")

    routes do
      base("/item-types")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for item type specific actions
      get(:by_business, route: "/by-business/:business_id")
      get(:active, route: "/active")
      get(:with_items, route: "/with-items")
    end
  end

  graphql do
    type(:item_type)

    queries do
      get(:get_item_type, :read)
      list(:list_item_types, :read)
      list(:item_types_by_business, :by_business)
      list(:active_item_types, :active)
      list(:item_types_with_items, :with_items)
    end

    mutations do
      create(:create_item_type, :create)
      update(:update_item_type, :update)
      destroy(:delete_item_type, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_business, args: [:business_id], action: :by_business)
    define(:active, action: :active)
    define(:with_items, action: :with_items)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :description, :business_id, :color, :icon, :is_active])
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

    read :active do
      filter(expr(is_active == true and is_nil(archived_at)))
    end

    read :with_items do
      # Load items relationship - handled by GraphQL automatically
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the item type (e.g., 'Table', 'Room', 'Equipment')")
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("A detailed description of this item type")
    end

    attribute :color, :string do
      allow_nil?(true)
      public?(true)
      description("Hex color code for visual representation (e.g., '#FF5733')")
    end

    attribute :icon, :string do
      allow_nil?(true)
      public?(true)
      description("Icon identifier for visual representation")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether this item type is currently active and can be used")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The business this item type belongs to")
    end

    has_many :items, RivaAsh.Resources.Item do
      destination_attribute(:item_type_id)
      public?(true)
      description("Items of this type")
    end

    has_many :pricing_rules, RivaAsh.Resources.Pricing do
      destination_attribute(:item_type_id)
      public?(true)
      description("Pricing rules for this item type")
    end
  end

  identities do
    identity(:unique_name_per_business, [:name, :business_id])
  end

  validations do
    validate(match(:color, ~r/^#[0-9A-Fa-f]{6}$/),
      message: "Color must be a valid hex color code (e.g., #FF5733)",
      where: [present(:color)]
    )
  end

  # Public helper functions
  @spec choices_for_select :: [{String.t(), String.t()}]
  def choices_for_select do
    __MODULE__
    |> Ash.read!()
    |> Enum.map(&{&1.id, display_name(&1)})
  end

  @spec get_business(String.t()) :: {:ok, RivaAsh.Resources.Business.t()} | {:error, String.t()}
  def get_business(item_type_id) do
    with {:ok, item_type} <- __MODULE__.by_id(item_type_id),
         {:ok, business} <- Ash.load(item_type, :business) do
      {:ok, business}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to load business: #{inspect(error)}"}
    end
  end

  @spec get_business_by_item_type_id(String.t()) :: {:ok, RivaAsh.Resources.Business.t()} | {:error, String.t()}
  def get_business_by_item_type_id(item_type_id) do
    case __MODULE__.by_id(item_type_id) do
      {:ok, item_type} -> get_business(item_type_id)
      {:error, reason} -> {:error, reason}
    end
  end

  # Private helper functions for filtering
  @spec apply_business_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_business_filter(query, nil), do: query

  defp apply_business_filter(query, business_id) do
    Ash.Query.filter(query, expr(business_id == ^business_id))
  end

  @spec apply_active_filter(Ash.Query.t()) :: Ash.Query.t()
  defp apply_active_filter(query) do
    Ash.Query.filter(query, expr(is_active == true and is_nil(archived_at)))
  end

  @spec apply_search_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_search_filter(query, nil), do: query

  defp apply_search_filter(query, search_term) do
    search_term = "%#{search_term}%"
    Ash.Query.filter(query,
      or: [
        expr(name like ^search_term),
        expr(description like ^search_term)
      ]
    )
  end

  # Private helper functions
  @spec display_name(__MODULE__.t()) :: String.t()
  defp display_name(item_type) do
    business_name =
      if item_type.business do
        item_type.business.name
      else
        "Unknown Business"
      end

    "#{item_type.name} (#{business_name})"
  end

  # Helper functions for business logic and data validation

  @doc """
  Checks if the item type is currently active (not archived).
  
  ## Parameters
  - item_type: The item type record to check
  
  ## Returns
  - `true` if the item type is active, `false` otherwise
  """
  @spec is_active?(t()) :: boolean()
  def is_active?(item_type) do
    with %{archived_at: nil} <- item_type do
      true
    else
      _ -> false
    end
  end

  @doc """
  Checks if the item type has a valid color specified.
  
  ## Parameters
  - item_type: The item type record to check
  
  ## Returns
  - `true` if color is valid, `false` otherwise
  """
  @spec has_valid_color?(t()) :: boolean()
  def has_valid_color?(item_type) do
    case item_type.color do
      nil -> true
      color when is_binary(color) -> String.match?(color, ~r/^#[0-9A-Fa-f]{6}$/)
      _ -> false
    end
  end

  @doc """
  Gets the display name of the item type with business information.
  
  ## Parameters
  - item_type: The item type record
  
  ## Returns
  - String with the formatted display name
  """
  @spec display_name(t()) :: String.t()
  def display_name(item_type) do
    business_name =
      if item_type.business do
        item_type.business.name
      else
        "Unknown Business"
      end

    "#{item_type.name} (#{business_name})"
  end

  @doc """
  Gets the formatted color information for display.
  
  ## Parameters
  - item_type: The item type record
  
  ## Returns
  - String with formatted color information
  """
  @spec color_info(t()) :: String.t()
  def color_info(item_type) do
    case item_type.color do
      nil -> "No color specified"
      color when is_binary(color) and String.match?(color, ~r/^#[0-9A-Fa-f]{6}$/) ->
        "Color: #{color}"
      _ ->
        "Invalid color format"
    end
  end

  @doc """
  Gets the icon information for display.
  
  ## Parameters
  - item_type: The item type record
  
  ## Returns
  - String with icon information or "No icon specified"
  """
  @spec icon_info(t()) :: String.t()
  def icon_info(item_type) do
    case item_type.icon do
      nil -> "No icon specified"
      icon when is_binary(icon) and icon != "" -> "Icon: #{icon}"
      _ -> "Invalid icon"
    end
  end

  @doc """
  Gets the item count for this item type.
  
  ## Parameters
  - item_type: The item type record
  
  ## Returns
  - Integer with the number of items of this type
  """
  @spec item_count(t()) :: integer()
  def item_count(item_type) do
    length(item_type.items || [])
  end

  @doc """
  Checks if the item type has any associated items.
  
  ## Parameters
  - item_type: The item type record to check
  
  ## Returns
  - `true` if items exist, `false` otherwise
  """
  @spec has_items?(t()) :: boolean()
  def has_items?(item_type), do: item_count(item_type) > 0

  @doc """
  Gets the pricing rule count for this item type.
  
  ## Parameters
  - item_type: The item type record
  
  ## Returns
  - Integer with the number of pricing rules
  """
  @spec pricing_rule_count(t()) :: integer()
  def pricing_rule_count(item_type) do
    length(item_type.pricing_rules || [])
  end

  @doc """
  Checks if the item type has any pricing rules.
  
  ## Parameters
  - item_type: The item type record to check
  
  ## Returns
  - `true` if pricing rules exist, `false` otherwise
  """
  @spec has_pricing_rules?(t()) :: boolean()
  def has_pricing_rules?(item_type), do: pricing_rule_count(item_type) > 0

  @doc """
  Validates that the item type has all required relationships.
  
  ## Parameters
  - item_type: The item type record to validate
  
  ## Returns
  - `{:ok, item_type}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_relationships(item_type) do
    cond do
      is_nil(item_type.business) ->
        {:error, "Business relationship is missing"}
      
      true ->
        {:ok, item_type}
    end
  end

  @doc """
  Formats the complete item type information for display.
  
  ## Parameters
  - item_type: The item type record
  
  ## Returns
  - String with complete item type information
  """
  @spec formatted_info(t()) :: String.t()
  def formatted_info(item_type) do
    with true <- is_active?(item_type),
         display_name <- display_name(item_type),
         description <- item_type.description || "No description",
         item_count <- item_count(item_type),
         color_info <- color_info(item_type),
         icon_info <- icon_info(item_type) do
      "#{display_name}: #{description} | Items: #{item_count} | #{color_info} | #{icon_info}"
    else
      false ->
        "Archived item type: #{display_name(item_type)}"
    end
  end

  @doc """
  Gets the business name associated with this item type.
  
  ## Parameters
  - item_type: The item type record
  
  ## Returns
  - String with the business name
  """
  @spec business_name(t()) :: String.t()
  def business_name(item_type) do
    case item_type.business do
      %{name: name} when is_binary(name) and name != "" -> name
      _ -> "Unknown business"
    end
  end

  @doc """
  Checks if the item type can be safely deleted.
  
  ## Parameters
  - item_type: The item type record to check
  
  ## Returns
  - `true` if can be deleted, `false` otherwise
  """
  @spec can_delete?(t()) :: boolean()
  def can_delete?(item_type) do
    not has_items?(item_type)
  end

  @doc """
  Gets the reason why the item type cannot be deleted.
  
  ## Parameters
  - item_type: The item type record to check
  
  ## Returns
  - String with deletion reason or empty string if can be deleted
  """
  @spec deletion_reason(t()) :: String.t()
  def deletion_reason(item_type) do
    if has_items?(item_type) do
      "Cannot delete item type with associated items"
    else
      ""
    end
  end

  @doc """
  Validates the item type data.
  
  ## Parameters
  - item_type: The item type record to validate
  
  ## Returns
  - `{:ok, item_type}` if valid
  - `{:error, reason}` if invalid
  """
  @spec validate_data(t()) :: {:ok, t()} | {:error, String.t()}
  def validate_data(item_type) do
    cond do
      is_nil(item_type.name) or item_type.name == "" ->
        {:error, "Item type name is required"}
      
      not has_valid_color?(item_type) ->
        {:error, "Invalid color format"}
      
      true ->
        {:ok, item_type}
    end
  end

  # Public helper functions
  @spec choices_for_select :: [{String.t(), String.t()}]
  def choices_for_select do
    __MODULE__
    |> Ash.read!()
    |> Enum.map(&{&1.id, display_name(&1)})
  end

  @spec get_business(String.t()) :: {:ok, RivaAsh.Resources.Business.t()} | {:error, String.t()}
  def get_business(item_type_id) do
    with {:ok, item_type} <- __MODULE__.by_id(item_type_id),
         {:ok, business} <- Ash.load(item_type, :business) do
      {:ok, business}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to load business: #{inspect(error)}"}
    end
  end

  @spec get_business_by_item_type_id(String.t()) :: {:ok, RivaAsh.Resources.Business.t()} | {:error, String.t()}
  def get_business_by_item_type_id(item_type_id) do
    case __MODULE__.by_id(item_type_id) do
      {:ok, item_type} -> get_business(item_type_id)
      {:error, reason} -> {:error, reason}
    end
  end

  # Private helper functions for filtering
  @spec apply_business_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_business_filter(query, nil), do: query

  defp apply_business_filter(query, business_id) do
    Ash.Query.filter(query, expr(business_id == ^business_id))
  end

  @spec apply_active_filter(Ash.Query.t()) :: Ash.Query.t()
  defp apply_active_filter(query) do
    Ash.Query.filter(query, expr(is_active == true and is_nil(archived_at)))
  end

  @spec apply_search_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_search_filter(query, nil), do: query

  defp apply_search_filter(query, search_term) do
    search_term = "%#{search_term}%"
    Ash.Query.filter(query,
      or: [
        expr(name like ^search_term),
        expr(description like ^search_term)
      ]
    )
  end

  # Private helper functions
  @spec display_name(__MODULE__.t()) :: String.t()
  defp display_name(item_type) do
    business_name =
      if item_type.business do
        item_type.business.name
      else
        "Unknown Business"
      end

    "#{item_type.name} (#{business_name})"
  end
end
