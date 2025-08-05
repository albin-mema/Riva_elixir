defmodule RivaAsh.Resources.Business do
  @moduledoc """
  Represents a business entity that can have multiple sections.
  Each business is a top-level organizational unit.
  """

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

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          description: String.t() | nil,
          owner_id: String.t(),
          is_public_searchable: boolean(),
          public_description: String.t() | nil,
          city: String.t() | nil,
          country: String.t() | nil,
          address: String.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  standard_postgres("businesses")
  standard_archive()
  standard_paper_trail()

  policies do
    # Any authenticated user can create a business (they become the owner)
    policy action_type(:create) do
      authorize_if(actor_present())
    end

    # Admins can update or delete any business
    policy action_type([:update, :destroy]) do
      authorize_if(actor_attribute_equals(:role, :admin))
    end

    # Business owners can update or delete their own businesses
    policy action_type([:update, :destroy]) do
      authorize_if(expr(owner_id == ^actor(:id)))
    end

    # Users can see their own businesses
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :admin))
      authorize_if(expr(owner_id == ^actor(:id)))
    end

    # Public search action requires no authentication
    policy action(:public_search) do
      authorize_if(always())
    end
  end

  json_api do
    type("business")

    routes do
      base("/businesses")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for business-specific actions
      get(:active, route: "/active")
      get(:inactive, route: "/inactive")
      get(:with_sections, route: "/with-sections")
      get(:with_employees, route: "/with-employees")
    end
  end

  graphql do
    type(:business)

    queries do
      get(:get_business, :read)
      list(:list_businesses, :read)
      list(:active_businesses, :active)
      list(:inactive_businesses, :inactive)
      get(:business_with_sections, :with_sections)
    end

    mutations do
      create(:create_business, :create)
      update(:update_business, :update)
      destroy(:delete_business, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:active, action: :active)
    define(:inactive, action: :inactive)
    define(:with_sections, action: :with_sections)
    define(:with_employees, action: :with_employees)
  end

  actions do
    defaults([:read, :destroy])

    update :update do
      accept([
        :name,
        :description,
        :is_public_searchable,
        :public_description,
        :city,
        :country,
        :address
      ])

      primary?(true)

      validate(present([:name]), message: "Business name is required")

      validate(match(:name, ~r/^[a-zA-Z0-9\s\-_&.]+$/),
        message: "Business name contains invalid characters"
      )
    end

    create :create do
      accept([
        :name,
        :description,
        :owner_id,
        :is_public_searchable,
        :public_description,
        :city,
        :country,
        :address
      ])

      primary?(true)

      validate(present([:name]), message: "Business name is required")

      validate(match(:name, ~r/^[a-zA-Z0-9\s\-_&.]+$/),
        message: "Business name contains invalid characters"
      )
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :active do
      # For now, all non-archived businesses are considered active
      # This can be enhanced later with an is_active field if needed
      filter(expr(is_nil(archived_at)))
    end

    read :inactive do
      # Archived businesses are considered inactive
      filter(expr(not is_nil(archived_at)))
    end

    read :with_sections do
      # Load sections relationship - this will be handled by GraphQL automatically
    end

    read :public_search do
      # Public search action for unregistered users
      # No authorization required, only returns publicly searchable businesses
      filter(expr(is_public_searchable == true and is_nil(archived_at)))

      # Allow searching by name, description, and location
      argument(:search_term, :string, allow_nil?: true)
      argument(:city, :string, allow_nil?: true)
      argument(:country, :string, allow_nil?: true)

      prepare(fn query, _context ->
        query
        |> apply_search_filter(Ash.Query.get_argument(query, :search_term))
        |> apply_location_filter(
          Ash.Query.get_argument(query, :city),
          Ash.Query.get_argument(query, :country)
        )
        |> apply_active_filter()
      end)
    end

    read :by_owner do
      argument(:owner_id, :uuid, allow_nil?: false)
      filter(expr(owner_id == ^arg(:owner_id)))
    end

    read :with_employees do
      # Load employees through sections (if needed) or directly if relationship exists
      # For now, this is a placeholder that can be enhanced
    end

    action :create_complete_setup, :struct do
      constraints(instance_of: RivaAsh.Resources.Item)
      argument(:business_name, :string, allow_nil?: false)
      argument(:business_description, :string, allow_nil?: true)
      argument(:section_name, :string, allow_nil?: false)
      argument(:section_description, :string, allow_nil?: true)
      argument(:item_name, :string, allow_nil?: false)
      argument(:item_description, :string, allow_nil?: true)
      argument(:item_capacity, :integer, allow_nil?: false, default: 1)
      run(RivaAsh.Reactors.ExampleReactor)
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the business")
      constraints(min_length: 2, max_length: 100, trim?: true)
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("A detailed description of the business")
      constraints(max_length: 1000, trim?: true)
    end

    attribute :owner_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("The ID of the user who owns this business")
    end

    attribute :is_public_searchable, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether this business appears in global search for unregistered users")
    end

    attribute :public_description, :string do
      allow_nil?(true)
      public?(true)
      description("Public-facing description shown in global search results")
      constraints(max_length: 500, trim?: true)
    end

    attribute :city, :string do
      allow_nil?(true)
      public?(true)
      description("City where the business is located")
      constraints(max_length: 100, trim?: true)
    end

    attribute :country, :string do
      allow_nil?(true)
      public?(true)
      description("Country where the business is located")
      constraints(max_length: 100, trim?: true)
    end

    attribute :address, :string do
      allow_nil?(true)
      public?(true)
      description("Full address of the business")
      constraints(max_length: 500, trim?: true)
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    has_many :plots, RivaAsh.Resources.Plot do
      destination_attribute(:business_id)
      public?(true)
      description("Plots owned or managed by this business")
    end

    # Note: owner relationship removed due to cross-domain constraints
    # Business ownership is managed at the application level via owner_id attribute
    # Use RivaAsh.Accounts.User.get(owner_id) to fetch owner when needed
  end

  identities do
    identity(:unique_name, [:name])
  end

  @doc """
  Returns a list of businesses formatted for dropdown selection.
  
  ## Returns
  A list of tuples `{id, name}` suitable for form dropdowns.
  """
  @spec choices_for_select :: [{String.t(), String.t()}]
  def choices_for_select do
    __MODULE__
    |> Ash.read!()
    |> Enum.map(&{&1.id, &1.name})
  end

  @doc """
  Fetches the owner user for a business.
  
  Since User is in a different domain, we handle this at the application level.
  
  ## Parameters
  - business - A business record with owner_id field
  
  ## Returns
  `{:ok, user | nil}` or `{:error, reason}`
  """
  @spec get_owner(%{owner_id: String.t() | nil}) :: {:ok, map() | nil} | {:error, any()}
  def get_owner(%{owner_id: owner_id}) when not is_nil(owner_id) do
    Ash.get(RivaAsh.Accounts.User, owner_id, domain: RivaAsh.Accounts)
  end

  def get_owner(_), do: {:ok, nil}

  @doc """
  Fetches the owner user for a business by business ID.
  
  ## Parameters
  - business_id - The UUID of the business
  
  ## Returns
  `{:ok, user | nil}` or `{:error, reason}`
  """
  @spec get_owner_by_business_id(String.t()) :: {:ok, map() | nil} | {:error, any()}
  def get_owner_by_business_id(business_id) do
    with {:ok, business} <- Ash.get(__MODULE__, business_id, domain: RivaAsh.Domain) do
      get_owner(business)
    end
  end

  @doc """
  Determines if a business is currently active (not archived).
  
  ## Parameters
  - business - A business record
  
  ## Returns
  `true` if the business is active, `false` otherwise
  """
  @spec is_active?(t()) :: boolean()
  def is_active?(%__MODULE__{archived_at: nil}), do: true
  def is_active?(%__MODULE__{}), do: false

  @doc """
  Formats the business address for display.
  
  ## Parameters
  - business - A business record
  
  ## Returns
  A formatted address string or "No address provided"
  """
  @spec formatted_address(t()) :: String.t()
  def formatted_address(%__MODULE__{address: address, city: city, country: country}) do
    [address, city, country]
    |> Enum.reject(&is_nil/1)
    |> Enum.join(", ")
    |> case do
      "" -> "No address provided"
      result -> result
    end
  end

  @doc """
  Generates a search-friendly name for the business.
  
  Combines name and location for better search results.
  
  ## Parameters
  - business - A business record
  
  ## Returns
  A formatted search string
  """
  @spec search_name(t()) :: String.t()
  def search_name(%__MODULE__{name: name, city: city}) do
    case city do
      nil -> name
      _ -> "#{name}, #{city}"
    end
  end

  # Private helper functions for search filtering
  @spec apply_search_filter(Ash.Query.t(), String.t() | nil) :: Ash.Query.t()
  defp apply_search_filter(query, nil), do: query

  defp apply_search_filter(query, "") do
    query
  end

  defp apply_search_filter(query, search_term) do
    Ash.Query.filter(
      query,
      expr(
        ilike(name, ^"%#{search_term}%") or
          ilike(public_description, ^"%#{search_term}%") or
          ilike(city, ^"%#{search_term}%") or
          ilike(address, ^"%#{search_term}%")
      )
    )
  end

  @spec apply_location_filter(Ash.Query.t(), String.t() | nil, String.t() | nil) :: Ash.Query.t()
  defp apply_location_filter(query, nil, _country), do: query

  defp apply_location_filter(query, "", _country), do: query

  defp apply_location_filter(query, city, nil) do
    Ash.Query.filter(query, expr(ilike(city, ^"%#{city}%")))
  end

  defp apply_location_filter(query, city, country) do
    query
    |> Ash.Query.filter(expr(ilike(city, ^"%#{city}%")))
    |> Ash.Query.filter(expr(ilike(country, ^"%#{country}%")))
  end

  @spec apply_active_filter(Ash.Query.t()) :: Ash.Query.t()
  defp apply_active_filter(query) do
    Ash.Query.filter(query, expr(is_nil(archived_at)))
  end
end
