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
      authorize_if actor_attribute_equals(:role, :admin)
      authorize_if expr(owner_id == ^actor(:id))
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
      accept([:name, :description, :is_public_searchable, :public_description, :city, :country, :address])
      primary?(true)

      validate(present([:name]), message: "Business name is required")

      validate(match(:name, ~r/^[a-zA-Z0-9\s\-_&.]+$/),
        message: "Business name contains invalid characters"
      )
    end

    create :create do
      accept([:name, :description, :owner_id, :is_public_searchable, :public_description, :city, :country, :address])
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
        query = case Ash.Query.get_argument(query, :search_term) do
          nil -> query
          "" -> query
          search_term ->
            Ash.Query.filter(query, expr(
              ilike(name, ^"%#{search_term}%") or
              ilike(public_description, ^"%#{search_term}%") or
              ilike(city, ^"%#{search_term}%") or
              ilike(address, ^"%#{search_term}%")
            ))
        end

        query = case Ash.Query.get_argument(query, :city) do
          nil -> query
          "" -> query
          city -> Ash.Query.filter(query, expr(ilike(city, ^"%#{city}%")))
        end

        case Ash.Query.get_argument(query, :country) do
          nil -> query
          "" -> query
          country -> Ash.Query.filter(query, expr(ilike(country, ^"%#{country}%")))
        end
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
      constraints instance_of: RivaAsh.Resources.Item
      argument :business_name, :string, allow_nil?: false
      argument :business_description, :string, allow_nil?: true
      argument :section_name, :string, allow_nil?: false
      argument :section_description, :string, allow_nil?: true
      argument :item_name, :string, allow_nil?: false
      argument :item_description, :string, allow_nil?: true
      argument :item_capacity, :integer, allow_nil?: false, default: 1
      run RivaAsh.Reactors.ExampleReactor
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

  # Helper function for admin dropdowns
  def choices_for_select do
    RivaAsh.Resources.Business
    |> Ash.read!()
    |> Enum.map(fn business ->
      {business.id, business.name}
    end)
  end

  @doc """
  Fetches the owner user for a business.
  Since User is in a different domain, we handle this at the application level.
  """
  def get_owner(%{owner_id: owner_id}) when not is_nil(owner_id) do
    Ash.get(RivaAsh.Accounts.User, owner_id, domain: RivaAsh.Accounts)
  end
  def get_owner(_), do: {:ok, nil}

  @doc """
  Fetches the owner user for a business by business ID.
  """
  def get_owner_by_business_id(business_id) do
    case Ash.get(__MODULE__, business_id, domain: RivaAsh.Domain) do
      {:ok, business} -> get_owner(business)
      error -> error
    end
  end
end
