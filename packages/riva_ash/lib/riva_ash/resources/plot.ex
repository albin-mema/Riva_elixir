defmodule RivaAsh.Resources.Plot do
  @moduledoc """
  Represents a physical land area owned or managed by a business.
  Plots are the top-level spatial organization unit that can contain multiple sections.

  A plot represents a real-world physical space that a business manages,
  such as a fairground, market area, parking lot, or event venue.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshArchival.Resource,
      AshAdmin.Resource
    ]

  postgres do
    table("plots")
    repo(RivaAsh.Repo)
  end

  # Configure soft delete functionality
  archive do
    # Use archived_at field for soft deletes
    attribute(:archived_at)
    # Allow both soft and hard deletes
    base_filter?(false)
  end

  # Configure admin interface
  admin do
    # Configure table display
    table_columns([:name, :business, :description, :total_area, :is_active])

    # Configure relationship display
    relationship_display_fields([:name])
  end

  json_api do
    type("plot")

    routes do
      base("/plots")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for plot specific actions
      get(:by_business, route: "/by-business/:business_id")
      get(:active, route: "/active")
      get(:with_sections, route: "/with-sections")
    end
  end

  graphql do
    type(:plot)

    queries do
      get(:get_plot, :read)
      list(:list_plots, :read)
      list(:plots_by_business, :by_business)
      list(:active_plots, :active)
    end

    mutations do
      create(:create_plot, :create)
      update(:update_plot, :update)
      destroy(:delete_plot, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_business, args: [:business_id], action: :by_business)
  end

  policies do
    # Default policy - deny all
    default_access_type(:strict)

    # Allow reading plots for authenticated users
    policy action_type(:read) do
      authorize_if(always())
    end

    # Allow business owners to manage their plots
    policy action_type([:create, :update, :destroy]) do
      authorize_if(relates_to_actor_via(:business))
    end

    # Allow employees to read plots for their business
    policy action_type(:read) do
      authorize_if(relates_to_actor_via([:business, :employees]))
    end
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :description, :business_id, :address, :total_area, :area_unit, :coordinates, :is_active])
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

    read :with_sections do
      # Load sections relationship - handled by GraphQL automatically
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the plot")
      constraints(min_length: 2, max_length: 100, trim?: true)
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("A detailed description of the plot")
      constraints(max_length: 1000, trim?: true)
    end

    attribute :address, :string do
      allow_nil?(true)
      public?(true)
      description("Physical address of the plot")
      constraints(max_length: 500, trim?: true)
    end

    attribute :total_area, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Total area of the plot")
    end

    attribute :area_unit, :string do
      allow_nil?(true)
      public?(true)
      default("sqft")
      constraints(max_length: 10)
      description("Unit of measurement for the area (sqft, sqm, acres, etc.)")
    end

    attribute :coordinates, :map do
      allow_nil?(true)
      public?(true)
      description("GPS coordinates or boundary coordinates of the plot")
    end

    attribute :is_active, :boolean do
      allow_nil?(false)
      default(true)
      public?(true)
      description("Whether the plot is currently active and available for use")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The business that owns or manages this plot")
    end

    has_many :sections, RivaAsh.Resources.Section do
      destination_attribute(:plot_id)
      public?(true)
      description("Sections within this plot")
    end

    has_many :layouts, RivaAsh.Resources.Layout do
      destination_attribute(:plot_id)
      public?(true)
      description("Visual layouts for this plot")
    end
  end

  identities do
    identity(:unique_name_per_business, [:name, :business_id])
  end

  # Helper function for admin dropdowns
  def choices_for_select do
    RivaAsh.Resources.Plot
    |> Ash.read!()
    |> Enum.map(fn plot ->
      {plot.id, plot.name}
    end)
  end
end
