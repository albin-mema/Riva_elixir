defmodule RivaAsh.Resources.Section do
  @moduledoc """
  Represents a section within a plot that can contain multiple items.
  Sections help organize items within a business context and provide
  spatial organization for reservable resources.
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

  standard_postgres("sections")
  standard_archive()
  standard_admin([:name, :plot, :description])

  # Authorization policies
  policies do
    # Admin bypass
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Business owner has full access to their business data
    policy action_type([:read, :create, :update, :destroy]) do
      authorize_if(expr(plot.business.owner_id == ^actor(:id)))
    end

    # Employees with manager role can manage sections
    policy action_type([:create, :update]) do
      authorize_if(actor_attribute_equals(:role, :manager))
    end

    # Employees can read sections
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :employee))
    end
  end

  json_api do
    type("section")

    routes do
      base("/sections")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for section-specific actions
      get(:by_business, route: "/by-business/:business_id")
      get(:active, route: "/active")
      get(:inactive, route: "/inactive")
      get(:with_items, route: "/with-items")
      get(:empty, route: "/empty")
    end
  end

  graphql do
    type(:section)

    queries do
      get(:get_section, :read)
      list(:list_sections, :read)
      list(:sections_by_business, :by_business)
      list(:active_sections, :active)
      list(:inactive_sections, :inactive)
      list(:sections_with_items, :with_items)
      list(:empty_sections, :empty)
    end

    mutations do
      create(:create_section, :create)
      update(:update_section, :update)
      destroy(:delete_section, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_plot, args: [:plot_id], action: :by_plot)
    define(:by_business, args: [:business_id], action: :by_business)
    define(:active, action: :active)
    define(:inactive, action: :inactive)
    define(:with_items, action: :with_items)
    define(:empty, action: :empty)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :description, :plot_id])
      primary?(true)

      # Note: Section doesn't have direct business_id, so we validate plot exists
      # Business context is enforced through plot relationship
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_plot do
      argument(:plot_id, :uuid, allow_nil?: false)
      filter(expr(plot_id == ^arg(:plot_id)))
    end

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(plot.business_id == ^arg(:business_id)))
    end

    read :active do
      # Non-archived sections are considered active
      filter(expr(is_nil(archived_at)))
    end

    read :inactive do
      # Archived sections are considered inactive
      filter(expr(not is_nil(archived_at)))
    end

    read :with_items do
      # Load items relationship - this will be handled by GraphQL automatically
    end

    read :empty do
      # Sections with no items
      filter(expr(not exists(items)))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the section")
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("A detailed description of the section")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :plot, RivaAsh.Resources.Plot do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The plot this section belongs to")
    end

    has_many :items, RivaAsh.Resources.Item do
      destination_attribute(:section_id)
      public?(true)
      description("Items contained in this section")
    end


  end

  identities do
    identity(:unique_name_per_plot, [:name, :plot_id])
  end

  # Helper function for admin dropdowns
  def choices_for_select do
    RivaAsh.Resources.Section
    |> Ash.read!()
    |> Enum.map(fn section ->
      business_name = if section.business do
        section.business.name
      else
        "Unknown Business"
      end
      {section.id, "#{section.name} (#{business_name})"}
    end)
  end
end
