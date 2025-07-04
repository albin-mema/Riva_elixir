defmodule RivaAsh.Resources.Business do
  @moduledoc """
  Represents a business entity that can have multiple sections.
  Each business is a top-level organizational unit.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [AshJsonApi.Resource, AshArchival.Resource]

  postgres do
    table("businesses")
    repo(RivaAsh.Repo)
  end

  # Configure soft delete functionality
  archive do
    # Use archived_at field for soft deletes
    attribute(:archived_at)
    # Allow both soft and hard deletes
    base_filter?(false)
  end

  policies do
    # Only admins can create, update, or delete businesses
    policy action_type([:create, :update, :destroy]) do
      authorize_if(actor_attribute_equals(:role, :admin))
    end

    # All authenticated employees can read businesses
    policy action_type(:read) do
      authorize_if(actor_present())
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
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :description])
      primary?(true)
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
      load([:sections])
    end

    read :with_employees do
      # Load employees through sections (if needed) or directly if relationship exists
      # For now, this is a placeholder that can be enhanced
      load([])
    end

    # TODO: Re-enable reactor action once reactor syntax is fixed
    # action :create_complete_setup, :struct do
    #   constraints instance_of: RivaAsh.Resources.Item
    #   argument :business_name, :string, allow_nil?: false
    #   argument :business_description, :string, allow_nil?: true
    #   argument :section_name, :string, allow_nil?: false
    #   argument :section_description, :string, allow_nil?: true
    #   argument :item_name, :string, allow_nil?: false
    #   argument :item_description, :string, allow_nil?: true
    #   argument :item_capacity, :integer, allow_nil?: false, default: 1
    #   run RivaAsh.Reactors.ExampleReactor
    # end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the business")
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("A detailed description of the business")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    has_many :sections, RivaAsh.Resources.Section do
      destination_attribute(:business_id)
      public?(true)
      description("Sections belonging to this business")
    end
  end

  identities do
    identity(:unique_name, [:name])
  end
end
