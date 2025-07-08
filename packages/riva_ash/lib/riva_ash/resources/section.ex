defmodule RivaAsh.Resources.Section do
  @moduledoc """
  Represents a section within a business that can contain multiple items.
  Sections help organize items within a business context.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource, AshGraphql.Resource, AshArchival.Resource, AshAdmin.Resource]

  postgres do
    table("sections")
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
    table_columns([:name, :business, :description])

    # Configure relationship display
    relationship_display_fields([:name])
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
    define(:by_business, args: [:business_id], action: :by_business)
    define(:active, action: :active)
    define(:inactive, action: :inactive)
    define(:with_items, action: :with_items)
    define(:empty, action: :empty)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :description, :business_id])
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
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The business this section belongs to")
    end

    has_many :items, RivaAsh.Resources.Item do
      destination_attribute(:section_id)
      public?(true)
      description("Items contained in this section")
    end
  end

  identities do
    identity(:unique_name_per_business, [:name, :business_id])
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
