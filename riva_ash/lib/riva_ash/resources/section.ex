defmodule RivaAsh.Resources.Section do
  @moduledoc """
  Represents a section within a business that can contain multiple items.
  Sections help organize items within a business context.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource]

  postgres do
    table "sections"
    repo RivaAsh.Repo
  end

  json_api do
    type "section"

    routes do
      base "/sections"

      get :read
      index :read
      post :create
      patch :update
      delete :destroy
    end
  end

  code_interface do
    define :create, action: :create
    define :read, action: :read
    define :update, action: :update
    define :destroy, action: :destroy
    define :by_id, args: [:id], action: :by_id
    define :by_business, args: [:business_id], action: :by_business
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      accept [:name, :description, :business_id]
      primary? true
    end

    read :by_id do
      argument :id, :uuid, allow_nil?: false
      get? true
      filter expr(id == ^arg(:id))
    end

    read :by_business do
      argument :business_id, :uuid, allow_nil?: false
      filter expr(business_id == ^arg(:business_id))
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
      description "The name of the section"
    end

    attribute :description, :string do
      allow_nil? true
      public? true
      description "A detailed description of the section"
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil? false
      attribute_writable? true
      public? true
      description "The business this section belongs to"
    end

    has_many :items, RivaAsh.Resources.Item do
      destination_attribute :section_id
      public? true
      description "Items contained in this section"
    end
  end

  identities do
    identity :unique_name_per_business, [:name, :business_id]
  end
end
