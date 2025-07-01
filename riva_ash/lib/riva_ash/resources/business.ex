defmodule RivaAsh.Resources.Business do
  @moduledoc """
  Represents a business entity that can have multiple sections.
  Each business is a top-level organizational unit.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource]

  postgres do
    table "businesses"
    repo RivaAsh.Repo
  end

  json_api do
    type "business"

    routes do
      base "/businesses"

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
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      accept [:name, :description]
      primary? true
    end

    read :by_id do
      argument :id, :uuid, allow_nil?: false
      get? true
      filter expr(id == ^arg(:id))
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
      description "The name of the business"
    end

    attribute :description, :string do
      allow_nil? true
      public? true
      description "A detailed description of the business"
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    has_many :sections, RivaAsh.Resources.Section do
      destination_attribute :business_id
      public? true
      description "Sections belonging to this business"
    end
  end

  identities do
    identity :unique_name, [:name]
  end
end
