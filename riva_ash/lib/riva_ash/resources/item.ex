defmodule RivaAsh.Resources.Item do
  @moduledoc """
  Represents an individual item that can optionally belong to a section.
  Items are the basic inventory units in the system.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource]

  postgres do
    table "items"
    repo RivaAsh.Repo
  end

  json_api do
    type "item"

    routes do
      base "/items"

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
    define :by_section, args: [:section_id], action: :by_section
    define :unassigned, action: :unassigned
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      accept [:name, :section_id]
      primary? true
    end

    read :by_id do
      argument :id, :uuid, allow_nil?: false
      get? true
      filter expr(id == ^arg(:id))
    end

    read :by_section do
      argument :section_id, :uuid, allow_nil?: false
      filter expr(section_id == ^arg(:section_id))
    end

    read :unassigned do
      filter expr(is_nil(section_id))
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
      description "The name of the item"
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :section, RivaAsh.Resources.Section do
      allow_nil? true
      attribute_writable? true
      public? true
      description "The section this item belongs to (optional)"
    end
  end

  identities do
    identity :unique_name, [:name]
  end
end
