alias RivaAsh.Resources, as: Resources
alias Ash.Policy, as: Policy
alias RivaAsh.Accounts, as: Accounts

defmodule RivaAsh.Resources.ChatRoom do
  @moduledoc """
  Chat room resource for organizing conversations.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource
    ]

  postgres do
    table("chat_rooms")
    repo(RivaAsh.Repo)
  end

  attributes do
    uuid_primary_key(:id)

    attribute(:name, :string, allow_nil?: false, public?: true)
    attribute(:description, :string, public?: true)
    # general, support, team, reservation
    attribute(:room_type, :string, default: "general", public?: true)
    attribute(:is_active, :boolean, default: true, public?: true)

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to(:business, RivaAsh.Resources.Business, allow_nil?: false, public?: true)
    belongs_to(:created_by, RivaAsh.Accounts.User, allow_nil?: false, public?: true)
    has_many(:messages, RivaAsh.Resources.ChatMessage, destination_attribute: :room_id, source_attribute: :id)
  end

  actions do
    defaults([:read])

    create :create do
      accept([:name, :description, :room_type])
      argument(:business_id, :uuid, allow_nil?: false)

      change(relate_actor(:created_by))
      change(set_attribute(:business_id, arg(:business_id)))
    end

    update :update do
      accept([:name, :description, :is_active])
    end

    read :for_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(business_id == ^arg(:business_id) and is_active == true))
      prepare(build(sort: [inserted_at: :desc]))
    end

    read :active_rooms do
      filter(expr(is_active == true))
      prepare(build(sort: [inserted_at: :desc]))
    end
  end

  policies do
    # Allow authenticated users to read and create rooms
    policy action_type([:read, :create, :update]) do
      authorize_if(actor_present())
    end
  end

  validations do
    validate(present([:name]))
    validate(string_length(:name, min: 1, max: 100))
    validate(one_of(:room_type, ["general", "support", "team", "reservation"]))
  end

  identities do
    identity(:unique_room_name_per_business, [:business_id, :name])
  end

  json_api do
    type("chat_room")

    routes do
      base("/chat_rooms")
      get(:read)
      post(:create)
      patch(:update)
    end
  end

  graphql do
    type(:chat_room)

    queries do
      read_one(:chat_room, :read)
      list(:chat_rooms, :read)
      list(:business_rooms, :for_business)
      list(:active_rooms, :active_rooms)
    end

    mutations do
      create(:create_room, :create)
      update(:update_room, :update)
    end
  end
end
