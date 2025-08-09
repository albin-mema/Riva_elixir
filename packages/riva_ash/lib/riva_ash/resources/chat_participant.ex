defmodule RivaAsh.Resources.ChatParticipant do
  @moduledoc """
  Participant in a chat room. A participant can be either an internal user or an external client.
  Exactly one of user_id or client_id must be present.
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
    table("chat_participants")
    repo(RivaAsh.Repo)
  end

  attributes do
    uuid_primary_key(:id)

    attribute :role, :string do
      default("member")
      allow_nil?(false)
      public?(true)
      description("Role of the participant in the room")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to(:room, RivaAsh.Resources.ChatRoom, allow_nil?: false, public?: true)
    belongs_to(:user, RivaAsh.Accounts.User, allow_nil?: true, public?: true)
    belongs_to(:client, RivaAsh.Resources.Client, allow_nil?: true, public?: true)
  end

  actions do
    defaults([:read])

    create :create do
      accept([:room_id, :user_id, :client_id, :role])

      validate(fn changeset, _ ->
        user_id = Ash.Changeset.get_attribute(changeset, :user_id)
        client_id = Ash.Changeset.get_attribute(changeset, :client_id)

        case {user_id, client_id} do
          {nil, nil} -> {:error, field: :user_id, message: "either user_id or client_id must be present"}
          {_, nil} -> :ok
          {nil, _} -> :ok
          {_, _} -> {:error, field: :client_id, message: "only one of user_id or client_id may be present"}
        end
      end)
    end
  end

  policies do
    # Tighten later: for now, require actor
    policy action_type([:read, :create]) do
      authorize_if(actor_present())
    end
  end

  json_api do
    type("chat_participant")

    routes do
      base("/chat_participants")
      get(:read)
      post(:create)
    end
  end

  graphql do
    type(:chat_participant)

    queries do
      list(:chat_participants, :read)
    end

    mutations do
      create(:add_participant, :create)
    end
  end
end
