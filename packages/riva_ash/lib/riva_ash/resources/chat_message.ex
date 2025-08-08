defmodule RivaAsh.Resources.ChatMessage do
  @moduledoc """
  Simple chat message resource for multi-room text communication.
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
    table("chat_messages")
    repo(RivaAsh.Repo)
  end

  attributes do
    uuid_primary_key(:id)

    attribute(:content, :string, allow_nil?: false, public?: true)
    attribute(:room_id, :uuid, allow_nil?: false, public?: true)

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to(:sender, RivaAsh.Accounts.User, allow_nil?: false, public?: true)
  end

  actions do
    defaults([:read])

    create :create do
      accept([:content, :room_id])
      change(relate_actor(:sender))
    end

    read :for_room do
      argument(:room_id, :uuid, allow_nil?: false)
      filter(expr(room_id == ^arg(:room_id)))
      prepare(build(sort: [inserted_at: :asc], load: [:sender]))
    end

    read :recent_rooms do
      prepare(
        build(
          sort: [inserted_at: :desc],
          load: [:sender]
        )
      )

      filter(
        expr(
          fragment(
            "? IN (SELECT DISTINCT room_id FROM chat_messages ORDER BY inserted_at DESC LIMIT 20)",
            ^arg(:room_id)
          )
        )
      )
    end
  end

  policies do
    # Allow authenticated users to read and create messages
    policy action_type([:read, :create]) do
      authorize_if(actor_present())
    end
  end

  validations do
    validate(present([:content, :room_id]))
    validate(string_length(:content, min: 1, max: 1000))
  end

  json_api do
    type("chat_message")

    routes do
      base("/chat_messages")
      get(:read)
      post(:create)
    end
  end

  graphql do
    type(:chat_message)

    queries do
      read_one(:chat_message, :read)
      list(:chat_messages, :read)
      list(:room_messages, :for_room)
      list(:recent_rooms, :recent_rooms)
    end

    mutations do
      create(:send_message, :create)
    end
  end
end
