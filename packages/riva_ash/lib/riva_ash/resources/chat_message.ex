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

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to(:room, RivaAsh.Resources.ChatRoom, allow_nil?: false, public?: true)
    belongs_to(:sender_user, RivaAsh.Accounts.User, allow_nil?: true, public?: true)
    belongs_to(:sender_client, RivaAsh.Resources.Client, allow_nil?: true, public?: true)
  end

  actions do
    defaults([:read])

    create :create do
      accept([:content, :room_id])
      # Relate actor to correct sender relationship based on actor type
      change(fn changeset, context ->
        case context[:actor] do
          %RivaAsh.Accounts.User{id: _} ->
            Ash.Changeset.manage_relationship(changeset, :sender_user, context[:actor], type: :append_and_remove)

          %RivaAsh.Resources.Client{id: _} ->
            Ash.Changeset.manage_relationship(changeset, :sender_client, context[:actor], type: :append_and_remove)

          _ ->
            Ash.Changeset.add_error(changeset, "Invalid actor for sending message")
        end
      end)
    end

    read :for_room do
      argument(:room_id, :uuid, allow_nil?: false)
      filter(expr(room_id == ^arg(:room_id)))
      prepare(build(sort: [inserted_at: :asc], load: [:sender_user, :sender_client]))
    end
  end

  policies do
    # Allow authenticated users to read and create messages
    policy action_type([:read, :create]) do
      authorize_if(actor_present())
    end
  end

  validations do
    validate(present([:content]))
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
    end

    mutations do
      create(:send_message, :create)
    end
  end
end
