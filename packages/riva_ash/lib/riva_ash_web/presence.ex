alias RivaAshWeb.Presence, as: Presence
alias Phoenix.PubSub, as: PubSub

defmodule RivaAshWeb.Presence do
  @moduledoc """
  Provides presence tracking functionality for the Riva Ash application.

  This module uses Phoenix.Presence to track user presence across the application,
  enabling real-time features like showing who's online, active users in specific
  areas, and collaborative features.

  ## Usage

      # Track a user's presence
      RivaAshWeb.Presence.track(socket, user_id, %{
        online_at: inspect(System.system_time(:second)),
        user: user_data
      })

      # Get list of present users
      RivaAshWeb.Presence.list(socket)

      # Subscribe to presence updates
      RivaAshWeb.Presence.subscribe("users:lobby")

  ## Integration with LiveView

      defmodule MyAppWeb.SomeLive do
        use Phoenix.LiveView
        alias RivaAshWeb.Presence

        def mount(_params, _session, socket) do
          topic = "users:lobby"
          
          if connected?(socket) do
            Phoenix.PubSub.subscribe(RivaAsh.PubSub, topic)
            Presence.track(self(), topic, socket.assigns.current_user.id, %{
              online_at: inspect(System.system_time(:second))
            })
          end

          {:ok, assign(socket, users: Presence.list(topic))}
        end

        def handle_info(%{event: "presence_diff"}, socket) do
          {:noreply, assign(socket, users: Presence.list("users:lobby"))}
        end
      end
  """

  use Phoenix.Presence,
    otp_app: :riva_ash,
    pubsub_server: RivaAsh.PubSub

  @doc """
  Tracks a user's presence in a given topic.

  ## Parameters
    - `pid`: The process to track (usually `self()` in LiveView)
    - `topic`: The topic/channel to track presence in
    - `key`: Unique identifier for the presence (usually user ID)
    - `meta`: Additional metadata about the presence

  ## Examples

      RivaAshWeb.Presence.track(self(), "room:123", user.id, %{
        username: user.name,
        joined_at: System.system_time(:second)
      })
  """
  def track(pid, topic, key, meta \\ %{}) do
    __MODULE__.track(pid, topic, key, meta)
  end

  @doc """
  Updates the metadata for an existing presence.

  ## Examples

      RivaAshWeb.Presence.update(self(), "room:123", user.id, %{
        status: "away"
      })
  """
  def update(pid, topic, key, meta) do
    __MODULE__.update(pid, topic, key, meta)
  end

  @doc """
  Gets the list of presences for a topic.

  Returns a map where keys are the presence keys and values contain
  the metadata and process information.

  ## Examples

      presences = RivaAshWeb.Presence.list("room:123")
      # => %{"user_1" => %{metas: [%{username: "John", ...}]}, ...}
  """
  def list(topic) do
    __MODULE__.list(topic)
  end

  @doc """
  Gets a simplified list of present users for a topic.

  This is a convenience function that extracts just the user information
  from the presence data, making it easier to work with in templates.

  ## Examples

      users = RivaAshWeb.Presence.list_users("room:123")
      # => [%{id: "user_1", username: "John", ...}, ...]
  """
  def list_users(topic) do
    topic
    |> list()
    |> Enum.map(fn {user_id, %{metas: [meta | _unmatched]}} ->
      Map.put(meta, :id, user_id)
    end)
  end

  @doc """
  Subscribes the current process to presence updates for a topic.

  ## Examples

      RivaAshWeb.Presence.subscribe("room:123")
  """
  def subscribe(topic) do
    Phoenix.PubSub.subscribe(RivaAsh.PubSub, topic)
  end

  @doc """
  Unsubscribes the current process from presence updates for a topic.

  ## Examples

      RivaAshWeb.Presence.unsubscribe("room:123")
  """
  def unsubscribe(topic) do
    Phoenix.PubSub.unsubscribe(RivaAsh.PubSub, topic)
  end

  @doc """
  Counts the number of unique presences in a topic.

  ## Examples

      count = RivaAshWeb.Presence.count("room:123")
      # => 5
  """
  def count(topic) do
    topic
    |> list()
    |> map_size()
  end

  @doc """
  Checks if a specific user is present in a topic.

  ## Examples

      RivaAshWeb.Presence.present?("room:123", "user_1")
      # => true
  """
  def present?(topic, user_id) do
    topic
    |> list()
    |> Map.has_key?(user_id)
  end
end
