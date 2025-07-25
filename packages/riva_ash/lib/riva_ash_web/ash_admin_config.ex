defmodule RivaAshWeb.AshAdminConfig do
  @moduledoc """
  Configuration for AshAdmin to handle authentication and actor setup.
  """

  def actor(conn) do
    # Get the current user from the connection assigns
    # This will be set by the :require_authenticated_user plug
    conn.assigns[:current_user]
  end

  def set_actor(conn) do
    case actor(conn) do
      nil ->
        Ash.set_actor!(conn, nil)
      user ->
        Ash.set_actor!(conn, user)
    end
  end
end
