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
    # TODO: Update to use the correct Ash function for setting actor
    # The Ash.set_actor!/2 function appears to be deprecated
    case actor(conn) do
      nil ->
        # Ash.set_actor!(conn, nil)
        conn

      _user ->
        # Ash.set_actor!(conn, user)
        conn
    end
  end
end
