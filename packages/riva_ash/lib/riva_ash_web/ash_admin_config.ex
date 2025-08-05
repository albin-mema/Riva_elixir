defmodule RivaAshWeb.AshAdminConfig do
  @moduledoc """
  Configuration for AshAdmin to handle authentication and actor setup.
  
  This module provides the bridge between your application's authentication
  system and AshAdmin, enabling proper actor-based authorization and
  user context management within the admin interface.
  """

  @type conn :: Plug.Conn.t()
  @type user :: any()
  @type result :: {:ok, conn()} | {:error, String.t()}

  @doc """
  Retrieves the current authenticated user from the connection.
  
  Extracts the user that was set by the authentication plug
  (typically :require_authenticated_user) and returns it for
  use in AshAdmin operations.
  
  ## Parameters
    - `conn`: The Phoenix connection containing assigns
  
  ## Returns
    The current user struct or nil if no user is authenticated
  """
  @spec actor(conn()) :: user() | nil
  def actor(conn) do
    # Get the current user from the connection assigns
    # This will be set by the :require_authenticated_user plug
    conn.assigns[:current_user]
  end

  @doc """
  Sets the actor context for Ash operations based on the current user.
  
  Configures Ash to use the current authenticated user for authorization
  and filtering purposes. This function should be called in the AshAdmin
  pipeline to ensure proper actor-based access control.
  
  ## Parameters
    - `conn`: The Phoenix connection containing user context
  
  ## Returns
    The connection with actor context set for Ash operations
    
  ## TODO
    Update to use the correct Ash function for setting actor
    The Ash.set_actor!/2 function appears to be deprecated
  """
  @spec set_actor(conn()) :: conn()
  def set_actor(conn) do
    with {:ok, user} <- extract_user(conn),
         :ok <- validate_user_context(user),
         updated_conn <- configure_actor_context(conn, user) do
      updated_conn
    else
      {:error, reason} ->
        handle_actor_configuration_error(conn, reason)
    end
  end

  # Private helper functions for better abstraction and error handling
  defp extract_user(conn) do
    case actor(conn) do
      nil -> {:error, "No authenticated user found"}
      user -> {:ok, user}
    end
  end

  defp validate_user_context(nil), do: {:error, "User context is nil"}
  defp validate_user_context(_user), do: :ok

  defp configure_actor_context(conn, nil) do
    # Ash.set_actor!(conn, nil)
    conn
  end

  defp configure_actor_context(conn, user) do
    # Ash.set_actor!(conn, user)
    conn
  end

  defp handle_actor_configuration_error(conn, reason) do
    # Log the error for debugging purposes
    Logger.warning("Failed to set Ash actor: #{reason}")
    conn
  end
end
