defmodule RivaAshWeb.AuthPlug do
  @moduledoc """
  A simple authentication plug for development purposes.
  In a production environment, replace this with a proper authentication solution.
  """
  import Plug.Conn
  
  def init(opts), do: opts
  
  def call(conn, _opts) do
    # For development, we'll use a hardcoded admin user
    # In production, replace this with actual user authentication
    user = %{
      id: "admin",
      email: "admin@example.com",
      role: :admin
    }
    
    # Store the user in the session
    conn
    |> put_session(:user, user)
    |> assign(:current_user, user)
  end
end
