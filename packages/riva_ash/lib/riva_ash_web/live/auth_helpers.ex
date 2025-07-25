defmodule RivaAshWeb.Live.AuthHelpers do
  @moduledoc """
  Authentication helpers for LiveViews.
  """

  alias RivaAsh.ErrorHelpers

  @doc """
  Gets the current user from the LiveView session.
  Returns {:ok, user} if authenticated, {:error, reason} otherwise.
  """
  def get_current_user_from_session(session) do
    user_token = session["user_token"]

    if user_token do
      with {:ok, user_id} <- Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", user_token, max_age: 86_400) |> ErrorHelpers.to_result(),
           {:ok, user} <- Ash.get(RivaAsh.Accounts.User, user_id, domain: RivaAsh.Accounts) |> ErrorHelpers.to_result() do
        ErrorHelpers.success(user)
      else
        _ -> ErrorHelpers.failure(:not_authenticated)
      end
    else
      ErrorHelpers.failure(:not_authenticated)
    end
  end

  @doc """
  Handles Ash authorization errors and redirects appropriately.
  Returns {:ok, redirect_socket} for authorization failures.
  """
  def handle_ash_error(socket, error) do
    case error do
      %Ash.Error.Forbidden{} ->
        {:ok, Phoenix.LiveView.redirect(socket, to: "/access-denied")}

      %Ash.Error.Invalid{} ->
        {:ok, Phoenix.LiveView.redirect(socket, to: "/access-denied")}

      _ ->
        # For other errors, redirect to 404
        {:ok, Phoenix.LiveView.redirect(socket, to: "/404")}
    end
  end

  @doc """
  Handles authentication in LiveView mount/3 callback.
  Returns {:ok, socket} with user assigned if authenticated,
  {:ok, redirect_socket} if not authenticated.

  Note: This function should be called from within a LiveView context
  where assign/3 and redirect/2 are available.
  """
  def require_authentication(socket, session, redirect_to \\ "/sign-in") do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        # Use the socket's assign function (available in LiveView context)
        socket_with_user = socket |> Phoenix.Component.assign(:current_user, user)
        ErrorHelpers.success(socket_with_user)
      {:error, _} ->
        # Use Phoenix.LiveView.push_redirect instead
        redirect_socket = socket |> Phoenix.LiveView.push_navigate(to: redirect_to)
        ErrorHelpers.success(redirect_socket)
    end
  end

  @doc """
  Macro to be used in LiveViews that require authentication.
  Usage:

  ```elixir
  defmodule MyLive do
    use RivaAshWeb, :live_view
    import RivaAshWeb.Live.AuthHelpers

    def mount(_params, session, socket) do
      with_authentication socket, session do
        # Your authenticated mount logic here
        # This will automatically handle Ash authorization errors
        {:ok, assign(socket, :data, load_data(socket.assigns.current_user))}
      end
    end
  end
  ```
  """
  defmacro with_authentication(socket, session, do: block) do
    quote do
      case RivaAshWeb.Live.AuthHelpers.require_authentication(unquote(socket), unquote(session)) do
        {:ok, socket_with_user} ->
          # Rebind socket to include current_user
          socket = socket_with_user

          # Execute the block and handle any Ash authorization errors
          try do
            unquote(block)
          rescue
            error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
              RivaAshWeb.Live.AuthHelpers.handle_ash_error(socket, error)
          end

        {:ok, redirect_socket} ->
          {:ok, redirect_socket}
      end
    end
  end
end
