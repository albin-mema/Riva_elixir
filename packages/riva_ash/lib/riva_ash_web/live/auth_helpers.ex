defmodule RivaAshWeb.Live.AuthHelpers do
  @moduledoc """
  Authentication helpers for LiveViews.

  This module provides authentication and authorization helpers that follow
  the Phoenix/Ash/Elixir patterns from the styleguide:
  - Separates authentication logic from business logic
  - Uses proper Ash error handling
  - Provides reusable patterns for LiveViews
  """

  import Phoenix.Component, only: [assign: 3]
  import Phoenix.LiveView, only: [redirect: 2, put_flash: 3]
  alias RivaAsh.ErrorHelpers
  alias RivaAsh.Resources.Business
  alias RivaAsh.Accounts.User
  alias RivaAsh.Accounts
  alias Ash.Error.Forbidden
  alias Ash.Error.Invalid

  # Use application configuration for token max age
  @token_max_age Application.compile_env(:riva_ash, :auth_token_max_age, 86_400)

  @doc """
  Gets the current user from the LiveView session.
  Returns {:ok, user} if authenticated, {:error, reason} otherwise.

  ## Error Handling

  Handles various error scenarios with proper pattern matching:
  - Invalid tokens
  - Expired tokens
  - User not found
  - Ash authorization errors
  """
  def get_current_user_from_session(session) do
    user_token = session["user_token"]

    if user_token do
      with {:ok, user_id} <-
             Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", user_token, max_age: @token_max_age)
             |> ErrorHelpers.to_result(),
           {:ok, user} <-
             Ash.get(User, user_id, action: :seed_read, domain: Accounts)
             |> ErrorHelpers.to_result() do
        ErrorHelpers.success(user)
      else
        {:error, :invalid} -> ErrorHelpers.failure(:invalid_token)
        {:error, :expired} -> ErrorHelpers.failure(:token_expired)
        {:error, :not_found} -> ErrorHelpers.failure(:user_not_found)
        _ -> ErrorHelpers.failure(:not_authenticated)
      end
    else
      ErrorHelpers.failure(:not_authenticated)
    end
  end

  @doc """
  Handles Ash authorization errors and redirects appropriately.
  Returns {:ok, redirect_socket} for authorization failures.

  ## Error Pattern Matching

  Handles specific Ash error types with appropriate user-friendly responses:
  - Forbidden errors redirect to access denied
  - Invalid errors redirect to access denied
  - Other errors redirect to 404
  """
  def handle_ash_error(socket, error) do
    case error do
      %Forbidden{errors: [%{message: message} | _]} ->
        {:ok,
         socket
         |> put_flash(:error, message)
         |> redirect(to: "/access-denied")}

      %Invalid{errors: [%{message: message} | _]} ->
        {:ok,
         socket
         |> put_flash(:error, message)
         |> redirect(to: "/access-denied")}

      %Forbidden{} ->
        {:ok,
         socket
         |> put_flash(:error, "You don't have permission to access this resource")
         |> redirect(to: "/access-denied")}

      %Invalid{} ->
        {:ok,
         socket
         |> put_flash(:error, "Invalid request parameters")
         |> redirect(to: "/access-denied")}

      _ ->
        # For other errors, redirect to 404
        {:ok,
         socket
         |> put_flash(:error, "Resource not found")
         |> redirect(to: "/404")}
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

      {:error, :not_authenticated} ->
        # Use Phoenix.LiveView.push_redirect instead
        redirect_socket = socket |> Phoenix.LiveView.push_navigate(to: redirect_to)
        ErrorHelpers.success(redirect_socket)

      {:error, reason} ->
        # Handle other authentication errors
        redirect_socket =
          socket
          |> put_flash(:error, auth_error_message(reason))
          |> Phoenix.LiveView.push_navigate(to: redirect_to)

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
            error in [Forbidden, Invalid] ->
              RivaAshWeb.Live.AuthHelpers.handle_ash_error(socket, error)
          end

        {:ok, redirect_socket} ->
          {:ok, redirect_socket}
      end
    end
  end

  @doc """
  Loads business-scoped resources for a user.
  This is the standard pattern for resources that belong to businesses.

  ## Examples

      # For resources with direct business_id
      load_business_scoped_resources(user, ItemType, :business_id)

      # For resources with nested business relationship
      load_business_scoped_resources(user, Item, [:section, :plot, :business_id])

  ## Error Handling

  Properly handles Ash errors and provides meaningful error messages.
  """
  def load_business_scoped_resources(user, resource_module, business_path) do
    # Get user's businesses first
    businesses = Business.read!(actor: user)
    business_ids = Enum.map(businesses, & &1.id)

    # Build the filter based on the business path
    filter = build_business_filter(business_path, business_ids)

    # Load the resources with the filter
    resource_module.read!(actor: user, filter: filter)
    rescue
      error in [Forbidden, Invalid] ->
        # Re-raise the error to be handled by the calling LiveView
        reraise error, __STACKTRACE__
  end

  @doc """
  Standard mount pattern for business-scoped LiveViews.
  Handles authentication, business filtering, and error handling.

  ## Example

      def mount(_params, session, socket) do
        mount_business_scoped(socket, session, ItemType, :business_id, "Item Types")
      end
  """
  def mount_business_scoped(socket, session, resource_module, business_path, page_title) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        resources = load_business_scoped_resources(user, resource_module, business_path)

        socket =
          socket
          |> Phoenix.Component.assign(:current_user, user)
          |> Phoenix.Component.assign(:page_title, page_title)
          |> Phoenix.Component.assign(resource_key(resource_module), resources)
          # Placeholder for pagination/metadata
          |> Phoenix.Component.assign(:meta, %{})

        {:ok, socket}
      rescue
        error in [Forbidden, Invalid] ->
          {:ok, handle_ash_error(socket, error)}

      {:error, _} ->
        {:ok, Phoenix.LiveView.redirect(socket, to: "/sign-in")}
    end
  end

  # Private helper to build nested filters
  defp build_business_filter(business_path, business_ids) when is_list(business_path) do
    # For nested paths like [:section, :plot, :business_id]
    # Build nested filter structure
    Enum.reduce(Enum.reverse(business_path), [in: business_ids], fn key, acc ->
      [{key, acc}]
    end)
  end

  defp build_business_filter(business_field, business_ids) when is_atom(business_field) do
    # For direct business_id fields
    [{business_field, [in: business_ids]}]
  end

  # Helper to generate the assign key from the resource module name
  defp resource_key(resource_module) do
    resource_module
    |> Module.split()
    |> List.last()
    |> Macro.underscore()
    # Pluralize
    |> then(fn name -> String.to_existing_atom("#{name}s") end)
  end

  @doc """
  Standardized mount helper for simple authenticated pages.
  """
  def mount_authenticated_simple(socket, session, page_title \\ "Page") do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, page_title)

        {:ok, socket}

      {:error, :not_authenticated} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @doc """
  Standardized error handling for LiveViews.
  """
  def handle_liveview_error(socket, error) do
    case error do
      %Forbidden{} ->
        {:ok, redirect(socket, to: "/access-denied")}

      %Invalid{} ->
        {:ok, redirect(socket, to: "/access-denied")}

      _ ->
        {:ok,
         socket
         |> put_flash(:error, "An unexpected error occurred")
         |> redirect(to: "/dashboard")}
    end
  end

  # Private helper functions

  @doc """
  Converts authentication error codes to user-friendly messages.
  """
  defp auth_error_message(:invalid_token), do: "Invalid authentication token"
  defp auth_error_message(:token_expired), do: "Authentication token has expired"
  defp auth_error_message(:user_not_found), do: "User not found"
  defp auth_error_message(:not_authenticated), do: "Please sign in to continue"
  defp auth_error_message(_), do: "Authentication failed"
end
