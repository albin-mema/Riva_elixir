defmodule RivaAshWeb.TokenLive do
  @moduledoc """
  LiveView for managing API Tokens.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Spinner
  import RivaAshWeb.Components.Molecules.NotificationToast
  import RivaAshWeb.Components.Molecules.ConfirmDialog
  import RivaAshWeb.Components.Forms.TokenForm
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Accounts.Token
  alias RivaAsh.Accounts.User
  alias RivaAsh.Accounts.TokenService
  alias RivaAsh.ErrorHelpers

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        # Only admins can view all tokens
        if user.role == :admin do
          case TokenService.get_token_setup_data(user) do
            {:ok, %{tokens: tokens, users: users}} ->
              socket =
                socket
                |> assign(:current_user, user)
                |> assign(:page_title, get_page_title())
                |> assign(:tokens, tokens)
                |> assign(:meta, %{})
                |> assign(:form, nil)
                |> assign(:show_form, false)
                |> assign(:editing_token, nil)
                |> assign(:loading, false)
                |> assign(:error_message, nil)
                |> assign(:success_message, nil)
                |> assign(:confirm_delete_id, nil)
                |> assign(:users, users)

              {:ok, socket}

            {:error, error} ->
              error_message = ErrorHelpers.format_error(error)
              {:ok, redirect(socket, to: "/access-denied")}
          end
        else
          {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def handle_params(params, _uri, socket) do
    user = socket.assigns.current_user

    # Use Flop to handle pagination, sorting, and filtering
    case list_tokens_with_flop(user, params) do
      {tokens, meta} ->
        # Load users for the form
        users = load_users(user)

        socket
        |> assign(:tokens, tokens)
        |> assign(:meta, meta)
        |> assign(:users, users)
        |> then(&{:noreply, &1})

      _ ->
        socket =
          socket
          |> assign(:tokens, [])
          |> assign(:meta, %{})
          |> assign(:users, [])
          |> assign(:error_message, "Failed to load tokens")

        {:noreply, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <!-- Error Toast -->
      <.notification_toast
        :if={@error_message}
        type="error"
        message={@error_message}
        show={@error_message != nil}
        on_dismiss="clear_error"
        aria-live="assertive"
        aria-atomic="true"
      />

      <!-- Success Toast -->
      <.notification_toast
        :if={@success_message}
        type="success"
        message={@success_message}
        show={@success_message != nil}
        on_dismiss="clear_success"
        aria-live="polite"
        aria-atomic="true"
      />

      <!-- Loading Spinner -->
      <.spinner :if={@loading} size="lg" show_label={true} label="Loading..." class="fixed inset-0 flex items-center justify-center bg-black bg-opacity-50 z-50" aria-label="Loading tokens" role="status" />

      <!-- Confirmation Dialog -->
      <.confirm_dialog
        :if={@confirm_delete_id}
        title="Delete Token"
        message="Are you sure you want to delete this token? This action cannot be undone. Any applications using this token will lose access."
        confirm_label="Delete"
        cancel_label="Cancel"
        variant="destructive"
        on_confirm="confirm_delete"
        on_cancel="cancel_delete"
        show={@confirm_delete_id != nil}
        aria-modal="true"
        role="dialog"
      />

      <!-- Page Header -->
      <.page_header title="Tokens" description="Manage API tokens for authentication">
        <:action>
          <.button phx-click="new_token" variant="primary" aria-label="Create new token">New Token</.button>
        </:action>
      </.page_header>

      <!-- Security Warning -->
      <div class="bg-yellow-50 border-l-4 border-yellow-400 p-4 mb-6" role="alert" aria-live="polite">
        <div class="flex">
          <div class="flex-shrink-0">
            <svg class="h-5 w-5 text-yellow-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
              <path fill-rule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clip-rule="evenodd" />
            </svg>
          </div>
          <div class="ml-3">
            <h3 class="text-sm font-medium text-yellow-800">Security Notice</h3>
            <div class="mt-2 text-sm text-yellow-700">
              <p>API tokens provide full access to the system. Keep them secure and never share them in code or public repositories.</p>
            </div>
          </div>
        </div>
      </div>

      <!-- Add/Edit Token Form -->
      <%= if @show_form do %>
        <.token_form
          form={@form}
          editing={@editing_token != nil}
          loading={@loading}
          users={@users}
          on_submit="save_token"
          on_change="validate_token"
          on_cancel="cancel_form"
        />
      <% end %>

      <!-- Tokens Table -->
      <div class="bg-white shadow rounded-lg mt-6">
        <div class="sm:p-6 px-4 py-5">
          <%= if @tokens == [] do %>
            <div class="text-center py-12" role="status" aria-live="polite">
              <p class="text-gray-500">No tokens found.</p>
              <.button phx-click="new_token" variant="primary" class="mt-4" aria-label="Create your first token">Create Your First Token</.button>
            </div>
          <% else %>
            <table class="min-w-full divide-y divide-gray-200" aria-label="Tokens">
              <thead class="bg-gray-50">
                <tr>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">User ID</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Token Prefix</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Purpose</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Expires At</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Actions</th>
                </tr>
              </thead>
              <tbody class="bg-white divide-y divide-gray-200">
                <tr :for={token <- @tokens}>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="User ID"><%= token.user_id %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Token Prefix">
                    <%= mask_token_value(token.token) %>
                    <span class="ml-2 text-xs text-gray-500">(Full token only shown when created)</span>
                  </td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Purpose"><%= token.purpose || "N/A" %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Expires At"><%= token.expires_at %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm font-medium" data-label="Actions">
                    <.button phx-click="edit_token" phx-value-id={token.id} variant="primary" class="mr-2" aria-label={"Edit token #{token.id}"}>Edit</.button>
                    <.button phx-click="delete_token" phx-value-id={token.id} variant="destructive" aria-label={"Delete token #{token.id}"}>Delete</.button>
                  </td>
                </tr>
              </tbody>
            </table>

            <!-- Pagination -->
            <div class="mt-6">
              <%= if @meta.total_pages > 1 do %>
                <nav class="flex items-center justify-between border-t border-gray-200 px-4 sm:px-0" role="navigation" aria-label="Pagination Navigation">
                  <div class="-mt-px flex w-0 flex-1">
                    <%= if @meta.has_previous_page? do %>
                      <.button phx-click="go_to_page" phx-value-page={@meta.previous_page} variant="outline" class="mt-4 inline-flex items-center border-t-2 border-transparent pt-4 pr-1 text-sm font-medium text-gray-500 hover:border-gray-300 hover:text-gray-700" aria-label="Go to previous page">
                        Previous
                      </.button>
                    <% else %>
                      <.button variant="outline" class="mt-4 inline-flex items-center border-t-2 border-transparent pt-4 pr-1 text-sm font-medium text-gray-300 cursor-not-allowed" disabled aria-label="No previous page" tabindex="-1">
                        Previous
                      </.button>
                    <% end %>
                  </div>
                  <div class="hidden md:-mt-px md:flex" role="list">
                    <%= for page <- 1..@meta.total_pages do %>
                      <.button phx-click="go_to_page" phx-value-page={page} variant={if page == @meta.current_page, do: "primary", else: "outline"} class={"inline-flex items-center border-t-2 px-4 pt-4 text-sm font-medium #{
                        if page == @meta.current_page do
                          "border-blue-500 text-blue-600"
                        else
                          "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700"
                        end
                      }"} aria-label={"Go to page #{page}" <> if page == @meta.current_page, do: " (current page)", else: ""} aria-current={if page == @meta.current_page, do: "page", else: "false"}><%= page %></.button>
                    <% end %>
                  </div>
                  <div class="-mt-px flex w-0 flex-1 justify-end">
                    <%= if @meta.has_next_page? do %>
                      <.button phx-click="go_to_page" phx-value-page={@meta.next_page} variant="outline" class="mt-4 inline-flex items-center border-t-2 border-transparent pt-4 pl-1 text-sm font-medium text-gray-500 hover:border-gray-300 hover:text-gray-700" aria-label="Go to next page">
                        Next
                      </.button>
                    <% else %>
                      <.button variant="outline" class="mt-4 inline-flex items-center border-t-2 border-transparent pt-4 pl-1 text-sm font-medium text-gray-300 cursor-not-allowed" disabled aria-label="No next page" tabindex="-1">
                        Next
                      </.button>
                    <% end %>
                  </div>
                </nav>
              <% end %>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("new_token", _params, socket) do
    user = socket.assigns.current_user

    form =
      AshPhoenix.Form.for_create(Token, :create, actor: user)
      |> to_form()

    socket =
      socket
      |> assign(:show_form, true)
      |> assign(:editing_token, nil)
      |> assign(:form, form)

    {:noreply, socket}
  end

  def handle_event("edit_token", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    case TokenService.get_token_for_edit(id, user) do
      {:ok, token} ->
        form =
          AshPhoenix.Form.for_update(token, :update, actor: user)
          |> to_form()

        socket =
          socket
          |> assign(:editing_token, token)
          |> assign(:form, form)
          |> assign(:show_form, true)

        {:noreply, socket}

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)
        socket =
          socket
          |> assign(:error_message, "Token not found: #{error_message}")
          |> assign(:show_form, false)

        {:noreply, socket}
    end
  end

  def handle_event("delete_token", %{"id" => id}, socket) do
    # Show confirmation dialog
    {:noreply, assign(socket, :confirm_delete_id, id)}
  end

  def handle_event("confirm_delete", _params, socket) do
    token_id = socket.assigns.confirm_delete_id
    user = socket.assigns.current_user

    socket =
      socket
      |> assign(:loading, true)
      |> assign(:confirm_delete_id, nil)

    case TokenService.delete_token(token_id, user) do
      {:ok, _token} ->
        socket =
          socket
          |> assign(:loading, false)
          |> assign(:success_message, "Token deleted successfully!")

        {:noreply, push_patch(socket, to: ~p"/tokens")}

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:error_message, "Failed to delete token: #{error_message}")

        {:noreply, socket}
    end
  end

  def handle_event("cancel_delete", _params, socket) do
    {:noreply, assign(socket, :confirm_delete_id, nil)}
  end

  def handle_event("cancel_form", _params, socket) do
    socket =
      socket
      |> assign(:show_form, false)
      |> assign(:editing_token, nil)
      |> assign(:form, nil)

    {:noreply, socket}
  end

  def handle_event("validate_token", %{"form" => params}, socket) do
    form =
      if socket.assigns.editing_token do
        AshPhoenix.Form.for_update(socket.assigns.editing_token, :update,
          actor: socket.assigns.current_user
        )
      else
        AshPhoenix.Form.for_create(Token, :create, actor: socket.assigns.current_user)
      end
      |> AshPhoenix.Form.validate(params, errors: true)
      |> to_form()

    {:noreply, assign(socket, :form, form)}
  end

  def handle_event("save_token", %{"form" => params}, socket) do
    user = socket.assigns.current_user

    socket = assign(socket, :loading, true)

    # Use TokenService for business logic
    result = if socket.assigns.editing_token do
      TokenService.update_token(socket.assigns.editing_token.id, params, user)
    else
      TokenService.create_token(params, user)
    end

    case result do
      {:ok, token} ->
        action_text = if socket.assigns.editing_token, do: "updated", else: "created"

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:show_form, false)
          |> assign(:editing_token, nil)
          |> assign(:form, nil)
          |> assign(:success_message, "Token #{action_text} successfully!")

        {:noreply, push_patch(socket, to: ~p"/tokens")}

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:form, nil)
          |> assign(:error_message, "Failed to save token: #{error_message}")

        {:noreply, socket}
    end
  end

  def handle_event("clear_error", _params, socket) do
    {:noreply, assign(socket, :error_message, nil)}
  end

  def handle_event("clear_success", _params, socket) do
    {:noreply, assign(socket, :success_message, nil)}
  end

  def handle_event("go_to_page", %{"page" => page}, socket) do
    {:noreply, push_patch(socket, to: ~p"/tokens?page=#{page}")}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions
  defp list_tokens_with_flop(user, params) do
    try do
      # Get tokens as a list
      tokens = load_tokens_simple(user)

      # Validate Flop parameters first
      case Flop.validate(params, for: Token) do
        {:ok, flop} ->
          # Apply manual sorting and pagination to the list
          {paginated_tokens, meta} = apply_flop_to_list(tokens, flop)
          {paginated_tokens, meta}

        {:error, meta} ->
          # Return empty results with error meta
          {[], meta}
      end
    rescue
      error ->
        {[], %Flop.Meta{errors: [Exception.message(error)]}}
    end
  end

  defp apply_flop_to_list(tokens, flop) do
    # Apply sorting
    sorted_tokens = apply_sorting(tokens, flop)

    # Apply pagination
    total_count = length(sorted_tokens)
    {paginated_tokens, pagination_info} = apply_pagination(sorted_tokens, flop)

    # Build meta information
    meta = build_meta(flop, total_count, pagination_info)

    {paginated_tokens, meta}
  end

  defp apply_sorting(tokens, %Flop{order_by: nil}), do: tokens
  defp apply_sorting(tokens, %Flop{order_by: []}), do: tokens

  defp apply_sorting(tokens, %Flop{order_by: order_by, order_directions: order_directions}) do
    # Default to :asc if no directions provided
    directions = order_directions || Enum.map(order_by, fn _ -> :asc end)

    # Zip order_by and directions, pad directions with :asc if needed
    order_specs =
      Enum.zip_with([order_by, directions], fn [field, direction] -> {field, direction} end)

    Enum.sort(tokens, fn token1, token2 ->
      compare_tokens(token1, token2, order_specs)
    end)
  end

  defp compare_tokens(_token1, _token2, []), do: true

  defp compare_tokens(token1, token2, [{field, direction} | rest]) do
    val1 = get_token_field_value(token1, field)
    val2 = get_token_field_value(token2, field)

    case {val1, val2} do
      {same, same} -> compare_tokens(token1, token2, rest)
      {nil, _} -> direction == :desc
      {_, nil} -> direction == :asc
      {v1, v2} when direction == :asc -> v1 <= v2
      {v1, v2} when direction == :desc -> v1 >= v2
    end
  end

  defp get_token_field_value(token, field) do
    case field do
      :user_id -> token.user_id
      :token -> token.token
      :expires_at -> token.expires_at
      :inserted_at -> token.inserted_at
      :updated_at -> token.updated_at
      _ -> nil
    end
  end

  defp apply_pagination(tokens, flop) do
    total_count = length(tokens)

    cond do
      # Page-based pagination
      flop.page && flop.page_size ->
        page = max(flop.page, 1)
        page_size = flop.page_size
        offset = (page - 1) * page_size

        paginated = tokens |> Enum.drop(offset) |> Enum.take(page_size)

        pagination_info = %{
          type: :page,
          page: page,
          page_size: page_size,
          offset: offset,
          total_count: total_count
        }

        {paginated, pagination_info}

      # Offset-based pagination
      flop.offset && flop.limit ->
        offset = max(flop.offset, 0)
        limit = flop.limit

        paginated = tokens |> Enum.drop(offset) |> Enum.take(limit)

        pagination_info = %{
          type: :offset,
          offset: offset,
          limit: limit,
          total_count: total_count
        }

        {paginated, pagination_info}

      # Limit only
      flop.limit ->
        limit = flop.limit
        paginated = Enum.take(tokens, limit)

        pagination_info = %{
          type: :limit,
          limit: limit,
          total_count: total_count
        }

        {paginated, pagination_info}

      # No pagination
      true ->
        pagination_info = %{
          type: :none,
          total_count: total_count
        }

        {tokens, pagination_info}
    end
  end

  defp build_meta(flop, total_count, pagination_info) do
    case pagination_info.type do
      :page ->
        page = pagination_info.page
        page_size = pagination_info.page_size
        total_pages = ceil(total_count / page_size)

        %Flop.Meta{
          current_page: page,
          current_offset: pagination_info.offset,
          flop: flop,
          has_next_page?: page < total_pages,
          has_previous_page?: page > 1,
          next_page: if(page < total_pages, do: page + 1, else: nil),
          previous_page: if(page > 1, do: page - 1, else: nil),
          page_size: page_size,
          total_count: total_count,
          total_pages: total_pages,
          opts: []
        }

      :offset ->
        offset = pagination_info.offset
        limit = pagination_info.limit
        current_page = div(offset, limit) + 1
        total_pages = ceil(total_count / limit)

        %Flop.Meta{
          current_page: current_page,
          current_offset: offset,
          flop: flop,
          has_next_page?: offset + limit < total_count,
          has_previous_page?: offset > 0,
          next_offset: if(offset + limit < total_count, do: offset + limit, else: nil),
          previous_offset: if(offset > 0, do: max(0, offset - limit), else: nil),
          page_size: limit,
          total_count: total_count,
          total_pages: total_pages,
          opts: []
        }

      _ ->
        %Flop.Meta{
          current_page: 1,
          current_offset: 0,
          flop: flop,
          has_next_page?: false,
          has_previous_page?: false,
          page_size: total_count,
          total_count: total_count,
          total_pages: 1,
          opts: []
        }
    end
  end

  defp load_tokens_simple(user) do
    try do
      case Token.read(actor: user) do
        {:ok, tokens} -> tokens
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp load_users(user) do
    try do
      case User.read(actor: user) do
        {:ok, users} -> users
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, __MODULE__, [])[:page_title] || "Tokens"
  end

  defp mask_token_value(token) when is_binary(token) and byte_size(token) > 8 do
    # Show first 4 and last 4 characters, mask the middle
    prefix = binary_part(token, 0, 4)
    suffix = binary_part(token, byte_size(token) - 4, 4)
    "#{prefix}****#{suffix}"
  end

  defp mask_token_value(_token), do: "****"
end
