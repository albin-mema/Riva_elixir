defmodule RivaAshWeb.TokenLive do
  @moduledoc """
  LiveView for managing API Tokens.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Accounts.Token

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        # Only admins can view all tokens
        if user.role == :admin do
          tokens = Token |> Ash.read!(domain: RivaAsh.Accounts)

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, "Tokens")
            |> assign(:tokens, tokens)
            |> assign(:meta, %{}) # Placeholder for pagination/metadata

          {:ok, socket}
        else
          {:ok, redirect(socket, to: "/access-denied")}
        end
      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Tokens" description="Manage API tokens for authentication">
        <:action>
          <.button phx-click="new_token" class="bg-blue-600 hover:bg-blue-700">New Token</.button>
        </:action>
      </.page_header>

      <.data_table
        id="tokens-table"
        items={@tokens}
        meta={@meta}
        path="/tokens"
      >
        <:col :let={token} label="User ID" field={:user_id} sortable>
          <%= token.user_id %>
        </:col>
        <:col :let={token} label="Value">
          <%= token.value %>
        </:col>
        <:col :let={token} label="Expires At">
          <%= token.expires_at %>
        </:col>
        <:col :let={token} label="Actions">
          <.button phx-click="edit_token" phx-value-id={token.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_token" phx-value-id={token.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_token", _params, socket) do
    {:noreply, push_patch(socket, to: "/tokens/new")}
  end

  def handle_event("edit_token", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/tokens/#{id}/edit")}
  end

  def handle_event("delete_token", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting token with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
