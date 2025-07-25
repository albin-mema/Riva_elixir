defmodule RivaAshWeb.LayoutLive do
  @moduledoc """
  LiveView for managing Layouts.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Layout
  alias RivaAsh.Resources.Business

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        try do
          # Get user's businesses first
          businesses = Business.read!(actor: user)
          business_ids = Enum.map(businesses, & &1.id)

          # Get layouts for user's businesses
          layouts = Layout.read!(actor: user, filter: [business_id: [in: business_ids]])

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, "Layouts")
            |> assign(:layouts, layouts)
            |> assign(:meta, %{}) # Placeholder for pagination/metadata

          {:ok, socket}
        rescue
          error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, :not_authenticated} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Layouts" description="Manage various layout configurations">
        <:action>
          <.button phx-click="new_layout" class="bg-blue-600 hover:bg-blue-700">New Layout</.button>
        </:action>
      </.page_header>

      <.data_table
        id="layouts-table"
        items={@layouts}
        meta={@meta}
        path="/layouts"
      >
        <:col :let={layout} label="Name" field={:name} sortable>
          <%= layout.name %>
        </:col>
        <:col :let={layout} label="Description">
          <%= layout.description %>
        </:col>
        <:col :let={layout} label="Width">
          <%= layout.width %>
        </:col>
        <:col :let={layout} label="Height">
          <%= layout.height %>
        </:col>
        <:col :let={layout} label="Actions">
          <.button phx-click="edit_layout" phx-value-id={layout.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_layout" phx-value-id={layout.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_layout", _params, socket) do
    {:noreply, push_patch(socket, to: "/layouts/new")}
  end

  def handle_event("edit_layout", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/layouts/#{id}/edit")}
  end

  def handle_event("delete_layout", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting layout with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions
  defp get_current_user_from_session(session) do
    user_token = session["user_token"]

    if user_token do
      with {:ok, user_id} <- Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", user_token, max_age: 86_400) |> RivaAsh.ErrorHelpers.to_result(),
           {:ok, user} <- Ash.get(RivaAsh.Accounts.User, user_id, domain: RivaAsh.Accounts) |> RivaAsh.ErrorHelpers.to_result() do
        RivaAsh.ErrorHelpers.success(user)
      else
        _ -> RivaAsh.ErrorHelpers.failure(:not_authenticated)
      end
    else
      RivaAsh.ErrorHelpers.failure(:not_authenticated)
    end
  end
end
