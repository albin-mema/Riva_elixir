defmodule RivaAshWeb.BusinessLive do
  @moduledoc """
  LiveView for managing Businesses.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Business

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        businesses = Business.read!(actor: user)

        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, "Businesses")
          |> assign(:businesses, businesses)
          |> assign(:meta, %{}) # Placeholder for pagination/metadata

        {:ok, socket}
      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Businesses" description="Manage business information">
        <:action>
          <.button phx-click="new_business" class="bg-blue-600 hover:bg-blue-700">New Business</.button>
        </:action>
      </.page_header>

      <.data_table
        id="businesses-table"
        items={@businesses}
        meta={@meta}
        path="/businesses"
      >
        <:col :let={business} label="Name" field={:name} sortable>
          <%= business.name %>
        </:col>
        <:col :let={business} label="Description">
          <%= business.description || "No description" %>
        </:col>
        <:col :let={business} label="Created">
          <%= Calendar.strftime(business.inserted_at, "%Y-%m-%d") %>
        </:col>
        <:col :let={business} label="Actions">
          <.button phx-click="edit_business" phx-value-id={business.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_business" phx-value-id={business.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_business", _params, socket) do
    {:noreply, push_patch(socket, to: "/businesses/new")}
  end

  def handle_event("edit_business", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/businesses/#{id}/edit")}
  end

  def handle_event("delete_business", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting business with ID: #{id}")
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
