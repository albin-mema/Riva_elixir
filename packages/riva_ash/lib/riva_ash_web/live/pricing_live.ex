defmodule RivaAshWeb.PricingLive do
  @moduledoc """
  LiveView for managing Pricing plans.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Pricing

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        pricings = Pricing.read!(actor: user)

        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, "Pricing")
          |> assign(:pricings, pricings)
          |> assign(:meta, %{}) # Placeholder for pagination/metadata

        {:ok, socket}

      {:error, :not_authenticated} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Pricing" description="Define and manage pricing rules and plans">
        <:action>
          <.button phx-click="new_pricing" class="bg-blue-600 hover:bg-blue-700">New Pricing</.button>
        </:action>
      </.page_header>

      <.data_table
        id="pricings-table"
        items={@pricings}
        meta={@meta}
        path="/pricings"
      >
        <:col :let={pricing} label="Name" field={:name} sortable>
          <%= pricing.name %>
        </:col>
        <:col :let={pricing} label="Amount">
          <%= pricing.amount %>
        </:col>
        <:col :let={pricing} label="Duration">
          <%= pricing.duration %>
        </:col>
        <:col :let={pricing} label="Actions">
          <.button phx-click="edit_pricing" phx-value-id={pricing.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_pricing" phx-value-id={pricing.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_pricing", _params, socket) do
    {:noreply, push_patch(socket, to: "/pricings/new")}
  end

  def handle_event("edit_pricing", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/pricings/#{id}/edit")}
  end

  def handle_event("delete_pricing", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting pricing with ID: #{id}")
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
