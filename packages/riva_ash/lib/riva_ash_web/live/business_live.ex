defmodule RivaAshWeb.BusinessLive do
  use RivaAshWeb, :live_view
  import OK, only: [success: 1, failure: 1, ~>>: 2]

  require Ash.Query
  import Ash.Expr

  # Import atomic design components
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Badge
  import RivaAshWeb.Components.Molecules.EmptyState
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.BusinessForm
  import RivaAshWeb.Components.Organisms.BusinessCard
  import SaladUI.Alert

  alias RivaAsh.Resources.Business

  @impl true
  def mount(_params, session, socket) do
    # Get the current user from the session
    user = get_current_user_from_session(session)

    if user do
      changeset = Business |> Ash.Changeset.for_create(:create) |> Map.put(:action, :validate)

      socket =
        socket
        |> assign(:current_user, user)
        |> assign(:businesses, list_user_businesses(user))
        |> assign(:form, AshPhoenix.Form.for_create(Business, :create, actor: user) |> to_form())
        |> assign(:changeset, changeset)
        |> assign(:show_form, false)
        |> assign(:editing_business, nil)
        |> assign(:loading, false)
        |> assign(:is_admin, user.role == :admin)
        |> assign(:business_count, count_user_businesses(user))
        |> assign(:page_title, "Business Management")

      success(socket)
    else
      # User not authenticated, redirect to sign in
      socket =
        socket
        |> put_flash(:error, "You must be logged in to access this page.")
        |> redirect(to: "/sign-in")

      success(socket)
    end
  end

  @impl true
  def handle_params(_params, _uri, socket) do
    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!-- Page Header -->
    <.page_header
      title="Business Management"
      description="Manage your business entities and their information"
      icon={:building_office_2}
    >
      <:badge>
        <.badge variant="outline">
          <%= @business_count %> <%= if @business_count == 1, do: "business", else: "businesses" %>
        </.badge>
      </:badge>

      <:action>
        <.button
          variant={if @show_form, do: "outline", else: "primary"}
          phx-click="toggle_form"
          icon_left={if @show_form, do: :x_mark, else: :plus}
        >
          <%= if @show_form, do: "Cancel", else: "Add Business" %>
        </.button>
      </:action>
    </.page_header>

        <!-- Success/Error Messages -->
        <div :if={@flash["info"]} class="mb-6">
          <.alert variant="default">
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
            <.alert_title>Success</.alert_title>
            <.alert_description><%= @flash["info"] %></.alert_description>
          </.alert>
        </div>

        <div :if={@flash["error"]} class="mb-6">
          <.alert variant="destructive">
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
            <.alert_title>Error</.alert_title>
            <.alert_description><%= @flash["error"] %></.alert_description>
          </.alert>
        </div>

        <!-- Business Form -->
        <div :if={@show_form} class="mb-8">
          <.business_form
            form={@form}
            editing={@editing_business != nil}
            loading={@loading}
            on_submit="save_business"
            on_change="validate_business"
            on_cancel="cancel_form"
          />
        </div>

        <!-- Business List -->
        <div class="space-y-4">
          <h2 class="font-semibold text-foreground text-xl">Existing Businesses</h2>

          <div :if={@businesses == []}>
            <.empty_state
              icon={:building_office_2}
              title="No businesses found"
              description="Create your first business to get started"
              variant="bordered"
            />
          </div>

          <div :if={@businesses != []} class="gap-4 grid">
            <.business_card
              :for={business <- @businesses}
              business={business}
              current_user={@current_user}
              is_admin={@is_admin}
              on_edit="edit_business"
              on_delete="delete_business"
            />
          </div>
        </div>
    """
  end

  # Event Handlers

  @impl true
  def handle_event("toggle_form", _params, socket) do
    socket =
      socket
      |> assign(:show_form, !socket.assigns.show_form)
      |> assign(:editing_business, nil)
      |> assign(:form, AshPhoenix.Form.for_create(Business, :create, actor: socket.assigns.current_user) |> to_form())

    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_form", _params, socket) do
    socket =
      socket
      |> assign(:show_form, false)
      |> assign(:editing_business, nil)
      |> assign(:form, AshPhoenix.Form.for_create(Business, :create, actor: socket.assigns.current_user) |> to_form())

    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_business", %{"form" => params}, socket) do
    form = AshPhoenix.Form.validate(socket.assigns.form, params) |> to_form()
    {:noreply, assign(socket, :form, form)}
  end

  @impl true
  def handle_event("save_business", %{"form" => params}, socket) do
    IO.puts("=== SAVE BUSINESS EVENT TRIGGERED ===")
    IO.inspect(params, label: "Form params")

    socket = assign(socket, :loading, true)
    user = socket.assigns.current_user

    # The Business resource will automatically set owner_id from the actor
    AshPhoenix.Form.submit(socket.assigns.form, params: params, actor: user)
    ~>> fn business ->
      action_text = if socket.assigns.editing_business, do: "updated", else: "created"

      socket
      |> assign(:businesses, list_user_businesses(user))
      |> assign(:business_count, count_user_businesses(user))
      |> assign(:show_form, false)
      |> assign(:editing_business, nil)
      |> assign(
        :form,
        AshPhoenix.Form.for_create(Business, :create, actor: user) |> to_form()
      )
      |> assign(:loading, false)
      |> put_flash(:info, "Business \"#{business.name}\" #{action_text} successfully!")
    end
    |> case do
      {:ok, socket} -> {:noreply, socket}

      {:error, %Ash.Error.Forbidden{}} ->
        socket =
          socket
          |> assign(:loading, false)
          |> put_flash(:error, "You do not have permission to perform this action.")

        {:noreply, socket}

      {:error, form} ->
        IO.inspect(form, label: "Form errors")

        socket =
          socket
          |> assign(:form, form |> to_form())
          |> assign(:loading, false)
          |> put_flash(:error, "Please fix the errors below and try again.")

        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("edit_business", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    OK.for do
      business <- Business 
                 |> Ash.get(id, actor: user) 
                 |> OK.required(:business_not_found)
      _ <- authorize_business_access(business, user)
      form = AshPhoenix.Form.for_update(business, :update, actor: user) |> to_form()
    after
      socket =
        socket
        |> assign(:show_form, true)
        |> assign(:editing_business, business)
        |> assign(:form, form)
      {:noreply, socket}
    else
      :business_not_found ->
        {:noreply, put_flash(socket, :error, "Business not found or access denied.")}
      :access_denied ->
        {:noreply, put_flash(socket, :error, "You do not have permission to edit this business.")}
      error ->
        {:noreply, put_flash(socket, :error, "An error occurred: #{inspect(error)}")}
    end
  end

  @impl true
  def handle_event("delete_business", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    OK.for do
      business <- Business 
                 |> Ash.get(id, actor: user) 
                 |> OK.required(:business_not_found)
      _ <- authorize_business_access(business, user)
      _ <- business |> Ash.destroy(actor: user)
    after
      socket
      |> assign(:businesses, list_user_businesses(user))
      |> assign(:business_count, count_user_businesses(user))
      |> put_flash(:info, "Business \"#{business.name}\" deleted successfully!")
    else
      :business_not_found ->
        put_flash(socket, :error, "Business not found or access denied.")
      :access_denied ->
        put_flash(socket, :error, "You do not have permission to delete this business.")
      %Ash.Error.Forbidden{} ->
        put_flash(socket, :error, "You do not have permission to delete this business.")
      error ->
        put_flash(socket, :error, "Failed to delete business: #{inspect(error)}")
    end
    |> case do
      {:ok, updated_socket} -> {:noreply, updated_socket}
      {:error, updated_socket} -> {:noreply, updated_socket}
    end
  end

  # Helper Functions

  defp authorize_business_access(business, user) do
    if business.owner_id == user.id || user.role == :admin do
      success(:ok)
    else
      failure(:access_denied)
    end
  end

  defp list_user_businesses(user) do
    query = if user.role == :admin do
      # Admins can see all businesses
      Business |> Ash.Query.load(:owner)
    else
      # Regular users can only see their own businesses
      Business
      |> Ash.Query.filter(expr(owner_id == ^user.id))
      |> Ash.Query.load(:owner)
    end

    query
    |> Ash.read(actor: user)
    ~>> fn businesses ->
      Enum.sort_by(businesses, & &1.inserted_at, {:desc, DateTime})
    end
    |> case do
      {:ok, businesses} -> businesses
      {:error, _} -> []
    end
  end

  defp count_user_businesses(user) do
    query = if user.role == :admin do
      # Count all businesses for admin
      Business
    else
      # Count only owned businesses for regular users
      Business |> Ash.Query.filter(expr(owner_id == ^user.id))
    end

    query
    |> Ash.count(actor: user)
    |> case do
      {:ok, count} -> count
      {:error, _} -> 0
    end
  end

  defp get_current_user_from_session(session) do
    OK.for do
      user_token <- session["user_token"] |> OK.required(:no_token)
      user_id <- Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", user_token, max_age: 86_400)
      user <- Ash.get(RivaAsh.Accounts.User, user_id, domain: RivaAsh.Accounts)
    after
      user
    else
      _ -> nil
    end
  end
end
