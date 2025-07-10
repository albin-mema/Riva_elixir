defmodule RivaAshWeb.BusinessLive do
  use RivaAshWeb, :live_view

  import SaladUI.Button
  import SaladUI.Card
  import SaladUI.Input
  import SaladUI.Label
  import SaladUI.Textarea
  import SaladUI.Alert
  import SaladUI.Badge

  alias RivaAsh.Resources.Business

  @impl true
  def mount(_params, _session, %{assigns: %{current_user: user}} = socket) do
    changeset = Business |> Ash.Changeset.for_create(:create) |> Map.put(:action, :validate)

    socket =
      socket
      |> assign(:current_user, user)
      |> assign(:businesses, list_businesses(user))
      |> assign(:form, AshPhoenix.Form.for_create(Business, :create, actor: user))
      |> assign(:changeset, changeset)
      |> assign(:show_form, false)
      |> assign(:editing_business, nil)
      |> assign(:loading, false)

    {:ok, socket}
  end

  def mount(_params, _session, socket) do
    # This should be handled by the require_authenticated_user plug
    socket =
      socket
      |> assign(:businesses, [])
      |> assign(:form, nil)
      |> assign(:changeset, nil)
      |> assign(:show_form, false)
      |> assign(:editing_business, nil)
      |> assign(:loading, false)

    {:ok, socket}
  end

  @impl true
  def handle_params(_params, _uri, socket) do
    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="bg-background min-h-screen">
      <div class="mx-auto px-4 py-8 container">
        <!-- Header -->
        <div class="flex justify-between items-center mb-8">
          <div>
            <h1 class="font-bold text-foreground text-3xl">Business Management</h1>
            <p class="mt-2 text-muted-foreground">Manage your business entities and their information</p>
          </div>
          <.button
            variant="default"
            phx-click="toggle_form"
            class="flex items-center gap-2"
          >
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4" />
            </svg>
            <%= if @show_form, do: "Cancel", else: "Add Business" %>
          </.button>
        </div>

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
          <.card>
            <.card_header>
              <.card_title>
                <%= if @editing_business, do: "Edit Business", else: "Create New Business" %>
              </.card_title>
              <.card_description>
                <%= if @editing_business do %>
                  Update the business information below.
                <% else %>
                  Fill in the details to create a new business entity.
                <% end %>
              </.card_description>
            </.card_header>
            <.card_content>
              <.form
                for={@form}
                phx-submit="save_business"
                phx-change="validate_business"
                class="space-y-6"
              >
                <div class="space-y-2">
                  <.label for="name">Business Name *</.label>
                  <.input
                    field={@form[:name]}
                    type="text"
                    placeholder="Enter business name"
                    required
                  />
                  <div :if={@form[:name].errors != []} class="text-destructive text-sm">
                    <%= Enum.map(@form[:name].errors, fn error ->
                      Phoenix.HTML.raw("• #{error}<br>")
                    end) %>
                  </div>
                </div>

                <div class="space-y-2">
                  <.label for="description">Description</.label>
                  <.textarea
                    field={@form[:description]}
                    placeholder="Enter business description (optional)"
                    rows="4"
                  />
                  <div :if={@form[:description].errors != []} class="text-destructive text-sm">
                    <%= Enum.map(@form[:description].errors, fn error ->
                      Phoenix.HTML.raw("• #{error}<br>")
                    end) %>
                  </div>
                </div>

                <div class="flex gap-3 pt-4">
                  <.button
                    type="submit"
                    variant="default"
                    disabled={@loading}
                    class="flex items-center gap-2"
                  >
                    <span :if={@loading} class="animate-spin">⏳</span>
                    <%= if @editing_business, do: "Update Business", else: "Create Business" %>
                  </.button>

                  <.button
                    type="button"
                    variant="outline"
                    phx-click="cancel_form"
                  >
                    Cancel
                  </.button>
                </div>
              </.form>
            </.card_content>
          </.card>
        </div>

        <!-- Business List -->
        <div class="space-y-4">
          <h2 class="font-semibold text-foreground text-xl">Existing Businesses</h2>

          <div :if={@businesses == []} class="py-12 text-center">
            <div class="text-muted-foreground">
              <svg class="opacity-50 mx-auto mb-4 w-12 h-12" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4" />
              </svg>
              <p class="font-medium text-lg">No businesses found</p>
              <p class="mt-1 text-sm">Create your first business to get started</p>
            </div>
          </div>

          <div :if={@businesses != []} class="gap-4 grid">
            <.card :for={business <- @businesses} class="hover:shadow-md transition-shadow">
              <.card_content class="p-6">
                <div class="flex justify-between items-start">
                  <div class="flex-1">
                    <div class="flex items-center gap-3 mb-2">
                      <h3 class="font-semibold text-foreground text-lg"><%= business.name %></h3>
                      <.badge variant="secondary">
                        ID: <%= String.slice(business.id, 0, 8) %>...
                      </.badge>
                    </div>

                    <p :if={business.description} class="mb-4 text-muted-foreground">
                      <%= business.description %>
                    </p>
                    <p :if={!business.description} class="mb-4 text-muted-foreground italic">
                      No description provided
                    </p>

                    <div class="flex items-center gap-4 text-muted-foreground text-sm">
                      <span>Created: <%= Calendar.strftime(business.inserted_at, "%B %d, %Y") %></span>
                      <span :if={business.updated_at != business.inserted_at}>
                        Updated: <%= Calendar.strftime(business.updated_at, "%B %d, %Y") %>
                      </span>
                    </div>
                  </div>

                  <div class="flex items-center gap-2 ml-4">
                    <.button
                      variant="outline"
                      size="sm"
                      phx-click="edit_business"
                      phx-value-id={business.id}
                    >
                      Edit
                    </.button>
                    <.button
                      variant="destructive"
                      size="sm"
                      phx-click="delete_business"
                      phx-value-id={business.id}
                      data-confirm="Are you sure you want to delete this business? This action cannot be undone."
                    >
                      Delete
                    </.button>
                  </div>
                </div>
              </.card_content>
            </.card>
          </div>
        </div>
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
      |> assign(:form, AshPhoenix.Form.for_create(Business, :create))

    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_form", _params, socket) do
    socket =
      socket
      |> assign(:show_form, false)
      |> assign(:editing_business, nil)
      |> assign(:form, AshPhoenix.Form.for_create(Business, :create))

    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_business", %{"form" => params}, socket) do
    form = AshPhoenix.Form.validate(socket.assigns.form, params)
    {:noreply, assign(socket, :form, form)}
  end

  @impl true
  def handle_event("save_business", %{"form" => params}, socket) do
    socket = assign(socket, :loading, true)

    case AshPhoenix.Form.submit(socket.assigns.form, params: params) do
      {:ok, business} ->
        action_text = if socket.assigns.editing_business, do: "updated", else: "created"

        socket =
          socket
          |> assign(:businesses, list_businesses(socket.assigns.current_user))
          |> assign(:show_form, false)
          |> assign(:editing_business, nil)
          |> assign(
            :form,
            AshPhoenix.Form.for_create(Business, :create, actor: socket.assigns.current_user)
          )
          |> assign(:loading, false)
          |> put_flash(:info, "Business \"#{business.name}\" #{action_text} successfully!")

        {:noreply, socket}

      {:error, form} ->
        socket =
          socket
          |> assign(:form, form)
          |> assign(:loading, false)
          |> put_flash(:error, "Please fix the errors below and try again.")

        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("edit_business", %{"id" => id}, socket) do
    try do
      business = Business |> Ash.get!(id)
      form = AshPhoenix.Form.for_update(business, :update)

      socket =
        socket
        |> assign(:show_form, true)
        |> assign(:editing_business, business)
        |> assign(:form, form)

      {:noreply, socket}
    rescue
      Ash.Error.Query.NotFound ->
        socket = put_flash(socket, :error, "Business not found.")
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("delete_business", %{"id" => id}, socket) do
    try do
      business = Business |> Ash.get!(id)

      case business |> Ash.destroy(actor: socket.assigns.current_user) do
        :ok ->
          socket =
            socket
            |> assign(:businesses, list_businesses(socket.assigns.current_user))
            |> put_flash(:info, "Business \"#{business.name}\" deleted successfully!")

          {:noreply, socket}

        {:error, _error} ->
          socket = put_flash(socket, :error, "Failed to delete business. Please try again.")
          {:noreply, socket}
      end
    rescue
      Ash.Error.Query.NotFound ->
        socket = put_flash(socket, :error, "Business not found.")
        {:noreply, socket}
    end
  end

  # Helper Functions

  defp list_businesses(actor) do
    Business
    |> Ash.read!(actor: actor)
    |> case do
      {:ok, businesses} -> Enum.sort_by(businesses, & &1.inserted_at, {:desc, DateTime})
      _ -> []
    end
  end
end
