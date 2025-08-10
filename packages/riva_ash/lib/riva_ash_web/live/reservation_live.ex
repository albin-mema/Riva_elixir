# alias RivaAshWeb.Components.Organisms, as: Organisms
# alias RivaAshWeb.Components.Atoms, as: Atoms
# alias RivaAshWeb.Components.Molecules, as: Molecules
# alias RivaAshWeb.Components.Forms, as: Forms
# alias RivaAshWeb.Live, as: Live
# alias RivaAsh.Resources, as: Resources
# alias RivaAsh.Reservation, as: Reservation
# alias AshPhoenix.Form, as: Form

defmodule RivaAshWeb.ReservationLive do
  @moduledoc """
  LiveView for managing Reservations.
  """
  use RivaAshWeb, :live_view

  # Import atomic design components
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Spinner
  import RivaAshWeb.Components.Molecules.NotificationToast
  import RivaAshWeb.Components.Molecules.ConfirmDialog
  import RivaAshWeb.Components.Forms.ReservationForm
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.Reservation
  alias RivaAsh.Resources.Client
  alias RivaAsh.Resources.Item
  alias RivaAsh.Resources.Employee
  alias RivaAsh.Reservation.ReservationService
  alias RivaAsh.ErrorHelpers

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        case ReservationService.get_user_businesses(user) do
          {:ok, business_ids} ->
            socket =
              socket
              |> assign(:current_user, user)
              |> assign(:page_title, get_page_title())
              |> assign(:business_ids, business_ids)
              |> assign(:form, nil)
              |> assign(:show_form, false)
              |> assign(:editing_reservation, nil)
              |> assign(:loading, false)
              |> assign(:error_message, nil)
              |> assign(:success_message, nil)
              |> assign(:confirm_delete_id, nil)
              |> assign(:clients, [])
              |> assign(:items, [])
              |> assign(:employees, [])

            {:ok, socket}

          {:error, error} ->
            error_message = ErrorHelpers.format_error(error)
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, :not_authenticated} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def handle_params(params, _uri, socket) do
    user = socket.assigns.current_user
    business_ids = socket.assigns.business_ids

    # Use Flop to handle pagination, sorting, and filtering
    case ReservationService.list_reservations_with_flop(user, business_ids, params) do
      {reservations, meta} ->
        socket
        |> assign(:reservations, reservations)
        |> assign(:meta, meta)
        |> then(&{:noreply, &1})

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)

        socket =
          socket
          |> assign(:reservations, [])
          |> assign(:meta, %{})
          |> assign(:error_message, "Failed to load reservations: #{error_message}")

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
      <.spinner :if={@loading} size="lg" show_label={true} label="Loading..." class="fixed inset-0 flex items-center justify-center bg-black bg-opacity-50 z-50" aria-label="Loading reservations" role="status" />

      <!-- Confirmation Dialog -->
      <.confirm_dialog
        :if={@confirm_delete_id}
        title="Delete Reservation"
        message="Are you sure you want to delete this reservation? This action cannot be undone."
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
      <.page_header title="Reservations" description="Manage all client reservations">
        <:action>
          <.button phx-click="new_reservation" variant="primary" aria-label="Create new reservation">New Reservation</.button>
        </:action>
      </.page_header>

      <!-- Add/Edit Reservation Form -->
      <%= if @show_form do %>
        <.reservation_form
          form={@form}
          editing={@editing_reservation != nil}
          loading={@loading}
          clients={@clients}
          items={@items}
          employees={@employees}
          on_submit="save_reservation"
          on_change="validate_reservation"
          on_cancel="cancel_form"
        />
      <% end %>

      <!-- Reservations Table -->
      <div class="bg-white shadow rounded-lg mt-6">
        <div class="sm:p-6 px-4 py-5">
          <%= if @reservations == [] do %>
            <div class="text-center py-12" role="status" aria-live="polite">
              <p class="text-gray-500">No reservations found.</p>
              <.button phx-click="new_reservation" variant="primary" class="mt-4" aria-label="Create your first reservation">Create Your First Reservation</.button>
            </div>
          <% else %>
            <table class="min-w-full divide-y divide-gray-200" aria-label="Reservations">
              <thead class="bg-gray-50">
                <tr>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Client ID</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Item ID</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Start Time</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">End Time</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Status</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Actions</th>
                </tr>
              </thead>
              <tbody class="bg-white divide-y divide-gray-200">
                <tr :for={reservation <- @reservations}>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Client ID"><%= reservation.client_id %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Item ID"><%= reservation.item_id %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Start Time"><%= reservation.reserved_from %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="End Time"><%= reservation.reserved_until %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Status">
                    <span class={"inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium #{
                                  case reservation.status do
                                    :pending -> "bg-yellow-100 text-yellow-800"
                                    :provisional -> "bg-blue-100 text-blue-800"
                                    :confirmed -> "bg-green-100 text-green-800"
                                    :cancelled -> "bg-red-100 text-red-800"
                                    :completed -> "bg-gray-100 text-gray-800"
                                    _unmatchedunmatched -> "bg-gray-100 text-gray-800"
                                  end
                                }"}><%= reservation.status %></span>
                  </td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm font-medium" data-label="Actions">
                    <.button phx-click="edit_reservation" phx-value-id={reservation.id} variant="primary" class="mr-2" aria-label={"Edit reservation #{reservation.id}"}>Edit</.button>
                    <.button phx-click="delete_reservation" phx-value-id={reservation.id} variant="destructive" aria-label={"Delete reservation #{reservation.id}"}>Delete</.button>
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
                     }"} aria-label={"Go to page #{page}" <> (if page == @meta.current_page, do: " (current page)", else: "")} aria-current={if page == @meta.current_page, do: "page", else: "false"}><%= page %></.button>
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
  def handle_event("new_reservation", _params, socket) do
    user = socket.assigns.current_user

    case ReservationService.load_reservation_form_data(user, socket.assigns.business_ids) do
      {:ok, %{clients: clients, items: items, employees: employees}} ->
        form =
          AshPhoenix.Form.for_create(Reservation, :create, actor: user)
          |> to_form()

        socket =
          socket
          |> assign(:show_form, true)
          |> assign(:editing_reservation, nil)
          |> assign(:form, form)
          |> assign(:clients, clients)
          |> assign(:items, items)
          |> assign(:employees, employees)

        {:noreply, socket}

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)

        socket =
          socket
          |> assign(:error_message, "Failed to load form data: #{error_message}")
          |> assign(:show_form, false)

        {:noreply, socket}
    end
  end

  def handle_event("edit_reservation", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    case ReservationService.get_reservation_for_edit(id, user, socket.assigns.business_ids) do
      {:ok, %{reservation: reservation, clients: clients, items: items, employees: employees}} ->
        form =
          AshPhoenix.Form.for_update(reservation, :update, actor: user)
          |> to_form()

        socket =
          socket
          |> assign(:editing_reservation, reservation)
          |> assign(:form, form)
          |> assign(:show_form, true)
          |> assign(:clients, clients)
          |> assign(:items, items)
          |> assign(:employees, employees)

        {:noreply, socket}

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)

        socket =
          socket
          |> assign(:error_message, "Failed to load reservation: #{error_message}")
          |> assign(:show_form, false)

        {:noreply, socket}
    end
  end

  def handle_event("delete_reservation", %{"id" => id}, socket) do
    # Show confirmation dialog
    {:noreply, assign(socket, :confirm_delete_id, id)}
  end

  def handle_event("confirm_delete", _params, socket) do
    reservation_id = socket.assigns.confirm_delete_id
    user = socket.assigns.current_user

    socket =
      socket
      |> assign(:loading, true)
      |> assign(:confirm_delete_id, nil)

    case ReservationService.delete_reservation(reservation_id, user) do
      {:ok, _reservation} ->
        socket =
          socket
          |> assign(:loading, false)
          |> assign(:success_message, "Reservation deleted successfully!")

        {:noreply, push_patch(socket, to: ~p"/reservations")}

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:error_message, "Failed to delete reservation: #{error_message}")

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
      |> assign(:editing_reservation, nil)
      |> assign(:form, nil)

    {:noreply, socket}
  end

  def handle_event("validate_reservation", %{"form" => params}, socket) do
    form =
      if socket.assigns.editing_reservation do
        AshPhoenix.Form.for_update(socket.assigns.editing_reservation, :update, actor: socket.assigns.current_user)
      else
        AshPhoenix.Form.for_create(Reservation, :create, actor: socket.assigns.current_user)
      end
      |> AshPhoenix.Form.validate(params, errors: true)
      |> to_form()

    {:noreply, assign(socket, :form, form)}
  end

  def handle_event("save_reservation", %{"form" => params}, socket) do
    user = socket.assigns.current_user

    socket = assign(socket, :loading, true)

    case ReservationService.save_reservation(socket.assigns.form, params, user) do
      {:ok, reservation} ->
        action_text = if socket.assigns.editing_reservation, do: "updated", else: "created"

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:show_form, false)
          |> assign(:editing_reservation, nil)
          |> assign(:form, nil)
          |> assign(:success_message, "Reservation #{action_text} successfully!")

        {:noreply, push_patch(socket, to: ~p"/reservations")}

      {:error, form, error} ->
        error_message = ErrorHelpers.format_error(error)

        error_messages =
          AshPhoenix.Form.errors(form)
          |> Enum.map_join(", ", fn {field, {message, _unmatched}} -> "#{field}: #{message}" end)

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:form, to_form(form))
          |> assign(:error_message, "Failed to save reservation. #{error_messages} #{error_message}")

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
    {:noreply, push_patch(socket, to: ~p"/reservations?page=#{page}")}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Reservations"
end
