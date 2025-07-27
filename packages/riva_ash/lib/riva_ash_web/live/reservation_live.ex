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

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        try do
          # Get user's businesses first
          businesses = RivaAsh.Resources.Business.read!(actor: user)
          business_ids = Enum.map(businesses, & &1.id)

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, "Reservations")
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
        rescue
          error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
            {:ok, redirect(socket, to: "/access-denied")}
          error ->
            {:ok, socket |> assign(:error_message, "Failed to load data: #{Exception.message(error)}")}
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
    case list_reservations_with_flop(user, business_ids, params) do
      {reservations, meta} ->
        socket
        |> assign(:reservations, reservations)
        |> assign(:meta, meta)
        |> then(&{:noreply, &1})
      _ ->
        socket =
          socket
          |> assign(:reservations, [])
          |> assign(:meta, %{})
          |> assign(:error_message, "Failed to load reservations")
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
                                    _ -> "bg-gray-100 text-gray-800"
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
  def handle_event("new_reservation", _params, socket) do
    user = socket.assigns.current_user

    # Load related data
    clients = load_clients(user, socket.assigns.business_ids)
    items = load_items(user, socket.assigns.business_ids)
    employees = load_employees(user, socket.assigns.business_ids)

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
  end

  def handle_event("edit_reservation", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    case Reservation.by_id(id, actor: user) do
      {:ok, reservation} ->
        # Load related data
        clients = load_clients(user, socket.assigns.business_ids)
        items = load_items(user, socket.assigns.business_ids)
        employees = load_employees(user, socket.assigns.business_ids)

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

      {:error, _reason} ->
        socket =
          socket
          |> assign(:error_message, "Reservation not found.")
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

    case Reservation.by_id(reservation_id, actor: user) do
      {:ok, reservation} ->
        case Ash.destroy(reservation, actor: user) do
          :ok ->
            socket =
              socket
              |> assign(:loading, false)
              |> assign(:success_message, "Reservation deleted successfully!")

            {:noreply, push_patch(socket, to: ~p"/reservations")}

          {:error, error} ->
            error_message = format_error_message(error)
            socket =
              socket
              |> assign(:loading, false)
              |> assign(:error_message, "Failed to delete reservation: #{error_message}")

            {:noreply, socket}
        end

      {:error, error} ->
        error_message = format_error_message(error)
        socket =
          socket
          |> assign(:loading, false)
          |> assign(:error_message, "Failed to find reservation: #{error_message}")

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

    # Submit the form with the correct parameters
    result = AshPhoenix.Form.submit(socket.assigns.form, params: params, actor: user)

    case result do
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

      {:error, form} ->
        error_messages =
          form
          |> AshPhoenix.Form.errors()
          |> Enum.map(fn {field, {message, _}} -> "#{field}: #{message}" end)
          |> Enum.join(", ")

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:form, to_form(form))
          |> assign(:error_message, "Failed to save reservation. #{error_messages}")

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

  defp list_reservations_with_flop(user, business_ids, params) do
    # Get reservations as a list
    reservations = load_reservations_simple(user, business_ids)

    # Validate Flop parameters first
    case Flop.validate(params, for: Reservation) do
      {:ok, flop} ->
        # Apply manual sorting and pagination to the list
        {paginated_reservations, meta} = apply_flop_to_list(reservations, flop)
        {paginated_reservations, meta}

      {:error, meta} ->
        # Return empty results with error meta
        {[], meta}
    end
  end

  defp apply_flop_to_list(reservations, flop) do
    # Apply sorting
    sorted_reservations = apply_sorting(reservations, flop)

    # Apply pagination
    total_count = length(sorted_reservations)
    {paginated_reservations, pagination_info} = apply_pagination(sorted_reservations, flop)

    # Build meta information
    meta = build_meta(flop, total_count, pagination_info)

    {paginated_reservations, meta}
  end

  defp apply_sorting(reservations, %Flop{order_by: nil}), do: reservations
  defp apply_sorting(reservations, %Flop{order_by: []}), do: reservations
  defp apply_sorting(reservations, %Flop{order_by: order_by, order_directions: order_directions}) do
    # Default to :asc if no directions provided
    directions = order_directions || Enum.map(order_by, fn _ -> :asc end)

    # Zip order_by and directions, pad directions with :asc if needed
    order_specs = Enum.zip_with([order_by, directions], fn [field, direction] -> {field, direction} end)

    Enum.sort(reservations, fn res1, res2 ->
      compare_reservations(res1, res2, order_specs)
    end)
  end

  defp compare_reservations(_res1, _res2, []), do: true
  defp compare_reservations(res1, res2, [{field, direction} | rest]) do
    val1 = get_field_value(res1, field)
    val2 = get_field_value(res2, field)

    case {val1, val2} do
      {same, same} -> compare_reservations(res1, res2, rest)
      {nil, _} -> direction == :desc
      {_, nil} -> direction == :asc
      {v1, v2} when direction == :asc -> v1 <= v2
      {v1, v2} when direction == :desc -> v1 >= v2
    end
  end

  defp get_field_value(reservation, field) do
    case field do
      :client_id -> reservation.client_id
      :item_id -> reservation.item_id
      :reserved_from -> reservation.reserved_from
      :reserved_until -> reservation.reserved_until
      :status -> reservation.status
      :inserted_at -> reservation.inserted_at
      :updated_at -> reservation.updated_at
      _ -> nil
    end
  end

  defp apply_pagination(reservations, flop) do
    total_count = length(reservations)

    cond do
      # Page-based pagination
      flop.page && flop.page_size ->
        page = max(flop.page, 1)
        page_size = flop.page_size
        offset = (page - 1) * page_size

        paginated = reservations |> Enum.drop(offset) |> Enum.take(page_size)
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

        paginated = reservations |> Enum.drop(offset) |> Enum.take(limit)
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
        paginated = Enum.take(reservations, limit)
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
        {reservations, pagination_info}
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

  defp load_reservations_simple(user, business_ids) do
    try do
      # Get reservations for user's businesses
      case Reservation.read(actor: user, filter: [item: [section: [plot: [business_id: [in: business_ids]]]]]) do
        {:ok, reservations} -> reservations
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp load_clients(user, business_ids) do
    try do
      case Client.read(actor: user, filter: [business_id: [in: business_ids]]) do
        {:ok, clients} -> clients
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp load_items(user, business_ids) do
    try do
      case Item.read(actor: user, filter: [section: [plot: [business_id: [in: business_ids]]]]) do
        {:ok, items} -> items
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp load_employees(user, business_ids) do
    try do
      case Employee.read(actor: user, filter: [business_id: [in: business_ids]]) do
        {:ok, employees} -> employees
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp format_error_message(error) do
    case RivaAsh.ErrorHelpers.format_error(error) do
      %{message: message} -> message
      _ -> "An unexpected error occurred"
    end
  end
end
