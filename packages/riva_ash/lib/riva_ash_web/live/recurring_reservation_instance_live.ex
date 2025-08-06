defmodule RivaAshWeb.RecurringReservationInstanceLive do
  @moduledoc """
  LiveView for managing Recurring Reservation Instances.
  """
  use RivaAshWeb, :live_view

  # Import atomic design components
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Spinner
  import RivaAshWeb.Components.Molecules.NotificationToast
  import RivaAshWeb.Components.Molecules.ConfirmDialog
  import RivaAshWeb.Components.Forms.RecurringReservationInstanceForm
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.RecurringReservationInstance
  alias RivaAsh.Resources.RecurringReservation
  alias RivaAsh.Resources.Reservation
  alias RivaAsh.Resources.Business

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        try do
          # Get user's businesses first
          businesses = Business.read!(actor: user)
          business_ids = Enum.map(businesses, & &1.id)

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, get_page_title())
            |> assign(:business_ids, business_ids)
            |> assign(:form, nil)
            |> assign(:show_form, false)
            |> assign(:editing_instance, nil)
            |> assign(:loading, false)
            |> assign(:error_message, nil)
            |> assign(:success_message, nil)
            |> assign(:confirm_delete_id, nil)
            |> assign(:recurring_reservations, [])
            |> assign(:reservations, [])

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
    case list_instances_with_flop(user, business_ids, params) do
      {instances, meta} ->
        socket
        |> assign(:recurring_reservation_instances, instances)
        |> assign(:meta, meta)
        |> then(&{:noreply, &1})

      _ ->
        socket =
          socket
          |> assign(:recurring_reservation_instances, [])
          |> assign(:meta, %{})
          |> assign(:error_message, "Failed to load instances")

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
      <.spinner :if={@loading} size="lg" show_label={true} label="Loading..." class="fixed inset-0 flex items-center justify-center bg-black bg-opacity-50 z-50" aria-label="Loading instances" role="status" />

      <!-- Confirmation Dialog -->
      <.confirm_dialog
        :if={@confirm_delete_id}
        title="Delete Instance"
        message="Are you sure you want to delete this recurring reservation instance? This action cannot be undone."
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
      <.page_header title="Recurring Reservation Instances" description="View and manage individual instances of recurring reservations">
        <:action>
          <.button phx-click="new_recurring_reservation_instance" variant="primary" aria-label="Create new recurring reservation instance">New Instance</.button>
        </:action>
      </.page_header>

      <!-- Add/Edit Instance Form -->
      <%= if @show_form do %>
        <.recurring_reservation_instance_form
          form={@form}
          editing={@editing_instance != nil}
          loading={@loading}
          recurring_reservations={@recurring_reservations}
          reservations={@reservations}
          on_submit="save_instance"
          on_change="validate_instance"
          on_cancel="cancel_form"
        />
      <% end %>

      <!-- Instances Table -->
      <div class="bg-white shadow rounded-lg mt-6">
        <div class="sm:p-6 px-4 py-5">
          <%= if @recurring_reservation_instances == [] do %>
            <div class="text-center py-12" role="status" aria-live="polite">
              <p class="text-gray-500">No recurring reservation instances found.</p>
              <.button phx-click="new_recurring_reservation_instance" variant="primary" class="mt-4" aria-label="Create your first recurring reservation instance">Create Your First Instance</.button>
            </div>
          <% else %>
            <table class="min-w-full divide-y divide-gray-200" aria-label="Recurring Reservation Instances">
              <thead class="bg-gray-50">
                <tr>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Scheduled Date</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Sequence</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Status</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Recurring Reservation</th>
                  <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" aria-sort="none">Actions</th>
                </tr>
              </thead>
              <tbody class="bg-white divide-y divide-gray-200">
                <tr :for={instance <- @recurring_reservation_instances}>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Scheduled Date"><%= instance.scheduled_date %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Sequence"><%= instance.sequence_number %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Status">
                    <span class={"inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium #{
                                  case instance.status do
                                    :pending -> "bg-yellow-100 text-yellow-800"
                                    :confirmed -> "bg-green-100 text-green-800"
                                    :failed -> "bg-red-100 text-red-800"
                                    :skipped -> "bg-gray-100 text-gray-800"
                                    :cancelled -> "bg-gray-100 text-gray-800"
                                    _ -> "bg-gray-100 text-gray-800"
                                  end
                                }"}><%= instance.status %></span>
                  </td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900" data-label="Recurring Reservation"><%= instance.recurring_reservation_id %></td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm font-medium" data-label="Actions">
                    <.button phx-click="edit_recurring_reservation_instance" phx-value-id={instance.id} variant="primary" class="mr-2" aria-label={"Edit instance #{instance.id}"}>Edit</.button>
                    <.button phx-click="delete_recurring_reservation_instance" phx-value-id={instance.id} variant="destructive" aria-label={"Delete instance #{instance.id}"}>Delete</.button>
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
                     }"} aria-label={"Go to page #{page}" <> if(page == @meta.current_page, " (current page)", "")} aria-current={if(page == @meta.current_page, "page", "false")}><%= page %></.button>
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
  def handle_event("new_recurring_reservation_instance", _params, socket) do
    user = socket.assigns.current_user

    # Load related data
    recurring_reservations = load_recurring_reservations(user, socket.assigns.business_ids)
    reservations = load_reservations(user, socket.assigns.business_ids)

    form =
      AshPhoenix.Form.for_create(RecurringReservationInstance, :create, actor: user)
      |> to_form()

    socket =
      socket
      |> assign(:show_form, true)
      |> assign(:editing_instance, nil)
      |> assign(:form, form)
      |> assign(:recurring_reservations, recurring_reservations)
      |> assign(:reservations, reservations)

    {:noreply, socket}
  end

  def handle_event("edit_recurring_reservation_instance", %{"id" => id}, socket) do
    user = socket.assigns.current_user

    case RecurringReservationInstance.by_id(id, actor: user) do
      {:ok, instance} ->
        # Load related data
        recurring_reservations = load_recurring_reservations(user, socket.assigns.business_ids)
        reservations = load_reservations(user, socket.assigns.business_ids)

        form =
          AshPhoenix.Form.for_update(instance, :update, actor: user)
          |> to_form()

        socket =
          socket
          |> assign(:editing_instance, instance)
          |> assign(:form, form)
          |> assign(:show_form, true)
          |> assign(:recurring_reservations, recurring_reservations)
          |> assign(:reservations, reservations)

        {:noreply, socket}

      {:error, _reason} ->
        socket =
          socket
          |> assign(:error_message, "Instance not found.")
          |> assign(:show_form, false)

        {:noreply, socket}
    end
  end

  def handle_event("delete_recurring_reservation_instance", %{"id" => id}, socket) do
    # Show confirmation dialog
    {:noreply, assign(socket, :confirm_delete_id, id)}
  end

  def handle_event("confirm_delete", _params, socket) do
    instance_id = socket.assigns.confirm_delete_id
    user = socket.assigns.current_user

    socket =
      socket
      |> assign(:loading, true)
      |> assign(:confirm_delete_id, nil)

    case RecurringReservationInstance.by_id(instance_id, actor: user) do
      {:ok, instance} ->
        case Ash.destroy(instance, actor: user) do
          :ok ->
            socket =
              socket
              |> assign(:loading, false)
              |> assign(:success_message, "Instance deleted successfully!")

            {:noreply, push_patch(socket, to: ~p"/recurring-reservation-instances")}

          {:error, error} ->
            error_message = format_error_message(error)

            socket =
              socket
              |> assign(:loading, false)
              |> assign(:error_message, "Failed to delete instance: #{error_message}")

            {:noreply, socket}
        end

      {:error, error} ->
        error_message = format_error_message(error)

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:error_message, "Failed to find instance: #{error_message}")

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
      |> assign(:editing_instance, nil)
      |> assign(:form, nil)

    {:noreply, socket}
  end

  def handle_event("validate_instance", %{"form" => params}, socket) do
    form =
      if socket.assigns.editing_instance do
        AshPhoenix.Form.for_update(socket.assigns.editing_instance, :update, actor: socket.assigns.current_user)
      else
        AshPhoenix.Form.for_create(RecurringReservationInstance, :create, actor: socket.assigns.current_user)
      end
      |> AshPhoenix.Form.validate(params, errors: true)
      |> to_form()

    {:noreply, assign(socket, :form, form)}
  end

  def handle_event("save_instance", %{"form" => params}, socket) do
    user = socket.assigns.current_user

    socket = assign(socket, :loading, true)

    # Submit the form with the correct parameters
    result = AshPhoenix.Form.submit(socket.assigns.form, params: params, actor: user)

    case result do
      {:ok, instance} ->
        action_text = if socket.assigns.editing_instance, do: "updated", else: "created"

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:show_form, false)
          |> assign(:editing_instance, nil)
          |> assign(:form, nil)
          |> assign(:success_message, "Instance #{action_text} successfully!")

        {:noreply, push_patch(socket, to: ~p"/recurring-reservation-instances")}

      {:error, form} ->
        error_messages =
          AshPhoenix.Form.errors(form)
          |> Enum.map_join(", ", fn {field, {message, _}} -> "#{field}: #{message}" end)

        socket =
          socket
          |> assign(:loading, false)
          |> assign(:form, to_form(form))
          |> assign(:error_message, "Failed to save instance. #{error_messages}")

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
    {:noreply, push_patch(socket, to: ~p"/recurring-reservation-instances?page=#{page}")}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  defp list_instances_with_flop(user, business_ids, params) do
    # Get instances as a list
    instances = load_instances_simple(user, business_ids)

    # Validate Flop parameters first
    case Flop.validate(params, for: RecurringReservationInstance) do
      {:ok, flop} ->
        # Apply manual sorting and pagination to the list
        {paginated_instances, meta} = apply_flop_to_list(instances, flop)
        {paginated_instances, meta}

      {:error, meta} ->
        # Return empty results with error meta
        {[], meta}
    end
  end

  defp apply_flop_to_list(instances, flop) do
    # Apply sorting
    sorted_instances = apply_sorting(instances, flop)

    # Apply pagination
    total_count = length(sorted_instances)
    {paginated_instances, pagination_info} = apply_pagination(sorted_instances, flop)

    # Build meta information
    meta = build_meta(flop, total_count, pagination_info)

    {paginated_instances, meta}
  end

  defp apply_sorting(instances, %Flop{order_by: nil}), do: instances
  defp apply_sorting(instances, %Flop{order_by: []}), do: instances

  defp apply_sorting(instances, %Flop{order_by: order_by, order_directions: order_directions}) do
    # Default to :asc if no directions provided
    directions = order_directions || Enum.map(order_by, fn _ -> :asc end)

    # Zip order_by and directions, pad directions with :asc if needed
    order_specs =
      Enum.zip_with([order_by, directions], fn [field, direction] -> {field, direction} end)

    Enum.sort(instances, fn inst1, inst2 ->
      compare_instances(inst1, inst2, order_specs)
    end)
  end

  defp compare_instances(_inst1, _inst2, []), do: true

  defp compare_instances(inst1, inst2, [{field, direction} | rest]) do
    val1 = get_field_value(inst1, field)
    val2 = get_field_value(inst2, field)

    case {val1, val2} do
      {same, same} -> compare_instances(inst1, inst2, rest)
      {nil, _} -> direction == :desc
      {_, nil} -> direction == :asc
      {v1, v2} when direction == :asc -> v1 <= v2
      {v1, v2} when direction == :desc -> v1 >= v2
    end
  end

  defp get_field_value(instance, field) do
    case field do
      :scheduled_date -> instance.scheduled_date
      :sequence_number -> instance.sequence_number
      :status -> instance.status
      :inserted_at -> instance.inserted_at
      :updated_at -> instance.updated_at
      :recurring_reservation_id -> instance.recurring_reservation_id
      _ -> nil
    end
  end

  defp apply_pagination(instances, flop) do
    total_count = length(instances)

    cond do
      # Page-based pagination
      flop.page && flop.page_size ->
        page = max(flop.page, 1)
        page_size = flop.page_size
        offset = (page - 1) * page_size

        paginated = instances |> Enum.drop(offset) |> Enum.take(page_size)

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

        paginated = instances |> Enum.drop(offset) |> Enum.take(limit)

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
        paginated = Enum.take(instances, limit)

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

        {instances, pagination_info}
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

  defp load_instances_simple(user, business_ids) do
    try do
      # Get instances for user's businesses
      case RecurringReservationInstance.read(
             actor: user,
             filter: [
               recurring_reservation: [item: [section: [plot: [business_id: [in: business_ids]]]]]
             ]
           ) do
        {:ok, instances} -> instances
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp load_recurring_reservations(user, business_ids) do
    try do
      case RecurringReservation.read(
             actor: user,
             filter: [item: [section: [plot: [business_id: [in: business_ids]]]]]
           ) do
        {:ok, recurring_reservations} -> recurring_reservations
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp load_reservations(user, business_ids) do
    try do
      case Reservation.read(
             actor: user,
             filter: [item: [section: [plot: [business_id: [in: business_ids]]]]]
           ) do
        {:ok, reservations} -> reservations
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp get_page_title,
    do: Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Recurring Reservation Instances"

  defp format_error_message(error) do
    case RivaAsh.ErrorHelpers.format_error(error) do
      %{message: message} -> message
      _ -> "An unexpected error occurred"
    end
  end
end
