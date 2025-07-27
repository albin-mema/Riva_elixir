defmodule RivaAshWeb.PlotLive do
  @moduledoc """
  LiveView for managing Plots.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Molecules.ConfirmDialog
  import RivaAshWeb.Components.Molecules.NotificationToast
  import RivaAshWeb.Components.Atoms.Spinner
  import Flop.Phoenix, except: [table: 1]

  alias RivaAsh.Resources.Plot
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
            |> assign(:page_title, "Plots")
            |> assign(:business_ids, business_ids)
            |> assign(:confirm_delete_id, nil)
            |> assign(:loading, false)
            |> assign(:error_message, nil)
            |> assign(:success_message, nil)

          {:ok, socket}
        rescue
          error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
            {:ok, redirect(socket, to: "/access-denied")}
          error ->
            {:ok, socket |> assign(:error_message, "Failed to load plots: #{Exception.message(error)}")}
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
    case list_plots_with_flop(user, business_ids, params) do
      {plots, meta} ->
        socket
        |> assign(:plots, plots)
        |> assign(:meta, meta)
        |> then(&{:noreply, &1})
      _ ->
        socket =
          socket
          |> assign(:plots, [])
          |> assign(:meta, %{})
          |> assign(:error_message, "Failed to load plots")
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
      <.spinner :if={@loading} size="lg" show_label={true} label="Loading..." class="fixed inset-0 flex items-center justify-center bg-black bg-opacity-50 z-50" aria-label="Loading plots" role="status" />

      <!-- Confirmation Dialog -->
      <.confirm_dialog
        :if={@confirm_delete_id}
        title="Delete Plot"
        message="Are you sure you want to delete this plot? This action cannot be undone."
        confirm_label="Delete"
        cancel_label="Cancel"
        variant="destructive"
        on_confirm="confirm_delete"
        on_cancel="cancel_delete"
        show={@confirm_delete_id != nil}
        aria-modal="true"
        role="dialog"
      />

      <.page_header title="Plots" description="Manage plots and their attributes">
        <:action>
          <.button phx-click="new_plot" class="bg-blue-600 hover:bg-blue-700">New Plot</.button>
        </:action>
      </.page_header>

      <%= if @plots == [] do %>
        <div class="text-center py-12">
          <p class="text-gray-500">No plots found.</p>
        </div>
      <% else %>
        <Flop.Phoenix.table
          items={@plots}
          meta={@meta}
          path={~p"/plots"}
          id="plots-table"
          opts={[
            container: true,
            container_attrs: [class: "overflow-hidden"],
            table_attrs: [class: "min-w-full divide-y divide-gray-200", role: "table", "aria-label": "Plots table"],
            thead_attrs: [class: "bg-gray-50"],
            tbody_attrs: [class: "bg-white divide-y divide-gray-200"],
            th_wrapper_attrs: [class: "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"],
            tbody_td_attrs: [class: "px-6 py-4 whitespace-nowrap text-sm text-gray-900"]
          ]}
        >
          <:col :let={plot} label="Name" field={:name}>
            <%= plot.name %>
          </:col>
          <:col :let={plot} label="Description">
            <%= plot.description || "—" %>
          </:col>
          <:col :let={plot} label="Status">
            <span class={if plot.is_active, do: "text-green-600", else: "text-red-600"}>
              <%= if plot.is_active, do: "Active", else: "Inactive" %>
            </span>
          </:col>
          <:col :let={plot} label="Actions">
            <div class="flex space-x-2">
              <.button
                variant="outline"
                size="sm"
                phx-click="edit_plot"
                phx-value-id={plot.id}
                aria-label={"Edit plot #{plot.name}"}
              >
                Edit
              </.button>
              <.button
                variant="destructive"
                size="sm"
                phx-click="delete_plot"
                phx-value-id={plot.id}
                aria-label={"Delete plot #{plot.name}"}
              >
                Delete
              </.button>
            </div>
          </:col>
        </Flop.Phoenix.table>

        <!-- Pagination -->
        <div class="mt-6">
          <Flop.Phoenix.pagination
            meta={@meta}
            path={~p"/plots"}
            class="flex justify-between items-center"
            page_link_attrs={[class: "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50", "aria-label": "Go to page"]}
            current_page_link_attrs={[class: "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-500 bg-gray-100 cursor-default", "aria-current": "page"]}
            disabled_link_attrs={[class: "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-400 bg-gray-100 cursor-not-allowed", "aria-disabled": "true"]}
            aria-label="Pagination navigation"
          />
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("new_plot", _params, socket) do
    {:noreply, push_patch(socket, to: "/plots/new")}
  end

  def handle_event("edit_plot", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/plots/#{id}/edit")}
  end

  def handle_event("delete_plot", %{"id" => id}, socket) do
    # Show confirmation dialog
    {:noreply, assign(socket, :confirm_delete_id, id)}
  end

  def handle_event("confirm_delete", _params, socket) do
    plot_id = socket.assigns.confirm_delete_id

    socket =
      socket
      |> assign(:loading, true)
      |> assign(:confirm_delete_id, nil)

    case delete_plot(plot_id, socket.assigns.current_user) do
      {:ok, _} ->
        # Instead of manually refreshing, we'll push a patch to reload with current params
        socket =
          socket
          |> assign(:loading, false)
          |> assign(:success_message, "Plot deleted successfully")

        {:noreply, push_patch(socket, to: ~p"/plots")}

      {:error, error} ->
        error_message = format_error_message(error)
        socket =
          socket
          |> assign(:loading, false)
          |> assign(:error_message, error_message)

        {:noreply, socket}
    end
  end

  def handle_event("cancel_delete", _params, socket) do
    {:noreply, assign(socket, :confirm_delete_id, nil)}
  end

  def handle_event("clear_error", _params, socket) do
    {:noreply, assign(socket, :error_message, nil)}
  end

  def handle_event("clear_success", _params, socket) do
    {:noreply, assign(socket, :success_message, nil)}
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

  defp delete_plot(plot_id, user) do
    try do
      with {:ok, plot} <- Ash.get(RivaAsh.Resources.Plot, plot_id, actor: user) |> RivaAsh.ErrorHelpers.to_result(),
           {:ok, _} <- Ash.destroy(plot, actor: user) |> RivaAsh.ErrorHelpers.to_result() do
        RivaAsh.ErrorHelpers.success(:ok)
      end
    rescue
      e ->
        RivaAsh.ErrorHelpers.failure(e)
    end
  end

  defp format_error_message(error) do
    case RivaAsh.ErrorHelpers.format_error(error) do
      %{message: message} -> message
      _ -> "An unexpected error occurred"
    end
  end

  defp list_plots_with_flop(user, business_ids, params) do
    # Get plots as a list
    plots = load_plots_simple(user, business_ids)

    # Validate Flop parameters first
    case Flop.validate(params, for: Plot) do
      {:ok, flop} ->
        # Apply manual sorting and pagination to the list
        {paginated_plots, meta} = apply_flop_to_list(plots, flop)
        {paginated_plots, meta}

      {:error, meta} ->
        # Return empty results with error meta
        {[], meta}
    end
  end

  defp apply_flop_to_list(plots, flop) do
    # Apply sorting
    sorted_plots = apply_sorting(plots, flop)

    # Apply pagination
    total_count = length(sorted_plots)
    {paginated_plots, pagination_info} = apply_pagination(sorted_plots, flop)

    # Build meta information
    meta = build_meta(flop, total_count, pagination_info)

    {paginated_plots, meta}
  end

  defp apply_sorting(plots, %Flop{order_by: nil}), do: plots
  defp apply_sorting(plots, %Flop{order_by: []}), do: plots
  defp apply_sorting(plots, %Flop{order_by: order_by, order_directions: order_directions}) do
    # Default to :asc if no directions provided
    directions = order_directions || Enum.map(order_by, fn _ -> :asc end)

    # Zip order_by and directions, pad directions with :asc if needed
    order_specs = Enum.zip_with([order_by, directions], fn [field, direction] -> {field, direction} end)

    Enum.sort(plots, fn plot1, plot2 ->
      compare_plots(plot1, plot2, order_specs)
    end)
  end

  defp compare_plots(_plot1, _plot2, []), do: true
  defp compare_plots(plot1, plot2, [{field, direction} | rest]) do
    val1 = get_field_value(plot1, field)
    val2 = get_field_value(plot2, field)

    case {val1, val2} do
      {same, same} -> compare_plots(plot1, plot2, rest)
      {nil, _} -> direction == :desc
      {_, nil} -> direction == :asc
      {v1, v2} when direction == :asc -> v1 <= v2
      {v1, v2} when direction == :desc -> v1 >= v2
    end
  end

  defp get_field_value(plot, field) do
    case field do
      :name -> plot.name
      :description -> plot.description
      :inserted_at -> plot.inserted_at
      :updated_at -> plot.updated_at
      :is_active -> plot.is_active
      :business_id -> plot.business_id
      _ -> nil
    end
  end

  defp apply_pagination(plots, flop) do
    total_count = length(plots)

    cond do
      # Page-based pagination
      flop.page && flop.page_size ->
        page = max(flop.page, 1)
        page_size = flop.page_size
        offset = (page - 1) * page_size

        paginated = plots |> Enum.drop(offset) |> Enum.take(page_size)
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

        paginated = plots |> Enum.drop(offset) |> Enum.take(limit)
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
        paginated = Enum.take(plots, limit)
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
        {plots, pagination_info}
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

  defp load_plots_simple(user, business_ids) do
    try do
      # Get plots for user's businesses
      case Plot.read(actor: user, filter: [business_id: [in: business_ids]]) do
        {:ok, plots} -> plots
        _ -> []
      end
    rescue
      _ -> []
    end
  end
end
