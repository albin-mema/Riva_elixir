defmodule RivaAshWeb.ReservationLive do
  @moduledoc """
  Reservation management LiveView with calendar integration.
  """
  use RivaAshWeb, :live_view
  import OK, only: [success: 1, failure: 1, ~>>: 2, for: 1, required: 2]

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Organisms.ReservationForm
  import RivaAshWeb.Components.Molecules.TabNavigation

  alias RivaAsh.Resources.Reservation

  @impl true
  def mount(_params, session, socket) do
    get_current_user_from_session(session)
    |> OK.required(:user_not_authenticated)
    ~>> fn user ->
      socket
      |> assign(:current_user, user)
      |> assign(:page_title, "Reservation Management")
      |> assign(:reservations, [])
      |> assign(:meta, %{})
      |> assign(:active_tab, "list")
      |> assign(:show_form, false)
      |> assign(:editing_reservation, nil)
      |> assign(:form, nil)
      |> assign(:clients, [])
      |> assign(:items, [])
      |> assign(:employees, [])
    end
    |> case do
      {:ok, socket} -> success(socket)
      {:error, :user_not_authenticated} -> success(redirect(socket, to: "/sign-in"))
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!-- Reservation management implementation will go here -->
    <div>
      <.page_header title="Reservation Management" description="Manage bookings and reservations">
        <:action>
          <button phx-click="new_reservation">New Reservation</button>
        </:action>
      </.page_header>

      <.tab_navigation
        tabs={[
          %{id: "list", label: "List View"},
          %{id: "calendar", label: "Calendar View"},
          %{id: "timeline", label: "Timeline View"}
        ]}
        active_tab={@active_tab}
        on_tab_change="change_view"
      />

      <div :if={@show_form}>
        <.reservation_form
          form={@form}
          clients={@clients}
          items={@items}
          employees={@employees}
          on_submit="save_reservation"
          on_change="validate_reservation"
          on_cancel="cancel_form"
        />
      </div>

      <div :if={@active_tab == "list" && !@show_form}>
        <.data_table
          items={@reservations}
          meta={@meta}
          path="/reservations"
          id="reservations-table"
        >
          <:col :let={item} label="Client" field={:client} sortable>
            <%= item.client.first_name %> <%= item.client.last_name %>
          </:col>
          <:col :let={item} label="Item" field={:item} sortable>
            <%= item.item.name %>
          </:col>
          <:col :let={item} label="Date" field={:reserved_from} sortable>
            <%= Calendar.strftime(item.reserved_from, "%Y-%m-%d") %>
          </:col>
          <:col :let={item} label="Time">
            <%= Calendar.strftime(item.reserved_from, "%H:%M") %> -
            <%= Calendar.strftime(item.reserved_until, "%H:%M") %>
          </:col>
          <:col :let={item} label="Status" field={:status}>
            <%= item.status %>
          </:col>
          <:col :let={item} label="Actions">
            <button phx-click="edit_reservation" phx-value-id={item.id}>Edit</button>
            <button phx-click="cancel_reservation" phx-value-id={item.id}>Cancel</button>
          </:col>
        </.data_table>
      </div>

      <div :if={@active_tab == "calendar" && !@show_form}>
        <!-- Calendar view will go here -->
        <p>Calendar view coming soon...</p>
      </div>

      <div :if={@active_tab == "timeline" && !@show_form}>
        <!-- Timeline view will go here -->
        <p>Timeline view coming soon...</p>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("change_view", %{"tab" => tab}, socket) do
    socket
    |> assign(:active_tab, tab)
    |> then(&{:noreply, &1})
  end

  def handle_event("new_reservation", _params, socket) do
    socket
    |> assign(:show_form, true)
    |> then(&{:noreply, &1})
  end

  def handle_event("edit_reservation", %{"id" => id}, socket) do
    OK.for do
      reservation <- Reservation.by_id(id)
      form <- OK.wrap(AshPhoenix.Form.for_update(reservation, :update, actor: socket.assigns.current_user))
    after
      socket
      |> assign(:editing_reservation, reservation)
      |> assign(:form, form |> to_form())
      |> assign(:show_form, true)
    else
      _ ->
        socket
        |> put_flash(:error, "Failed to load reservation for editing")
    end
    |> then(&{:noreply, &1})
  end

  def handle_event("cancel_reservation", %{"id" => id}, socket) do
    Reservation.by_id(id)
    ~>> fn reservation ->
      Reservation.update(reservation, %{status: :cancelled}, actor: socket.assigns.current_user)
    end
    |> case do
      {:ok, _} ->
        socket
        |> put_flash(:info, "Reservation cancelled successfully")
        |> assign(:reservations, load_reservations(socket.assigns.current_user))
      {:error, error} ->
        socket
        |> put_flash(:error, "Failed to cancel reservation: #{inspect(error)}")
    end
    |> then(&{:noreply, &1})
  end

  def handle_event("save_reservation", %{"form" => params}, socket) do
    AshPhoenix.Form.submit(socket.assigns.form, params: params, actor: socket.assigns.current_user)
    ~>> fn reservation ->
      socket
      |> assign(:reservations, load_reservations(socket.assigns.current_user))
      |> assign(:show_form, false)
      |> assign(:editing_reservation, nil)
      |> assign(:form, nil)
      |> put_flash(:info, "Reservation saved successfully")
    end
    |> case do
      {:ok, socket} -> {:noreply, socket}
      {:error, form} ->
        socket =
          socket
          |> assign(:form, form |> to_form())
          |> put_flash(:error, "Please fix the errors below")
        {:noreply, socket}
    end
  end

  def handle_event("validate_reservation", %{"form" => params}, socket) do
    form = AshPhoenix.Form.validate(socket.assigns.form, params) |> to_form()
    socket = assign(socket, :form, form)
    {:noreply, socket}
  end

  def handle_event("cancel_form", _params, socket) do
    socket
    |> assign(:show_form, false)
    |> assign(:editing_reservation, nil)
    |> assign(:form, nil)
    |> then(&{:noreply, &1})
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  defp get_current_user_from_session(session) do
    # Implementation will be added when auth system is integrated
    case Map.get(session, "user_token") do
      nil -> nil
      _token -> %{id: "user-1", role: :user} # Mock user for now
    end
  end

  defp load_reservations(user) do
    Reservation
    |> Ash.Query.load([:client, :item, :employee])
    |> Ash.read(actor: user)
    |> case do
      {:ok, reservations} -> reservations
      {:error, _} -> []
    end
  end
end
