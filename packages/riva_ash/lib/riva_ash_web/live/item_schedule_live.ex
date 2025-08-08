alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAsh.Resources, as: Resources
alias RivaAsh.Schedule, as: Schedule
alias RivaAsh.Live, as: Live
alias Ash.Error, as: Error

defmodule RivaAshWeb.ItemScheduleLive do
  @moduledoc """
  LiveView for managing Item Schedules.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.ItemSchedule
  alias RivaAsh.Schedule.ScheduleService
  alias RivaAsh.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_item_schedules_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}

          {:error, reason} ->
            Logger.error("Failed to load item schedules: #{inspect(reason)}")
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _unmatched} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Item Schedules" description="Manage item scheduling">
        <:action>
          <.button phx-click="new_item_schedule" class="bg-blue-600 hover:bg-blue-700">New Item Schedule</.button>
        </:action>
      </.page_header>

      <.data_table
        id="item-schedules-table"
        items={@item_schedules}
        meta={@meta}
        path="/item-schedules"
      >
        <:col :let={item_schedule} label="Item" field={:item_id} sortable>
          <%= if item_schedule.item do %>
            <%= item_schedule.item.name %>
          <% else %>
            <span class="text-gray-500">Unknown Item</span>
          <% end %>
        </:col>
        <:col :let={item_schedule} label="Start Time">
          <%= format_datetime(item_schedule.start_time) %>
        </:col>
        <:col :let={item_schedule} label="End Time">
          <%= format_datetime(item_schedule.end_time) %>
        </:col>
        <:col :let={item_schedule} label="Day of Week">
          <%= format_day_of_week(item_schedule.day_of_week) %>
        </:col>
        <:col :let={item_schedule} label="Recurring">
          <%= if item_schedule.recurring do %>
            <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
              Yes
            </span>
          <% else %>
            <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
              No
            </span>
          <% end %>
        </:col>
        <:col :let={item_schedule} label="Status">
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case item_schedule.status do
              :active -> "bg-green-100 text-green-800"
              :inactive -> "bg-gray-100 text-gray-800"
              :expired -> "bg-red-100 text-red-800"
              _unmatchedunmatched -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(item_schedule.status)) %>
          </span>
        </:col>
        <:col :let={item_schedule} label="Actions">
          <%= if item_schedule.status == :active do %>
            <.button phx-click="edit_item_schedule" phx-value-id={item_schedule.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
            <.button phx-click="deactivate_item_schedule" phx-value-id={item_schedule.id} class="bg-yellow-600 hover:bg-yellow-700">Deactivate</.button>
          <% else %>
            <.button phx-click="activate_item_schedule" phx-value-id={item_schedule.id} class="bg-green-600 hover:bg-green-700">Activate</.button>
            <.button phx-click="view_item_schedule" phx-value-id={item_schedule.id} variant="secondary">View</.button>
          <% end %>
          <.button phx-click="delete_item_schedule" phx-value-id={item_schedule.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_item_schedule", _params, socket) do
    {:noreply, push_patch(socket, to: "/item-schedules/new")}
  end

  def handle_event("edit_item_schedule", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-schedules/#{id}/edit")}
  end

  def handle_event("view_item_schedule", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/item-schedules/#{id}")}
  end

  def handle_event("activate_item_schedule", %{"id" => id}, socket) do
    case ScheduleService.activate_schedule(id, socket.assigns.current_user) do
      {:ok, _item_schedule} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item schedule activated successfully")
         |> reload_item_schedules()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to activate item schedule: #{format_error(reason)}")}
    end
  end

  def handle_event("deactivate_item_schedule", %{"id" => id}, socket) do
    case ScheduleService.deactivate_schedule(id, socket.assigns.current_user) do
      {:ok, _item_schedule} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item schedule deactivated successfully")
         |> reload_item_schedules()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to deactivate item schedule: #{format_error(reason)}")}
    end
  end

  def handle_event("delete_item_schedule", %{"id" => id}, socket) do
    case ScheduleService.delete_schedule(id, socket.assigns.current_user) do
      {:ok, _item_schedule} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item schedule deleted successfully")
         |> reload_item_schedules()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete item schedule: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_item_schedules_data(socket, user) do
    case ScheduleService.get_user_schedules(user) do
      {:ok, {item_schedules, meta}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:item_schedules, item_schedules)
          |> assign(:meta, meta)
          |> assign(:loading, false)

        {:ok, socket}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_item_schedules(socket) do
    case ScheduleService.get_user_schedules(socket.assigns.current_user) do
      {:ok, {item_schedules, meta}} ->
        assign(socket, :item_schedules, item_schedules)
        |> assign(:meta, meta)

      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :item_schedules_page_title, "Item Schedules")
  end

  defp format_datetime(nil), do: "N/A"

  defp format_datetime(datetime) do
    case DateTime.from_naive(datetime, "Etc/UTC") do
      {:ok, datetime} ->
        Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")

      {:error, _unmatched} ->
        "Invalid date"
    end
  end

  defp format_day_of_week(nil), do: "N/A"

  defp format_day_of_week(day) when is_integer(day) do
    case day do
      0 -> "Sunday"
      1 -> "Monday"
      2 -> "Tuesday"
      3 -> "Wednesday"
      4 -> "Thursday"
      5 -> "Friday"
      6 -> "Saturday"
      _unmatchedunmatched -> "Unknown"
    end
  end

  defp format_day_of_week(day) when is_binary(day), do: day

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} ->
        Enum.map_join(errors, ", ", &format_validation_error/1)

      %Ash.Error.Forbidden{} ->
        "You don't have permission to perform this action"

      %Ash.Error.NotFound{} ->
        "Item schedule not found"

      _unmatchedunmatched ->
        "An unexpected error occurred"
    end
  end

  defp format_validation_error(error) do
    case error do
      {message, _unmatched} -> message
      message when is_binary(message) -> message
      _unmatchedunmatched -> "Invalid input"
    end
  end
end
