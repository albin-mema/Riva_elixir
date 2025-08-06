defmodule RivaAshWeb.Components.Navigation.NotificationCenter do
  @moduledoc """
  Notification management center component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Icon
  import RivaAshWeb.Components.Atoms.Badge

  @doc """
  Renders a notification center.
  """
  attr(:notifications, :list, default: [])
  attr(:unread_count, :integer, default: 0)
  attr(:show_panel, :boolean, default: false)
  attr(:on_toggle, :string, required: true)
  attr(:on_mark_read, :string, required: true)
  attr(:on_mark_all_read, :string, required: true)
  attr(:on_notification_click, :string, default: nil)
  attr(:max_display, :integer, default: 10)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec notification_center(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def notification_center(assigns) do
    # Render notification center using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:trigger_class, build_trigger_class(assigns.unread_count))
    |> Map.put_new(:panel_class, build_panel_class(assigns.show_panel))
    |> Map.put_new(:header_class, build_header_class(assigns.unread_count))
    |> Map.put_new(:list_class, build_list_class(assigns.notifications))
    |> render_notification_center_component()
  end

  # Private helper for notification center rendering
  @spec render_notification_center_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_notification_center_component(assigns) do
    ~H"""
    <!-- Notification center implementation will go here -->
    <div {@rest} class={@container_class}>
      <.button
        variant="ghost"
        size="sm"
        phx-click={@on_toggle}
        class={@trigger_class}
      >
        <.icon name={:bell} />
        <.badge :if={@unread_count > 0} variant="destructive" size="sm">
          <%= @unread_count %>
        </.badge>
      </.button>

      <div :if={@show_panel} class={@panel_class}>
        <div class={@header_class}>
          <h3>Notifications</h3>
          <.button
            :if={@unread_count > 0}
            variant="ghost"
            size="sm"
            phx-click={@on_mark_all_read}
          >
            Mark all read
          </.button>
        </div>

        <div :if={@notifications == []} class="empty-notifications">
          <.icon name={:bell_slash} />
          <p>No notifications</p>
        </div>

        <div :if={@notifications != []} class={@list_class}>
          <div
            :for={notification <- Enum.take(@notifications, @max_display)}
            class={[
              "notification-item",
              if(!notification.read, do: "unread", else: "read")
            ]}
          >
            <div class="notification-icon">
              <.icon name={get_notification_icon(notification.type)} />
            </div>

            <div
              class="notification-content"
              phx-click={@on_notification_click}
              phx-value-id={notification.id}
            >
              <div class="notification-title">
                <%= notification.title %>
              </div>
              <div class="notification-message">
                <%= notification.message %>
              </div>
              <div class="notification-time">
                <%= format_time_ago(notification.inserted_at) %>
              </div>
            </div>

            <div class="notification-actions">
              <.button
                :if={!notification.read}
                variant="ghost"
                size="xs"
                phx-click={@on_mark_read}
                phx-value-id={notification.id}
              >
                <.icon name={:check} />
              </.button>
            </div>
          </div>
        </div>

        <div :if={length(@notifications) > @max_display} class="panel-footer">
          <a href="/notifications" class="view-all-link">
            View all notifications
          </a>
        </div>
      </div>
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    ["notification-center", class]
    |> Enum.filter(&(&1 != ""))
    |> Enum.join(" ")
  end

  # Helper function to build trigger classes
  @spec build_trigger_class(integer()) :: String.t()
  defp build_trigger_class(unread_count) do
    "notification-trigger"
  end

  # Helper function to build panel classes
  @spec build_panel_class(boolean()) :: String.t()
  defp build_panel_class(show_panel), do: if show_panel, do: "notification-panel", else: "hidden"

  # Helper function to build header classes
  @spec build_header_class(integer()) :: String.t()
  defp build_header_class(unread_count) do
    "panel-header"
  end

  # Helper function to build list classes
  @spec build_list_class(list()) :: String.t()
  defp build_list_class(notifications) do
    "notifications-list"
  end

  defp get_notification_icon(type) do
    case type do
      "reservation_created" -> :calendar_days
      "payment_received" -> :credit_card
      "client_registered" -> :user_plus
      "system_alert" -> :exclamation_triangle
      "reminder" -> :clock
      _ -> :bell
    end
  end

  defp format_time_ago(datetime) do
    # This would be implemented with a proper time formatting library
    "#{datetime} ago"
  end
end
