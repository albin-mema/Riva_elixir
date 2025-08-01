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

  def notification_center(assigns) do
    ~H"""
    <!-- Notification center implementation will go here -->
    <div {@rest} class={["notification-center", @class]}>
      <.button
        variant="ghost"
        size="sm"
        phx-click={@on_toggle}
        class="notification-trigger"
      >
        <.icon name={:bell} />
        <.badge :if={@unread_count > 0} variant="destructive" size="sm">
          <%= @unread_count %>
        </.badge>
      </.button>
      
      <div :if={@show_panel} class="notification-panel">
        <div class="panel-header">
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
        
        <div :if={@notifications != []} class="notifications-list">
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
