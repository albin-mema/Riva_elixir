defmodule RivaAshWeb.Storybook.Combinations.DashboardHeader do
  use PhoenixStorybook.Story, :component
  use RivaAshWeb.Components.AtomicComponents

  def function, do: &dashboard_header/1

  def dashboard_header(assigns) do
    ~H"""
    <div class="flex items-center justify-between p-4 bg-background border-b border-border">
      <.search_bar placeholder="Search reservations..." />

      <div class="flex items-center space-x-4">
        <.notification_toast count={@user.notification_count}>
          <.icon name={:bell} />
        </.notification_toast>

        <.avatar
          name={@user.name}
          size="md"
          variant="circle"
        />
      </div>
    </div>
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          user: %{
            name: "Sarah Johnson",
            role: "Manager",
            notification_count: 3
          }
        }
      },
      %Variation{
        id: :with_active_notifications,
        attributes: %{
          user: %{
            name: "Alex Chen",
            role: "Admin",
            notification_count: 12
          }
        }
      }
    ]
  end
end
