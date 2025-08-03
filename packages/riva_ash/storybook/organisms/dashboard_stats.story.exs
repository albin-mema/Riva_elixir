defmodule Storybook.Organisms.DashboardStats do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.DashboardStats.dashboard_stats/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          stats: [
            %{icon: :users, value: "1,234", label: "Total Clients", change: "+12%"},
            %{icon: :calendar, value: "56", label: "Upcoming Reservations", change: "-3%"},
            %{icon: :currency_dollar, value: "$12,345", label: "Revenue", change: "+8%"},
            %{icon: :building_office, value: "24", label: "Active Businesses", change: "+2%"}
          ],
          loading: false
        }
      },
      %Variation{
        id: :loading,
        attributes: %{
          stats: [
            %{icon: :users, value: "1,234", label: "Total Clients", change: "+12%"},
            %{icon: :calendar, value: "56", label: "Upcoming Reservations", change: "-3%"},
            %{icon: :currency_dollar, value: "$12,345", label: "Revenue", change: "+8%"},
            %{icon: :building_office, value: "24", label: "Active Businesses", change: "+2%"}
          ],
          loading: true
        }
      },
      %Variation{
        id: :with_custom_class,
        attributes: %{
          stats: [
            %{icon: :users, value: "1,234", label: "Total Clients", change: "+12%"},
            %{icon: :calendar, value: "56", label: "Upcoming Reservations", change: "-3%"},
            %{icon: :currency_dollar, value: "$12,345", label: "Revenue", change: "+8%"},
            %{icon: :building_office, value: "24", label: "Active Businesses", change: "+2%"}
          ],
          loading: false,
          class: "bg-gray-100 p-4 rounded-lg"
        }
      }
    ]
  end
end
