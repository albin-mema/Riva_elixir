defmodule Storybook.Templates.DashboardTemplate do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Templates.DashboardTemplate.dashboard_template/1

  def doc do
    """
    # DashboardTemplate

    Dashboard page template with grid layout.

    ## Features

    - Page header with quick actions
    - Stats section for key metrics
    - Grid layout with main content and sidebar
    - Responsive design
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          title: "Business Dashboard",
          description: "Overview of your business performance",
          stats: [
            %{label: "Total Reservations", value: "1,234", change: "+12%"},
            %{label: "Revenue", value: "$45,678", change: "+8%"},
            %{label: "Active Clients", value: "567", change: "+3%"}
          ],
          quick_actions: [
            %{label: "New Reservation", icon: :plus, action: "new_reservation"},
            %{label: "Add Client", icon: :user_plus, action: "add_client"},
            %{label: "Generate Report", icon: :file_text, action: "generate_report"}
          ]
        },
        slots: [
          """
          <:stats_section>
            <div class="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
              <.card>
                <:body>
                  <div class="flex items-center justify-between">
                    <div>
                      <p class="text-sm font-medium text-gray-500">Total Reservations</p>
                      <p class="text-2xl font-semibold">1,234</p>
                    </div>
                    <div class="p-3 bg-blue-100 rounded-full">
                      <.icon name={:calendar} class="h-6 w-6 text-blue-600" />
                    </div>
                  </div>
                  <p class="text-sm text-green-500 mt-1">+12% from last month</p>
                </:body>
              </.card>
              <.card>
                <:body>
                  <div class="flex items-center justify-between">
                    <div>
                      <p class="text-sm font-medium text-gray-500">Revenue</p>
                      <p class="text-2xl font-semibold">$45,678</p>
                    </div>
                    <div class="p-3 bg-green-100 rounded-full">
                      <.icon name={:dollar_sign} class="h-6 w-6 text-green-600" />
                    </div>
                  </div>
                  <p class="text-sm text-green-500 mt-1">+8% from last month</p>
                </:body>
              </.card>
              <.card>
                <:body>
                  <div class="flex items-center justify-between">
                    <div>
                      <p class="text-sm font-medium text-gray-500">Active Clients</p>
                      <p class="text-2xl font-semibold">567</p>
                    </div>
                    <div class="p-3 bg-purple-100 rounded-full">
                      <.icon name={:users} class="h-6 w-6 text-purple-600" />
                    </div>
                  </div>
                  <p class="text-sm text-red-500 mt-1">-2% from last month</p>
                </:body>
              </.card>
            </div>
          </:stats_section>
          """,
          """
          <:main_content>
            <.card>
              <:header>
                <h3 class="text-lg font-semibold">Recent Activity</h3>
              </:header>
              <:body>
                <div class="space-y-4">
                  <div class="flex items-start">
                    <div class="p-2 bg-blue-100 rounded-full mr-3">
                      <.icon name={:calendar} class="h-4 w-4 text-blue-600" />
                    </div>
                    <div>
                      <p class="font-medium">New reservation created</p>
                      <p class="text-sm text-gray-500">2 hours ago</p>
                    </div>
                  </div>
                  <div class="flex items-start">
                    <div class="p-2 bg-green-100 rounded-full mr-3">
                      <.icon name={:dollar_sign} class="h-4 w-4 text-green-600" />
                    </div>
                    <div>
                      <p class="font-medium">Payment received</p>
                      <p class="text-sm text-gray-500">5 hours ago</p>
                    </div>
                  </div>
                </div>
              </:body>
            </.card>
          </:main_content>
          """,
          """
          <:sidebar_content>
            <.card>
              <:header>
                <h3 class="text-lg font-semibold">Quick Actions</h3>
              </:header>
              <:body>
                <div class="grid grid-cols-2 gap-3">
                  <.button variant="outline" class="flex flex-col items-center justify-center h-20">
                    <.icon name={:plus} class="h-5 w-5 mb-1" />
                    <span>New Reservation</span>
                  </.button>
                  <.button variant="outline" class="flex flex-col items-center justify-center h-20">
                    <.icon name={:user_plus} class="h-5 w-5 mb-1" />
                    <span>Add Client</span>
                  </.button>
                </div>
              </:body>
            </.card>
          </:sidebar_content>
          """
        ]
      },
      %Variation{
        id: :minimal,
        attributes: %{
          title: "Simple Dashboard",
          description: "Basic dashboard layout"
        },
        slots: [
          """
          <:main_content>
            <.card>
              <:header>
                <h3 class="text-lg font-semibold">Content Area</h3>
              </:header>
              <:body>
                <p>This is the main content area of the dashboard.</p>
              </:body>
            </.card>
          </:main_content>
          """
        ]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_class,
        description: "Dashboard template with custom CSS classes",
        attributes: %{
          title: "Styled Dashboard",
          class: "bg-gray-50 p-4"
        },
        template: """
        <.dashboard_template title="Styled Dashboard" class="bg-gray-50 p-4">
          <:main_content>
            <div class="bg-white p-6 rounded-lg shadow">
              <h3 class="text-lg font-semibold mb-2">Custom Content</h3>
              <p>This dashboard has custom styling applied.</p>
            </div>
          </:main_content>
        </.dashboard_template>
        """
      }
    ]
  end
end
