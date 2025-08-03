defmodule Storybook.Templates.DetailViewTemplate do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Templates.DetailViewTemplate.detail_view_template/1

  def doc do
    """
    # DetailViewTemplate

    Resource detail page template with tabs and actions.

    ## Features

    - Page header with title, subtitle, and description
    - Action buttons slot
    - Header content area
    - Tab navigation
    - Tab content areas
    - Breadcrumb support
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          title: "Reservation Details",
          subtitle: "Reservation #12345",
          description: "View and manage reservation information",
          item: %{id: 12345, name: "Conference Room Booking"},
          tabs: [
            %{id: "details", label: "Details"},
            %{id: "history", label: "History"},
            %{id: "notes", label: "Notes"}
          ],
          active_tab: "details",
          breadcrumbs: [
            %{label: "Home", href: "/"},
            %{label: "Reservations", href: "/reservations"},
            %{label: "Details", current: true}
          ]
        },
        slots: [
          """
          <:actions>
            <.button variant="outline">
              <.icon name={:printer} class="mr-2 h-4 w-4" />
              Print
            </.button>
            <.button variant="default">
              <.icon name={:pencil} class="mr-2 h-4 w-4" />
              Edit
            </.button>
          </:actions>
          """,
          """
          <:header_content>
            <div class="grid grid-cols-2 gap-4">
              <div>
                <p class="text-sm text-gray-500">Status</p>
                <.badge variant="success">Confirmed</.badge>
              </div>
              <div>
                <p class="text-sm text-gray-500">Created</p>
                <p>July 25, 2025</p>
              </div>
            </div>
          </:header_content>
          """,
          """
          <:tab_content tab_id="details">
            <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
              <.card>
                <:header>
                  <h3 class="text-lg font-semibold">Reservation Information</h3>
                </:header>
                <:body>
                  <div class="space-y-4">
                    <div>
                      <p class="text-sm text-gray-500">Resource</p>
                      <p>Conference Room A</p>
                    </div>
                    <div>
                      <p class="text-sm text-gray-500">Date & Time</p>
                      <p>July 30, 2025, 10:00 AM - 12:00 PM</p>
                    </div>
                    <div>
                      <p class="text-sm text-gray-500">Attendees</p>
                      <p>15 people</p>
                    </div>
                  </div>
                </:body>
              </.card>
              <.card>
                <:header>
                  <h3 class="text-lg font-semibold">Client Information</h3>
                </:header>
                <:body>
                  <div class="space-y-4">
                    <div>
                      <p class="text-sm text-gray-500">Name</p>
                      <p>John Smith</p>
                    </div>
                    <div>
                      <p class="text-sm text-gray-500">Email</p>
                      <p>john@example.com</p>
                    </div>
                    <div>
                      <p class="text-sm text-gray-500">Phone</p>
                      <p>(555) 123-4567</p>
                    </div>
                  </div>
                </:body>
              </.card>
            </div>
          </:tab_content>
          """,
          """
          <:tab_content tab_id="history">
            <.card>
              <:body>
                <div class="space-y-4">
                  <div class="flex items-start">
                    <div class="p-2 bg-blue-100 rounded-full mr-3">
                      <.icon name={:calendar} class="h-4 w-4 text-blue-600" />
                    </div>
                    <div>
                      <p class="font-medium">Reservation created</p>
                      <p class="text-sm text-gray-500">July 25, 2025 by Admin User</p>
                    </div>
                  </div>
                  <div class="flex items-start">
                    <div class="p-2 bg-green-100 rounded-full mr-3">
                      <.icon name={:check_circle} class="h-4 w-4 text-green-600" />
                    </div>
                    <div>
                      <p class="font-medium">Reservation confirmed</p>
                      <p class="text-sm text-gray-500">July 26, 2025 by System</p>
                    </div>
                  </div>
                </div>
              </:body>
            </.card>
          </:tab_content>
          """,
          """
          <:tab_content tab_id="notes">
            <.card>
              <:body>
                <div class="space-y-4">
                  <div>
                    <p class="text-sm text-gray-500">Special Requirements</p>
                    <p>Need projector and whiteboard</p>
                  </div>
                  <div>
                    <p class="text-sm text-gray-500">Internal Notes</p>
                    <p>Client requested early setup</p>
                  </div>
                </div>
              </:body>
            </.card>
          </:tab_content>
          """
        ]
      },
      %Variation{
        id: :without_tabs,
        attributes: %{
          title: "Simple Detail View",
          item: %{id: 1, name: "Item Details"}
        },
        slots: [
          """
          <:main_content>
            <.card>
              <:body>
                <p>This detail view has no tabs.</p>
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
        description: "Detail view template with custom CSS classes",
        attributes: %{
          title: "Styled Detail View",
          item: %{id: 1, name: "Item"},
          class: "bg-gray-50 p-4"
        },
        template: """
        <.detail_view_template title="Styled Detail View" item={%{id: 1, name: "Item"}} class="bg-gray-50 p-4">
          <:tab_content tab_id="details">
            <div class="bg-white p-6 rounded-lg shadow">
              <p>Custom styled content</p>
            </div>
          </:tab_content>
        </.detail_view_template>
        """
      }
    ]
  end
end
