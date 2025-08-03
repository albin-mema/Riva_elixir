defmodule Storybook.Templates.ListViewTemplate do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Templates.ListViewTemplate.list_view_template/1

  def doc do
    """
    # ListViewTemplate

    Resource list page template with table and filters.

    ## Features

    - Page header with title and description
    - Filter panel integration
    - Data table with pagination
    - Empty state handling
    - Action buttons
    - Search functionality
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          title: "Reservations",
          description: "Manage all reservations",
          items: [
            %{id: 1, name: "Conference Room A", client: "Acme Corp", date: "2025-07-30", status: "Confirmed"},
            %{id: 2, name: "Auditorium", client: "Globex Inc", date: "2025-08-01", status: "Pending"},
            %{id: 3, name: "Meeting Room 3", client: "Wayne Enterprises", date: "2025-08-05", status: "Confirmed"}
          ],
          meta: %{page: 1, per_page: 10, total_pages: 1, total_count: 3},
          columns: [:name, :client, :date, :status],
          path: "/reservations",
          table_id: "reservations-table",
          filters: [
            %{type: "text", label: "Search", key: "search"},
            %{type: "select", label: "Status", key: "status", options: ["Confirmed", "Pending", "Cancelled"]},
            %{type: "date_range", label: "Date Range", key: "date_range"}
          ],
          filter_values: %{}
        },
        slots: [
          """
          <:actions>
            <.button variant="default">
              <.icon name={:plus} class="mr-2 h-4 w-4" />
              New Reservation
            </.button>
          </:actions>
          """,
          """
          <:col label="Resource" field={:name}>
            <.link navigate={"/reservations/1"} class="font-medium text-blue-600 hover:underline">
              Conference Room A
            </.link>
          </:col>
          """,
          """
          <:col label="Client" field={:client}>
            Acme Corp
          </:col>
          """,
          """
          <:col label="Date" field={:date}>
            Jul 30, 2025
          </:col>
          """,
          """
          <:col label="Status" field={:status}>
            <.badge variant="success">Confirmed</.badge>
          </:col>
          """
        ]
      },
      %Variation{
        id: :with_empty_state,
        attributes: %{
          title: "Reservations",
          description: "Manage all reservations",
          items: [],
          meta: %{page: 1, per_page: 10, total_pages: 1, total_count: 0},
          columns: [:name, :client, :date, :status],
          path: "/reservations",
          table_id: "reservations-table",
          empty_state: %{
            icon: :calendar,
            title: "No reservations found",
            description: "Get started by creating a new reservation"
          }
        },
        slots: [
          """
          <:actions>
            <.button variant="default">
              <.icon name={:plus} class="mr-2 h-4 w-4" />
              New Reservation
            </.button>
          </:actions>
          """,
          """
          <:col label="Resource" field={:name}></:col>
          """,
          """
          <:col label="Client" field={:client}></:col>
          """,
          """
          <:col label="Date" field={:date}></:col>
          """,
          """
          <:col label="Status" field={:status}></:col>
          """
        ]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :without_filters,
        description: "List view template without filters",
        attributes: %{
          title: "Simple List",
          description: "A list view without filters",
          items: [
            %{id: 1, name: "Item 1"},
            %{id: 2, name: "Item 2"}
          ],
          meta: %{page: 1, per_page: 10, total_pages: 1, total_count: 2},
          columns: [:name],
          path: "/items",
          table_id: "items-table",
          show_filters: false
        },
        template: """
        <.list_view_template
          title="Simple List"
          description="A list view without filters"
          items={[%{id: 1, name: "Item 1"}, %{id: 2, name: "Item 2"}]}
          meta={%{page: 1, per_page: 10, total_pages: 1, total_count: 2}}
          columns={[:name]}
          path="/items"
          table_id="items-table"
          show_filters={false}
        >
          <:actions>
            <.button variant="default">Add Item</.button>
          </:actions>
          <:col label="Name" field={:name}>
            <.link navigate={"/items/1"} class="font-medium text-blue-600 hover:underline">
              Item 1
            </.link>
          </:col>
        </.list_view_template>
        """
      },
      %Example{
        id: :with_custom_class,
        description: "List view template with custom CSS classes",
        attributes: %{
          title: "Styled List",
          items: [],
          meta: %{page: 1, per_page: 10, total_pages: 1, total_count: 0},
          columns: [:name],
          path: "/items",
          table_id: "items-table",
          class: "bg-gray-50 p-4"
        },
        template: """
        <.list_view_template
          title="Styled List"
          items={[]}
          meta={%{page: 1, per_page: 10, total_pages: 1, total_count: 0}}
          columns={[:name]}
          path="/items"
          table_id="items-table"
          class="bg-gray-50 p-4"
        >
          <:col label="Name" field={:name}></:col>
        </.list_view_template>
        """
      }
    ]
  end
end
