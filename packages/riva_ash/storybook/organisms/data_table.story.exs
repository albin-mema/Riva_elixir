defmodule Storybook.Organisms.DataTable do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.DataTable.data_table/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          items: [
            %{id: 1, name: "John Doe", email: "john@example.com", role: "Admin"},
            %{id: 2, name: "Jane Smith", email: "jane@example.com", role: "User"},
            %{id: 3, name: "Bob Johnson", email: "bob@example.com", role: "Manager"}
          ],
          meta: %{
            page: 1,
            per_page: 10,
            total_pages: 5,
            total_count: 42
          },
          path: "/users",
          id: "users-table",
          show_search: true,
          show_filters: true,
          show_pagination: true,
          selectable: false,
          actions: []
        },
        slots: [
          """
          <:col label="Name" field={:name} />
          <:col label="Email" field={:email} />
          <:col label="Role" field={:role} />
          """
        ]
      },
      %Variation{
        id: :with_actions,
        attributes: %{
          items: [
            %{id: 1, name: "John Doe", email: "john@example.com", role: "Admin"},
            %{id: 2, name: "Jane Smith", email: "jane@example.com", role: "User"}
          ],
          meta: %{
            page: 1,
            per_page: 10,
            total_pages: 1,
            total_count: 2
          },
          path: "/users",
          id: "users-table-actions",
          show_search: true,
          show_filters: true,
          show_pagination: true,
          selectable: false,
          actions: ["edit", "delete"]
        },
        slots: [
          """
          <:col label="Name" field={:name} />
          <:col label="Email" field={:email} />
          <:col label="Role" field={:role} />
          """
        ]
      },
      %Variation{
        id: :selectable,
        attributes: %{
          items: [
            %{id: 1, name: "John Doe", email: "john@example.com", role: "Admin"},
            %{id: 2, name: "Jane Smith", email: "jane@example.com", role: "User"},
            %{id: 3, name: "Bob Johnson", email: "bob@example.com", role: "Manager"}
          ],
          meta: %{
            page: 1,
            per_page: 10,
            total_pages: 5,
            total_count: 42
          },
          path: "/users",
          id: "users-table-selectable",
          show_search: true,
          show_filters: true,
          show_pagination: true,
          selectable: true,
          actions: []
        },
        slots: [
          """
          <:col label="Name" field={:name} />
          <:col label="Email" field={:email} />
          <:col label="Role" field={:role} />
          """
        ]
      }
    ]
  end
end
