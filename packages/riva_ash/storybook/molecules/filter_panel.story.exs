defmodule Storybook.Molecules.FilterPanel do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.FilterPanel.filter_panel/1

  def doc do
    """
    # FilterPanel

    Advanced filtering interface for data tables.

    ## Features

    - Advanced filtering interface
    - Configurable filters
    - Collapsible panel
    - Apply and clear actions
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          filters: [
            %{type: "text", label: "Name", key: "name"},
            %{type: "select", label: "Status", key: "status", options: ["Active", "Inactive", "Pending"]},
            %{type: "date", label: "Created Date", key: "created_date"}
          ],
          values: %{},
          on_apply: "apply_filters",
          on_clear: "clear_filters"
        }
      },
      %Variation{
        id: :with_values,
        attributes: %{
          filters: [
            %{type: "text", label: "Name", key: "name"},
            %{type: "select", label: "Status", key: "status", options: ["Active", "Inactive", "Pending"]},
            %{type: "date", label: "Created Date", key: "created_date"}
          ],
          values: %{
            "name" => "John",
            "status" => "Active"
          },
          on_apply: "apply_filters",
          on_clear: "clear_filters"
        }
      },
      %Variation{
        id: :non_collapsible,
        attributes: %{
          filters: [
            %{type: "text", label: "Search", key: "search"},
            %{type: "select", label: "Category", key: "category", options: ["A", "B", "C"]}
          ],
          values: %{},
          on_apply: "apply_filters",
          on_clear: "clear_filters",
          collapsible: false
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Filter panel with custom CSS classes",
        template: """
        <.filter_panel
          filters={[
            %{type: "text", label: "Name", key: "name"},
            %{type: "select", label: "Status", key: "status", options: ["Active", "Inactive"]}
          ]}
          values={%{}}
          on_apply="apply_filters"
          on_clear="clear_filters"
          class="bg-gray-50 p-4 rounded-lg"
        />
        """
      }
    ]
  end
end
