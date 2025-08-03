defmodule Storybook.Molecules.SearchBar do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.SearchBar.search_bar/1

  def doc do
    """
    # SearchBar

    Search bar component with filters and suggestions.

    ## Features

    - Search input with optional filters
    - Loading state
    - Suggestions support
    - Custom placeholder
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          placeholder: "Search...",
          on_search: "perform_search"
        }
      },
      %Variation{
        id: :with_value,
        attributes: %{
          value: "search term",
          placeholder: "Search...",
          on_search: "perform_search"
        }
      },
      %Variation{
        id: :with_filters,
        attributes: %{
          placeholder: "Search...",
          show_filters: true,
          filters: [
            %{label: "Category", key: "category", options: ["A", "B", "C"]},
            %{label: "Status", key: "status", options: ["Active", "Inactive"]}
          ],
          on_search: "perform_search"
        }
      },
      %Variation{
        id: :loading,
        attributes: %{
          placeholder: "Searching...",
          loading: true,
          on_search: "perform_search"
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_suggestions,
        description: "Search bar with suggestions",
        template: """
        <.search_bar
          placeholder="Search for items..."
          suggestions={["Apple", "Banana", "Cherry", "Date"]}
          on_search="search_items"
        />
        """
      },
      %Example{
        id: :with_custom_styling,
        description: "Search bar with custom CSS classes",
        template: """
        <.search_bar
          placeholder="Custom styled search..."
          on_search="custom_search"
          class="bg-blue-50 border-blue-200"
        />
        """
      }
    ]
  end
end
