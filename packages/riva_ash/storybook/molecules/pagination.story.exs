defmodule Storybook.Molecules.Pagination do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.Pagination.pagination/1

  def doc do
    """
    # Pagination

    Pagination component for table navigation.

    ## Features

    - Table navigation controls
    - Page size selection
    - Configurable page sizes
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          meta: %{page: 1, per_page: 10, total_count: 100, total_pages: 10},
          path: "/items"
        }
      },
      %Variation{
        id: :with_page_size_options,
        attributes: %{
          meta: %{page: 3, per_page: 20, total_count: 100, total_pages: 5},
          path: "/items",
          page_sizes: [10, 20, 50, 100]
        }
      },
      %Variation{
        id: :without_page_size,
        attributes: %{
          meta: %{page: 2, per_page: 10, total_count: 50, total_pages: 5},
          path: "/items",
          show_page_size: false
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Pagination with custom CSS classes",
        template: """
        <.pagination
          meta={%{page: 1, per_page: 10, total_count: 100, total_pages: 10}}
          path="/items"
          class="bg-gray-50 p-4 rounded-lg"
        />
        """
      }
    ]
  end
end
