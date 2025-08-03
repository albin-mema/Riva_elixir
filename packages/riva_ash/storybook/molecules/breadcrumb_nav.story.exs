defmodule Storybook.Molecules.BreadcrumbNav do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.BreadcrumbNav.breadcrumb_nav/1

  def doc do
    """
    # BreadcrumbNav

    Breadcrumb navigation component.

    ## Features

    - Navigation trail showing user's location in the app
    - Configurable separator icon
    - Option to show/hide home link
    - Custom home path
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          items: [
            %{label: "Dashboard", href: "/dashboard"},
            %{label: "Products", href: "/products"},
            %{label: "Product Details", current: true}
          ]
        }
      },
      %Variation{
        id: :with_custom_separator,
        attributes: %{
          items: [
            %{label: "Home", href: "/"},
            %{label: "Library", href: "/library"},
            %{label: "Data", current: true}
          ],
          separator: :chevron_double_right
        }
      },
      %Variation{
        id: :without_home,
        attributes: %{
          items: [
            %{label: "Products", href: "/products"},
            %{label: "Categories", href: "/categories"},
            %{label: "Electronics", current: true}
          ],
          show_home: false
        }
      },
      %Variation{
        id: :with_custom_home_path,
        attributes: %{
          items: [
            %{label: "Projects", href: "/projects"},
            %{label: "Project Alpha", current: true}
          ],
          home_path: "/dashboard"
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Breadcrumb navigation with custom CSS classes",
        template: """
        <.breadcrumb_nav
          items={[
            %{label: "Dashboard", href: "/dashboard"},
            %{label: "Users", href: "/users"},
            %{label: "User Profile", current: true}
          ]}
          class="bg-gray-100 p-4 rounded-lg"
        />
        """
      }
    ]
  end
end
