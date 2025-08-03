defmodule Storybook.Molecules.TabNavigation do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.TabNavigation.tab_navigation/1

  def doc do
    """
    # TabNavigation

    Tab navigation component for switching between views.

    ## Features

    - Tab navigation interface
    - Active tab highlighting
    - Different variants (default, pills, underline)
    - Different sizes (sm, md, lg)
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          tabs: [
            %{id: "tab1", label: "Tab 1"},
            %{id: "tab2", label: "Tab 2"},
            %{id: "tab3", label: "Tab 3"}
          ],
          active_tab: "tab1",
          on_tab_change: "change_tab"
        }
      },
      %Variation{
        id: :pills,
        attributes: %{
          tabs: [
            %{id: "home", label: "Home"},
            %{id: "profile", label: "Profile"},
            %{id: "settings", label: "Settings"}
          ],
          active_tab: "home",
          on_tab_change: "change_tab",
          variant: "pills"
        }
      },
      %Variation{
        id: :underline,
        attributes: %{
          tabs: [
            %{id: "overview", label: "Overview"},
            %{id: "analytics", label: "Analytics"},
            %{id: "reports", label: "Reports"}
          ],
          active_tab: "overview",
          on_tab_change: "change_tab",
          variant: "underline"
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :different_sizes,
        description: "Tab navigation in different sizes",
        template: """
        <div class="space-y-6">
          <.tab_navigation
            tabs={[
              %{id: "small", label: "Small"},
              %{id: "tab", label: "Tab"}
            ]}
            active_tab="small"
            on_tab_change="change_tab"
            size="sm"
          />
          <.tab_navigation
            tabs={[
              %{id: "medium", label: "Medium"},
              %{id: "tab", label: "Tab"}
            ]}
            active_tab="medium"
            on_tab_change="change_tab"
            size="md"
          />
          <.tab_navigation
            tabs={[
              %{id: "large", label: "Large"},
              %{id: "tab", label: "Tab"}
            ]}
            active_tab="large"
            on_tab_change="change_tab"
            size="lg"
          />
        </div>
        """
      },
      %Example{
        id: :with_custom_styling,
        description: "Tab navigation with custom CSS classes",
        template: """
        <.tab_navigation
          tabs={[
            %{id: "custom", label: "Custom"},
            %{id: "tab", label: "Tab"}
          ]}
          active_tab="custom"
          on_tab_change="change_tab"
          class="bg-blue-50 p-2 rounded-lg"
        />
        """
      }
    ]
  end
end
