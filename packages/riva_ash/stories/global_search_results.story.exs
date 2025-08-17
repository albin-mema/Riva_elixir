defmodule RivaAsh.Stories.GlobalSearchResultsStories do
  use Phoenix.Component
  import RivaAsh.Components.UI.GlobalSearchResults
  import RivaAsh.Components.UI.CommandPalette

  # Sample data for stories
  defp sample_results do
    [
      %{id: 1, title: "Dashboard Overview", type: "Page", relevance: 95},
      %{id: 2, title: "User Management", type: "Page", relevance: 85},
      %{id: 3, title: "Reservation System", type: "Feature", relevance: 75},
      %{id: 4, title: "Billing Settings", type: "Page", relevance: 65},
      %{id: 5, title: "API Documentation", type: "Resource", relevance: 55}
    ]
  end

  def desktop_layout(assigns) do
    ~H"""
    <div class="bg-gray-50 dark:bg-gray-900 p-8 min-h-screen">
      <.global_search_results
        results={sample_results()}
        selected_index={1}
        query="user"
        on_select={JS.push("select_result")}
      />
    </div>
    """
  end

  def mobile_layout(assigns) do
    ~H"""
    <div class="bg-gray-50 dark:bg-gray-900 p-4 min-h-screen" style="max-width: 375px">
      <.global_search_results
        results={sample_results()}
        selected_index={2}
        query="res"
        on_select={JS.push("select_result")}
      />
    </div>
    """
  end

  def keyboard_navigation(assigns) do
    ~H"""
    <div class="bg-gray-50 dark:bg-gray-900 p-8 min-h-screen">
      <.global_search_results
        results={sample_results()}
        selected_index={3}
        query="bill"
        on_select={JS.push("select_result")}
      />
    </div>
    """
  end

  def command_palette_integration(assigns) do
    ~H"""
    <div class="bg-gray-50 dark:bg-gray-900 p-8 min-h-screen">
      <.command_palette open={true} search_query="user">
        <:header>
          <.search_bar value="user" />
        </:header>
        <:content>
          <.global_search_results
            results={sample_results()}
            selected_index={0}
            query="user"
            on_select={JS.push("select_result")}
          />
        </:content>
      </.command_palette>
    </div>
    """
  end

  def loading_state(assigns) do
    ~H"""
    <div class="bg-gray-50 dark:bg-gray-900 p-8 min-h-screen">
      <.global_search_results
        loading={true}
        results={[]}
        query="test"
        on_select={JS.push("select_result")}
      />
    </div>
    """
  end

  def empty_state(assigns) do
    ~H"""
    <div class="bg-gray-50 dark:bg-gray-900 p-8 min-h-screen">
      <.global_search_results
        results={[]}
        query="nonexistent"
        on_select={JS.push("select_result")}
      />
    </div>
    """
  end

  def error_state(assigns) do
    ~H"""
    <div class="bg-gray-50 dark:bg-gray-900 p-8 min-h-screen">
      <.global_search_results
        results={[]}
        error="Failed to fetch results"
        query="error"
        on_select={JS.push("select_result")}
      />
    </div>
    """
  end
end
