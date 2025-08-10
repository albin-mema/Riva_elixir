defmodule RivaAsh.Components.UI.GlobalSearchResults do
  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms
  import RivaAsh.Components.UI.Kbd

  def global_search_results(assigns) do
    ~H"""
    <div class="relative mx-auto w-full max-w-md" role="listbox" aria-label="Search results" tabindex="0" phx-keydown.arrow-up="navigate_up" phx-keydown.arrow-down="navigate_down" phx-keydown.escape="close_results" aria-live="polite">
      <div class="sr-only" id="search-status">
        <%= if @results == [] do %>
          No results found
        <% else %>
          <%= length(@results) %> results found
        <% end %>
      </div>
      <div class="z-10 absolute bg-white dark:bg-gray-800 shadow-lg mt-2 rounded-lg w-full overflow-hidden"
      <div class="z-10 absolute bg-white dark:bg-gray-800 shadow-lg mt-2 rounded-lg w-full overflow-hidden"
           style={["--spacing": "0.5rem", "--radius": "0.375rem", "--shadow": "0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)"]}>
        <%= if @loading do %>
          <.skeleton_list items={5} class="divide-y divide-gray-100 dark:divide-gray-700" />
        <% else %>
          <%= if @results != [] do %>
          <ul class="divide-y divide-gray-100 dark:divide-gray-700">
            <%= for {result, index} <- Enum.with_index(@results) do %>
              <li class={["flex flex-col items-center justify-between cursor-pointer py-3 px-4 hover:bg-gray-50 dark:hover:bg-gray-700 min-h-11 md:flex-row",
                          "bg-blue-50 dark:bg-blue-900" if index == @selected_index]}
                  role="option"
                  aria-selected={index == @selected_index}
                  tabindex={if index == @selected_index, do: "0", else: "-1"}
                  phx-click={@on_select}
                  phx-keydown.enter={@on_select}
                  phx-value-index={index}>
                <div class="flex items-center gap-3">
                  <.icon name="search" size="sm" class="text-gray-500" />
                  <span class="font-medium text-sm">
                    <%= for {type, part} <- highlight(result.title, @query) do %>
                      <span class={if type == :match, do: "font-bold text-primary", else: ""}><%= part %></span>
                    <% end %>
                  </span>
                </div>
                <div class="flex items-center gap-2">
                  <%= if result.type do %>
                    <span class="bg-gray-100 dark:bg-gray-700 px-2 py-0.5 rounded-full text-xs">
                      <%= result.type %>
                    </span>
                  <% end %>
                  <% end %>
                  <span class="text-gray-500 dark:text-gray-400 text-xs">
                    <%= result.relevance %>% match
                  </span>
                </div>
              </li>
            <% end %>
          </ul>
        <% else %>
          <div class="px-4 py-8 text-gray-500 dark:text-gray-400 text-center">
            No results found
          </div>
        <% end %>
      </div>
    </div>
    """
  end
end
