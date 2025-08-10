defmodule RivaAshWeb.Components.UI.InboxList do
  use Phoenix.Component
  import RivaAshWeb.Components.UI.{StatusIndicator, Pagination, Skeleton}

  @doc """
  InboxList organism component displaying notifications and tasks with proper accessibility and responsive behavior.

  ## Composition
  Follows atomic design composition pattern using:
  - List container (molecule)
  - StatusIndicator (molecule)
  - Pagination (molecule)
  - Skeleton (atom) for empty states

  ## Attributes
    * `items` - List of inbox items with structure:
      [
        %{
          id: String.t(),
          type: :notification | :task,
          content: String.t(),
          status: :info | :warning | :success,
          status_label: String.t(),
          actions: list(Action)
        }
      ]
    * `pagination` - Pagination configuration map
    * `empty_state` - Configuration for empty state display
    * `id` - Unique identifier for the component instance
  """
  def inbox_list(assigns) do
    assigns = assign_new(assigns, :items, fn -> [] end)
    assigns = assign_new(assigns, :empty_state_type, fn -> :empty end)
    assigns = assign_new(assigns, :pagination, fn -> %{} end)

    ~H"""
    <div
      role="list"
      aria-live="polite"
      class="w-full inbox-list"
      phx-hook="InboxList"
      id={"inbox-list-#{@id}"}
      data-testid="inbox-list"
    >
      <%= if @items == [] do %>
        <.empty_state {@empty_state} type={@empty_state_type} />
      <% else %>
        <%= for item <- @items do %>
          <div
            role="listitem"
            class={[
              "inbox-item border-b last:border-b-0 transition-colors",
              "flex flex-col md:flex-row md:items-center",
              "hover:bg-gray-50",
              "min-h-[44px]": @mobile_touch_targets,
              "touch-target": item.type == :task
            ]}
            id={"inbox-item-#{item.id}"}
            tabindex="0"
            aria-label={"#{item.content}. #{item.status_label}"}
            data-testid={"inbox-item-#{item.id}"}
          >
            <div class="flex-1 py-3 md:py-2 min-w-0">
              <div class="flex items-center">
                <.StatusIndicator.status_indicator variant={item.status} class="mr-2" aria-label={item.status_label} />
                <div class="font-medium truncate">{item.content}</div>
              </div>
              <div class="flex items-center mt-1 md:mt-0 text-gray-500 text-sm">
                <%= if item.type == :notification do %>
                  <.tag variant="info" class="mr-2">Notification</.tag>
                <% else %>
                  <.tag variant="success" class="mr-2">Task</.tag>
                <% end %>
                <span class="max-w-[200px] md:max-w-none truncate"><%= item.metadata %></span>
              </div>
            </div>

            <div class="flex-shrink-0 mt-2 md:mt-0 md:ml-4">
              <.Pagination.pagination
                {@pagination}
                phx-hook="Pagination"
                data-testid="inbox-pagination"
                aria-label="Inbox pagination"
              />
            </div>
          </div>
        <% end %>
      <% end %>
    </div>
    """
  end

    @doc """
    Handles pagination events from the child pagination component.
    """
    def handle_event("page_change", %{"page" => page}, socket) do
      page = String.to_integer(page)
      send(self(), {:page_change, page})
      {:noreply, socket}
    end

    @doc """
    Handles keyboard navigation for the inbox list items.
    """
    def handle_event("keydown", %{"key" => "ArrowDown", "shiftKey" => false}, socket) do
      current_index = socket.assigns[:focused_index] || 0
      next_index = min(current_index + 1, length(socket.assigns.items) - 1)
      {:noreply, assign(socket, focused_index: next_index)}
    end

    def handle_event("keydown", %{"key" => "ArrowUp", "shiftKey" => false}, socket) do
      current_index = socket.assigns[:focused_index] || 0
      prev_index = max(current_index - 1, 0)
      {:noreply, assign(socket, focused_index: prev_index)}
    end

    def handle_event("keydown", %{"key" => "Enter"}, socket) do
      if focused_index = socket.assigns[:focused_index] do
        item = Enum.at(socket.assigns.items, focused_index)
        send(self(), {:item_selected, item.id})
      end
      {:noreply, socket}
    end

    def handle_event("keydown", %{"key" => " "}, socket) do
      if focused_index = socket.assigns[:focused_index] do
        item = Enum.at(socket.assigns.items, focused_index)
        send(self(), {:item_selected, item.id})
      end
      {:noreply, socket}
    end

    def handle_event("keydown", %{"key" => "Home"}, socket) do
      {:noreply, assign(socket, focused_index: 0)}
    end

    def handle_event("keydown", %{"key" => "End"}, socket) do
      {:noreply, assign(socket, focused_index: length(socket.assigns.items) - 1)}
    end

    def handle_event("keydown", %{"key" => "Escape"}, socket) do
      {:noreply, assign(socket, focused_index: nil)}
    end

    @doc """
    Handles focus management for inbox items.
    """
    def handle_event("focus", %{"id" => id}, socket) do
      index = Enum.find_index(socket.assigns.items, & &1.id == id)
      {:noreply, assign(socket, focused_index: index)}
    end

    def handle_event("blur", _, socket) do
      {:noreply, assign(socket, focused_index: nil)}
    end
  end

  attr :config, :map, default: %{}
  attr :type, :atom, default: :empty

  def empty_state(assigns) do
    ~H"""
    <div class="p-6 text-center empty-state" data-testid={"inbox-#{@type}-state"}>
      <%= case @type do %>
        <% :empty -> %>
          <div class="space-y-4">
            <.Skeleton.skeleton height="h-4" width="w-3/4" class="mx-auto mb-2" />
            <.Skeleton.skeleton height="h-4" width="w-1/2" class="mx-auto" />
          </div>

        <% :loading -> %>
          <div class="space-y-4">
            <.Skeleton.skeleton height="h-4" width="w-full" />
            <.Skeleton.skeleton height="h-4" width="w-full" />
            <.Skeleton.skeleton height="h-4" width="w-3/4" class="mx-auto" />
          </div>

        <% :error -> %>
          <div class="space-y-4">
            <.StatusIndicator.status_indicator variant={:destructive} class="mx-auto" />
            <div class="font-medium text-lg">Failed to load inbox items</div>
            <div class="text-gray-500">There was an error loading your notifications and tasks.</div>
            <button class="btn btn-primary" phx-click="retry">Retry</button>
          </div>

        <% :empty_with_action -> %>
          <div class="space-y-4">
            <.StatusIndicator.status_indicator variant={:info} class="mx-auto" />
            <div class="font-medium text-lg">No notifications or tasks</div>
            <div class="text-gray-500">Get started by creating your first task or checking for new notifications.</div>
            <button class="btn btn-primary" phx-click="create_task">Create Task</button>
          </div>
      <% end %>
    </div>
    """
  end
end
