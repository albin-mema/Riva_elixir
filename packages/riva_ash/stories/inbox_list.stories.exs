defmodule RivaAshWeb.Stories.InboxListStories do
  use Surface.LiveView, page_title: "Inbox List Component"
  alias RivaAshWeb.Components.UI.{InboxList, StatusIndicator}

  @variants [:info, :warning, :success]
  @types [:notification, :task]

  def render(assigns) do
    ~H"""
    <div class="mx-auto p-6 max-w-4xl">
      <h1 class="mb-6 font-bold text-2xl">Inbox List Component</h1>

      <div class="mb-8">
        <h2 class="mb-4 font-semibold text-xl">Desktop Layout</h2>
        <InboxList.inbox_list
          id="desktop"
          items={@desktop_items}
          pagination={%{current_page: 1, total_pages: 5}}
        />
      </div>

      <div class="mb-8">
        <h2 class="mb-4 font-semibold text-xl">Mobile Layout (Simulated)</h2>
        <div class="bg-gray-50 mx-auto p-4 border rounded-lg max-w-md" style="width: 320px;">
          <InboxList.inbox_list
            id="mobile"
            items={@mobile_items}
            pagination={%{current_page: 1, total_pages: 3}}
            mobile_touch_targets={true}
          />
        </div>
      </div>

      <div class="mb-8">
        <h2 class="mb-4 font-semibold text-xl">Notification vs Task Variants</h2>
        <div class="gap-4 grid grid-cols-2">
          <div>
            <h3 class="mb-2 font-medium">Notification Item</h3>
            <InboxList.inbox_list
              id="notification"
              items={[
                %{
                  id: "1",
                  type: :notification,
                  content: "New reservation request for July 15",
                  status: :info,
                  status_label: "Informational",
                  metadata: "2 hours ago"
                }
              ]}
            />
          </div>
          <div>
            <h3 class="mb-2 font-medium">Task Item</h3>
            <InboxList.inbox_list
              id="task"
              items={[
                %{
                  id: "2",
                  type: :task,
                  content: "Complete client onboarding process",
                  status: :warning,
                  status_label: "Action required",
                  metadata: "Due today"
                }
              ]}
            />
          </div>
        </div>
      </div>

      <div class="mb-8">
        <h2 class="mb-4 font-semibold text-xl">Empty State</h2>
        <InboxList.inbox_list
          id="empty"
          items={[]}
          empty_state_type={:empty}
        />
      </div>

      <div class="mb-8">
        <h2 class="mb-4 font-semibold text-xl">Loading State</h2>
        <InboxList.inbox_list
          id="loading"
          items={[]}
          empty_state_type={:loading}
        />
      </div>

      <div class="mb-8">
        <h2 class="mb-4 font-semibold text-xl">Error State</h2>
        <InboxList.inbox_list
          id="error"
          items={[]}
          empty_state_type={:error}
        />
      </div>

      <div class="mb-8">
        <h2 class="mb-4 font-semibold text-xl">Empty State with Action</h2>
        <InboxList.inbox_list
          id="empty-action"
          items={[]}
          empty_state_type={:empty_with_action}
        />
      </div>

      <div class="mb-8">
        <h2 class="mb-4 font-semibold text-xl">Status Indicator Variants</h2>
        <div class="space-y-4">
          <%= for variant <- @variants do %>
            <div class="flex items-center">
              <StatusIndicator.status_indicator variant={variant} class="mr-2" />
              <span class="capitalize"><%= variant %> status indicator</span>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  def mount(_params, _session, socket) do
    desktop_items = [
      %{
        id: "1",
        type: :notification,
        content: "New reservation request for July 15",
        status: :info,
        status_label: "Informational",
        metadata: "2 hours ago"
      },
      %{
        id: "2",
        type: :task,
        content: "Complete client onboarding process",
        status: :warning,
        status_label: "Action required",
        metadata: "Due today"
      },
      %{
        id: "3",
        type: :notification,
        content: "Payment received for reservation #456",
        status: :success,
        status_label: "Completed",
        metadata: "Yesterday"
      }
    ]

    mobile_items = [
      %{
        id: "1",
        type: :notification,
        content: "New reservation request for July 15",
        status: :info,
        status_label: "Informational",
        metadata: "2 hours ago"
      },
      %{
        id: "2",
        type: :task,
        content: "Complete client onboarding process",
        status: :warning,
        status_label: "Action required",
        metadata: "Due today"
      }
    ]

    {:ok, assign(socket, desktop_items: desktop_items, mobile_items: mobile_items)}
  end
end
