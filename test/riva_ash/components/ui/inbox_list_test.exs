defmodule RivaAshWeb.Components.UI.InboxListTest do
  use ExUnit.Case, async: true
  use RivaAshWeb.ConnCase
  import RivaAshWeb.Components.UI.InboxList
  import Phoenix.LiveViewTest

  describe "inbox_list/1" do
    setup do
      %{
        desktop_items: [
          %{
            id: "1",
            type: :notification,
            content: "New reservation request",
            status: :info,
            status_label: "Informational",
            metadata: "2 hours ago"
          },
          %{
            id: "2",
            type: :task,
            content: "Complete client onboarding",
            status: :warning,
            status_label: "Action required",
            metadata: "Due today"
          }
        ],
        pagination: %{current_page: 1, total_pages: 5}
      }
    end

    test "renders desktop layout with items", %{desktop_items: items, pagination: pagination} do
      html = render_component(fn -> inbox_list(items: items, pagination: pagination, id: "test") end)

      assert html =~ "inbox-list"
      assert html =~ "inbox-item-1"
      assert html =~ "Notification"
      assert html =~ "Task"
      assert html =~ "New reservation request"
      assert html =~ "Complete client onboarding"
      assert html =~ "Pagination"
    end

    test "renders empty state when no items" do
      html = render_component(fn -> inbox_list(items: [], empty_state: %{message: "No notifications"}) end)

      assert html =~ "empty-state"
      assert html =~ "No notifications"
      assert html =~ "skeleton"
    end

    test "renders mobile touch targets", %{desktop_items: items, pagination: pagination} do
      html = render_component(fn -> inbox_list(items: items, pagination: pagination, id: "mobile", mobile_touch_targets: true) end)

      assert html =~ "min-h-\\[44px\\]"
      assert html =~ "touch-target"
    end

    test "renders correct ARIA attributes", %{desktop_items: items, pagination: pagination} do
      html = render_component(fn -> inbox_list(items: items, pagination: pagination, id: "aria-test") end)

      assert html =~ "role=\"list\""
      assert html =~ "aria-live=\"polite\""
      assert html =~ "role=\"listitem\""
      assert html =~ "tabindex=\"0\""
    end
  end
end
