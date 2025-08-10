defmodule RivaAsh.Components.UI.RightRailTest do
  use RivaAsh.DataCase, async: true
  import Phoenix.LiveViewTest
  alias RivaAsh.Components.UI.RightRail

  describe "ARIA attributes" do
    test "has proper complementary role" do
      {:ok, _view, html} = live_isolated(build_conn(), RightRail, session: %{})

      assert html =~ ~r/role="complementary"/
      assert html =~ ~r/aria-label="Context panel"/
    end

    test "tab navigation has correct ARIA attributes" do
      {:ok, _view, html} = live_isolated(build_conn(), RightRail, session: %{})

      assert html =~ ~r/role="tablist"/
      assert html =~ ~r/role="tab" aria-selected="true" aria-controls="details-panel"/
      assert html =~ ~r/role="tab" aria-selected="false" aria-controls="history-panel"/
      assert html =~ ~r/role="tabpanel" id="details-panel" aria-labelledby="tab-details"/
    end

    test "screen reader announcements for tab changes" do
      {:ok, view, _html} = live_isolated(build_conn(), RightRail, session: %{})

      assert view
             |> element("[aria-controls='history-panel']")
             |> render_click() =~
               ~r/aria-selected="true" aria-live="polite">History<\\/span>/

      assert view
             |> element("[aria-controls='details-panel']")
             |> render_click() =~
               ~r/aria-selected="true" aria-live="polite">Details<\\/span>/
    end
  end

  describe "responsive behavior" do
    test "mobile slide-in panel implementation" do
      {:ok, _view, html} = live_isolated(build_conn(), RightRail, session: %{})

      # Mobile closed state
      assert html =~ ~r/class=".*translate-x-full.*sm:translate-x-0"/

      # Mobile open state
      {:ok, view, _} = live_isolated(build_conn(), RightRail, session: %{"open" => true})
      assert view_html(view) =~ ~r/class=".*translate-x-0.*sm:translate-x-0"/

      # Desktop state
      assert view_html(view) =~ ~r/class=".*sm:translate-x-0"/
    end

    test "overlay click closes panel on mobile" do
      {:ok, view, _} = live_isolated(build_conn(), RightRail, session: %{"open" => true})
      assert view |> element(".lg\\:hidden") |> render_click()
      refute view_html(view) =~ ~r/translate-x-0/
    end

    test "ESC key dismisses panel" do
      {:ok, view, _} = live_isolated(build_conn(), RightRail, session: %{"open" => true})
      assert view |> element("[phx-keydown.esc]") |> render_keydown("esc")
      refute view_html(view) =~ ~r/translate-x-0/
    end

    test "touch targets meet minimum size requirements" do
      {:ok, _view, html} = live_isolated(build_conn(), RightRail, session: %{})

      # Check tab navigation touch targets
      assert html =~ ~r/class=".*h-11.*"/
      assert html =~ ~r/class=".*min-w-11.*"/
    end
  end

  describe "tab navigation" do
    test "content switching works correctly" do
      {:ok, view, _} = live_isolated(build_conn(), RightRail, session: %{})

      # Initial state - details tab selected
      assert view_html(view) =~ ~r/<div role="tabpanel" id="details-panel" .* class=""/
      refute view_html(view) =~ ~r/<div role="tabpanel" id="history-panel" .* class="hidden"/

      # Switch to history tab
      assert view |> element("[aria-controls='history-panel']") |> render_click()
      refute view_html(view) =~ ~r/<div role="tabpanel" id="details-panel" .* class=""/
      assert view_html(view) =~ ~r/<div role="tabpanel" id="history-panel" .* class=""/
    end

    test "focus management on tab change" do
      {:ok, view, _} = live_isolated(build_conn(), RightRail, session: %{})

      # Initial focus on first tab
      assert view_html(view) =~ ~r/<div role="tab" .* tabindex="0"/

      # Switch tab and check focus
      assert view |> element("[aria-controls='history-panel']") |> render_click()
      assert view_html(view) =~ ~r/<div role="tab" .* tabindex="-1"/
      assert view_html(view) =~ ~r/<div role="tab" .* tabindex="0" aria-selected="true"/
    end
  end

  describe "mobile gesture handling" do
    test "swipe gesture dismisses panel" do
      {:ok, view, _} = live_isolated(build_conn(), RightRail, session: %{"open" => true})
      assert view |> element("[phx-hook='RightRailSwipe']") |> render_hook("swipe", %{"direction" => "left"})
      refute view_html(view) =~ ~r/translate-x-0/
    end

    test "swipe gesture does not dismiss on desktop" do
      {:ok, view, _} = live_isolated(build_conn(), RightRail, session: %{"open" => true, "is_mobile" => false})
      assert view |> element("[phx-hook='RightRailSwipe']") |> render_hook("swipe", %{"direction" => "left"})
      assert view_html(view) =~ ~r/translate-x-0/
    end
  end

  describe "integration with AppShell" do
    test "works with AppShell's right_rail slot" do
      {:ok, _view, html} = live_isolated(build_conn(), RivaAsh.Components.UI.AppShell, session: %{})

      # Check if RightRail is properly integrated
      assert html =~ ~r/<div class="right-rail".*role="complementary"/
      assert html =~ ~r/phx-keydown.esc={@on_right_rail_toggle}/
    end
  end
end
