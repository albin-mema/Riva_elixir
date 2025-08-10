defmodule ToastProviderTest do
  use ExUnit.Case, async: true
  import Phoenix.LiveViewTest

  alias RivaAshWeb.Components.Ui.ToastProvider

  setup do
    {:ok, view, _html} = live_isolated(build_conn(), ToastProvider)
    %{view: view}
  end

  describe "queue management" do
    test "adds toast to queue", %{view: view} do
      view
      |> ToastProvider.add_toast(:success, "Success message")
      |> assert_rendered_toast(:success, "Success message")
    end

    test "removes toast after timeout", %{view: view} do
      view
      |> ToastProvider.add_toast(:error, "Error message", timeout: 10)
      |> assert_rendered_toast(:error, "Error message")

      # Wait for timeout + animation duration
      :timer.sleep(150)

      refute_rendered_toast(view, "Error message")
    end

    test "maintains multiple toasts in queue", %{view: view} do
      view
      |> ToastProvider.add_toast(:success, "First message")
      |> ToastProvider.add_toast(:error, "Second message")
      |> assert_rendered_toast(:success, "First message")
      |> assert_rendered_toast(:error, "Second message")
    end
  end

  describe "ARIA announcements" do
    test "announces toast with polite politeness by default", %{view: view} do
      view
      |> ToastProvider.add_toast(:info, "Info message")
      |> assert_aria_live_region("polite", "Info message")
    end

    test "announces toast with assertive politeness when specified", %{view: view} do
      view
      |> ToastProvider.add_toast(:error, "Critical error", polite: false)
      |> assert_aria_live_region("assertive", "Critical error")
    end
  end

  describe "dismissal behavior" do
    test "dismisses toast when dismiss button clicked", %{view: view} do
      view
      |> ToastProvider.add_toast(:warning, "Warning message")
      |> element("[aria-label='Dismiss']")
      |> render_click()

      refute_rendered_toast(view, "Warning message")
    end

    test "dismisses toast with escape key", %{view: view} do
      view
      |> ToastProvider.add_toast(:info, "Info message")
      |> send_event("keydown", %{"key" => "Escape"})

      refute_rendered_toast(view, "Info message")
    end
  end

  describe "type-specific styling" do
    test "applies success styling", %{view: view} do
      view
      |> ToastProvider.add_toast(:success, "Success message")
      |> assert_toast_has_class("text-success")
    end

    test "applies error styling", %{view: view} do
      view
      |> ToastProvider.add_toast(:error, "Error message")
      |> assert_toast_has_class("text-destructive")
    end

    test "applies warning styling", %{view: view} do
      view
      |> ToastProvider.add_toast(:warning, "Warning message")
      |> assert_toast_has_class("text-warning")
    end

    test "applies info styling", %{view: view} do
      view
      |> ToastProvider.add_toast(:info, "Info message")
      |> assert_toast_has_class("text-primary")
    end
  end

  # Helper assertions
  defp assert_rendered_toast(view, type, message) do
    assert view
           |> element("[data-type='#{type}']")
           |> render() =~ message

    view
  end

  defp refute_rendered_toast(view, message) do
    refute view
           |> render()
           |> String.contains?(message)
  end

  defp assert_aria_live_region(view, politeness, message) do
    assert view
           |> element("#toast-live-region")
           |> render() =~ "aria-live=\"#{politeness}\""

    assert view
           |> element("#toast-live-region")
           |> render() =~ message
  end

  defp assert_toast_has_class(view, class) do
    assert view
           |> element(".#{class}")
           |> render()
  end
end
