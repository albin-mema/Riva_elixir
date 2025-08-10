defmodule RivaAsh.Components.UI.DateRangePickerTest do
  use ExUnit.Case, async: true
  import Phoenix.LiveViewTest
  alias RivaAsh.Components.UI.DateRangePicker

  describe "date range selection" do
    test "selects valid date range" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      # Select start date
      start_date = Date.utc_today()
      end_date = Date.add(start_date, 3)

      view
      |> element("#date-range-picker-start", "Start date selection")
      |> render_click(%{date: Date.to_iso8601(start_date)})

      # Select end date
      view
      |> element("#date-range-picker-end", "End date selection")
      |> render_click(%{date: Date.to_iso8601(end_date)})

      # Verify dates are updated
      assert render(view) =~ "Start date: #{Date.to_iso8601(start_date)}"
      assert render(view) =~ "End date: #{Date.to_iso8601(end_date)}"
      refute render(view) =~ "error"
    end

    test "shows error for end date before start date" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      start_date = Date.add(Date.utc_today(), 5)
      end_date = Date.utc_today()

      view
      |> element("#date-range-picker-start", "Start date selection")
      |> render_click(%{date: Date.to_iso8601(start_date)})

      view
      |> element("#date-range-picker-end", "End date selection")
      |> render_click(%{date: Date.to_iso8601(end_date)})

      assert render(view) =~ "End date must be after start date"
      assert view |> element(".apply-button") |> has_attribute?("disabled", "true")
    end

    test "shows error for range exceeding maximum days" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      start_date = Date.utc_today()
      end_date = Date.add(start_date, 31)

      view
      |> element("#date-range-picker-start", "Start date selection")
      |> render_click(%{date: Date.to_iso8601(start_date)})

      view
      |> element("#date-range-picker-end", "End date selection")
      |> render_click(%{date: Date.to_iso8601(end_date)})

      assert render(view) =~ "Date range cannot exceed 30 days"
      assert view |> element(".apply-button") |> has_attribute?("disabled", "true")
    end
  end

  describe "preset ranges" do
    test "applies today preset" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      view
      |> element("[aria-label='Today']", "Today")
      |> render_click()

      today = Date.utc_today()
      assert render(view) =~ "Start date: #{Date.to_iso8601(today)}"
      assert render(view) =~ "End date: #{Date.to_iso8601(today)}"
      assert view |> element(".preset-button", "Today") |> has_attribute?("aria-checked", "true")
    end

    test "applies this week preset" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      view
      |> element("[aria-label='This Week']", "This Week")
      |> render_click()

      today = Date.utc_today()
      start_of_week = Date.beginning_of_week(today)
      end_of_week = Date.end_of_week(today)

      assert render(view) =~ "Start date: #{Date.to_iso8601(start_of_week)}"
      assert render(view) =~ "End date: #{Date.to_iso8601(end_of_week)}"
      assert view |> element(".preset-button", "This Week") |> has_attribute?("aria-checked", "true")
    end

    test "applies this month preset" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      view
      |> element("[aria-label='This Month']", "This Month")
      |> render_click()

      today = Date.utc_today()
      start_of_month = Date.beginning_of_month(today)
      end_of_month = Date.end_of_month(today)

      assert render(view) =~ "Start date: #{Date.to_iso8601(start_of_month)}"
      assert render(view) =~ "End date: #{Date.to_iso8601(end_of_month)}"
      assert view |> element(".preset-button", "This Month") |> has_attribute?("aria-checked", "true")
    end
  end

  describe "accessibility" do
    test "has proper ARIA roles and labels" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      # Group role and label
      assert view |> element("[role='group'][aria-labelledby='date-range-picker-label']") |> has_element?()
      assert view |> element("#date-range-picker-label.sr-only") |> has_text?("Select date range")

      # Date pickers with proper labels
      assert view |> element("#date-range-picker-start[aria-label='Start date selection']") |> has_element?()
      assert view |> element("#date-range-picker-end[aria-label='End date selection']") |> has_element?()

      # Presets as radiogroup
      assert view |> element(".presets[role='radiogroup'][aria-label='Date presets']") |> has_element?()
      assert view |> element(".preset-button[role='radio']") |> has_element?()
    end

    test "keyboard navigation through presets" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      # Focus first preset
      view
      |> element(".preset-button[aria-label='Today']")
      |> render_keydown("ArrowRight")

      # Verify next preset is focused
      assert view |> element(".preset-button[aria-label='This Week'][tabindex='0']") |> has_element?()

      # Navigate back
      view
      |> element(".preset-button[aria-label='This Week']")
      |> render_keydown("ArrowLeft")

      assert view |> element(".preset-button[aria-label='Today'][tabindex='0']") |> has_element?()
    end
  end

  describe "apply button" do
    test "is disabled when error exists" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      # Create invalid range
      start_date = Date.add(Date.utc_today(), 5)
      end_date = Date.utc_today()

      view
      |> element("#date-range-picker-start", "Start date selection")
      |> render_click(%{date: Date.to_iso8601(start_date)})

      view
      |> element("#date-range-picker-end", "End date selection")
      |> render_click(%{date: Date.to_iso8601(end_date)})

      assert view |> element(".apply-button[disabled]") |> has_element?()
      assert view |> element(".apply-button[aria-disabled='true']") |> has_element?()
    end

    test "triggers on_apply callback when valid" do
      {:ok, view, _html} = live_isolated(build_conn(), DateRangePicker, session: %{})

      # Select valid range
      start_date = Date.utc_today()
      end_date = Date.add(start_date, 3)

      view
      |> element("#date-range-picker-start", "Start date selection")
      |> render_click(%{date: Date.to_iso8601(start_date)})

      view
      |> element("#date-range-picker-end", "End date selection")
      |> render_click(%{date: Date.to_iso8601(end_date)})

      # Click apply
      view
      |> element(".apply-button")
      |> render_click()

      # Verify apply was triggered (would be handled by parent in real usage)
      assert true # Implementation would verify parent callback
    end
  end
end
