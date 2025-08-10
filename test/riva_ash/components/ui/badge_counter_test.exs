defmodule BadgeCounterTest do
  use ExUnit.Case, async: true
  use Surface.LiveViewTest

  alias RivaAsh.Components.UI.BadgeCounter

  describe "value display" do
    test "shows exact value when below max" do
      assigns = %{value: 42}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ "42"
    end

    test "shows overflow value when above max" do
      assigns = %{value: 105}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ "99+"
    end

    test "respects custom max value" do
      assigns = %{value: 50, max: 25}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ "25+"
    end
  end

  describe "variant rendering" do
    test "applies default variant classes" do
      assigns = %{value: 5}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ "bg-secondary text-secondary-foreground"
    end

    test "applies info variant classes" do
      assigns = %{value: 5, variant: :info}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ "bg-info text-info-foreground"
    end

    test "applies success variant classes" do
      assigns = %{value: 5, variant: :success}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ "bg-success text-success-foreground"
    end
  end

  describe "removable state" do
    test "shows remove button when removable is true" do
      assigns = %{value: 5, removable: true}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ "icon=\"close\""
    end

    test "triggers on_remove event when button clicked" do
      assigns = %{value: 5, removable: true, on_remove: "remove"}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ ~r/phx-click="remove"/
    end

    test "does not show remove button when removable is false" do
      assigns = %{value: 5, removable: false}
      html = ~H"<BadgeCounter {@assigns} />"
      refute html =~ "icon=\"close\""
    end
  end

  describe "accessibility" do
    test "includes role=status attribute" do
      assigns = %{value: 5}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ ~r/role="status"/
    end

    test "includes aria-live=polite attribute" do
      assigns = %{value: 5}
      html = ~H"<BadgeCounter {@assigns} />"
      assert html =~ ~r/aria-live="polite"/
    end
  end
end
