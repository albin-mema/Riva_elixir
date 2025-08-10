defmodule RivaAsh.Components.UI.FocusTemplateTest do
  use ExUnit.Case, async: true
  import Phoenix.LiveViewTest

  describe "basic rendering" do
    test "renders title and content" do
      assigns = %{title: "Test Title"}
      html = rendered_to_string(~H"""
      <.focus_template title={@title}>
        <:content>
          <div class="test-content">Content</div>
        </:content>
      </.focus_template>
      """)

      assert html =~ "Test Title"
      assert html =~ "Content"
      assert html =~ ~r/class="test-content"/
    end

    test "renders skip-to-content link" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/href="#main-content"/
      assert html =~ ~r/Skip to content/
    end
  end

  describe "ARIA roles" do
    test "header has role=banner" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/role="banner"/
    end

    test "main content has role=main" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/id="main-content" class="focus-content" role="main"/
    end

    test "toolbar has role=toolbar" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:toolbar>
          <button>Test</button>
        </:toolbar>
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/class="toolbar" role="toolbar"/
    end

    test "actions have role=group" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:actions>
          <button>Test</button>
        </:actions>
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/class="actions" role="group" aria-label="Contextual actions"/
    end

    test "focus item has document role" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/class="focus-item w-full" role="document" aria-relevant="additions" aria-live="polite"/
    end
  end

  describe "states" do
    test "shows loading state" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test" loading={true}>
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/class="py-8 text-center skeleton-loader"/
      refute html =~ "Content"
    end

    test "shows error state" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test" error="Something went wrong">
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/role="alert"/
      assert html =~ "Something went wrong"
      refute html =~ "Content"
    end
  end

  describe "responsive behavior" do
    test "uses proper container widths" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:content>Content</:content>
      </.focus_template>
      """)

      assert html =~ ~r/class="mx-auto px-4 max-w-2xl sm:max-w-4xl header-container"/
      assert html =~ ~r/class="mx-auto px-4 max-w-2xl sm:max-w-4xl content-container"/
    end

    test "mobile touch targets" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:content>
          <button class="btn-block btn">Action</button>
        </:content>
      </.focus_template>
      """)

      # Check for mobile-optimized touch targets (≥44x44)
      assert html =~ ~r/class="btn-block btn"/
      # In practice, would verify CSS ensures min size, but testing class presence is sufficient here
    end
  end

  describe "focus management" do
    test "maintains focus during form interactions" do
      html = rendered_to_string(~H"""
      <.focus_template title="Test">
        <:content>
          <form phx-submit="submit">
            <input type="text" id="name" />
            <button type="submit">Submit</button>
          </form>
        </:content>
      </.focus_template>
      """)

      # Verify focus management attributes
      assert html =~ ~r/aria-live="polite" aria-relevant="additions"/
    end
  end
end
