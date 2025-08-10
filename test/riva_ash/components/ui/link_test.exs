defmodule RivaAsh.Components.UI.LinkTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest

  alias RivaAsh.Components.UI.Link

  describe "link/1" do
    test "renders default link with correct classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Link.link href="#">Default Link</Link.link>
        """)

      assert html =~ ~r/class="[^"]*text-link[^"]*hover:underline[^"]*focus:underline/
      assert html =~ ~r/href="#"/
      assert html =~ ~r/role="link"/
    end

    test "renders inline variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Link.link href="#" variant={:inline}>Inline Link</Link.link>
        """)

      assert html =~ ~r/class="[^"]*text-link[^"]*hover:underline[^"]*focus:underline/
      assert html =~ ~r/visited:text-link-visited/
    end

    test "renders external variant with icon" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Link.link href="#" variant={:external} icon={:external} icon_position={:right}>
          External Link
        </Link.link>
        """)

      assert html =~ ~r/class="[^"]*text-link[^"]*hover:underline[^"]*focus:underline/
      assert html =~ ~r/<\.icon name="external" class="inline-block ml-1 align-middle"/
    end

    test "renders muted variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Link.link href="#" variant={:muted}>Muted Link</Link.link>
        """)

      assert html =~ ~r/class="[^"]*text-muted-foreground[^"]*hover:text-foreground[^"]*focus:text-foreground[^"]*focus:underline/
    end

    test "renders disabled state correctly" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Link.link href="#" disabled>Disabled Link</Link.link>
        """)

      assert html =~ ~r/<span class="[^"]*disabled/
      assert html =~ ~r/aria-disabled="true"/
      refute html =~ ~r/href="#"/
    end

    test "applies aria-label when provided" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Link.link href="#" aria_label="Accessible link">Link</Link.link>
        """)

      assert html =~ ~r/aria-label="Accessible link"/
    end

    test "renders icon on left position" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Link.link href="#" icon={:external} icon_position={:left}>Left Icon</Link.link>
        """)

      assert html =~ ~r/<\.icon name="external" class="inline-block mr-1 align-middle"/
    end

    test "renders icon on right position" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Link.link href="#" icon={:external} icon_position={:right}>Right Icon</Link.link>
        """)

      assert html =~ ~r/<\.icon name="external" class="inline-block ml-1 align-middle"/
    end

    test "applies size classes correctly" do
      sm_html =
        rendered_to_string(~H"""
        <Link.link href="#" size={:sm}>Small</Link.link>
        """)

      md_html =
        rendered_to_string(~H"""
        <Link.link href="#" size={:md}>Medium</Link.link>
        """)

      lg_html =
        rendered_to_string(~H"""
        <Link.link href="#" size={:lg}>Large</Link.link>
        """)

      assert sm_html =~ ~r/class="[^"]*text-xs/
      assert md_html =~ ~r/class="[^"]*text-sm/
      assert lg_html =~ ~r/class="[^"]*text-base/
    end
  end
end
