defmodule RivaAsh.Components.UI.SkeletonTest do
  use ExUnit.Case, async: true
  use Phoenix.ConnTest

  import RivaAsh.Components.UI.Skeleton
  import Phoenix.LiveViewTest

  @moduletag :component
  @moduletag :skeleton

  describe "skeleton/1" do
    test "renders with correct default classes", %{conn: conn} do
      html = render_component(fn assigns -> ~H"<.skeleton variant={:lines} size={:md} />" end, conn)
      assert html =~ "bg-\\[var$--color-surface-skeleton$\\] animate-pulse rounded"
    end

    test "supports all variants", %{conn: conn} do
      variants = [:lines, :blocks, :avatar]
      Enum.each(variants, fn variant ->
        html = render_component(fn assigns -> ~H"<.skeleton variant={variant} size={:md} />" end, conn, variant: variant)
        assert html =~ "variant=\"#{variant}"
      end)
    end

    test "supports custom class", %{conn: conn} do
      html = render_component(fn assigns -> ~H"<.skeleton variant={:lines} size={:md} class=\"custom-class\" />" end, conn)
      assert html =~ "class=\"custom-class"
    end

    test "applies correct size classes for lines variant", %{conn: conn} do
      sizes = [sm: "h-2 w-3/4", md: "h-3 w-full", lg: "h-4 w-full"]
      Enum.each(sizes, fn {size, expected_class} ->
        html = render_component(fn assigns -> ~H"<.skeleton variant={:lines} size={size} />" end, conn, size: size)
        assert html =~ expected_class
      end)
    end

    test "applies correct size classes for blocks variant", %{conn: conn} do
      sizes = [sm: "h-8 w-8", md: "h-12 w-12", lg: "h-16 w-16"]
      Enum.each(sizes, fn {size, expected_class} ->
        html = render_component(fn assigns -> ~H"<.skeleton variant={:blocks} size={size} />" end, conn, size: size)
        assert html =~ expected_class
      end)
    end

    test "applies correct size classes for avatar variant", %{conn: conn} do
      sizes = [sm: "h-6 w-6", md: "h-8 w-8", lg: "h-10 w-10"]
      Enum.each(sizes, fn {size, expected_class} ->
        html = render_component(fn assigns -> ~H"<.skeleton variant={:avatar} size={size} />" end, conn, size: size)
        assert html =~ expected_class
      end)
    end
  end
end
