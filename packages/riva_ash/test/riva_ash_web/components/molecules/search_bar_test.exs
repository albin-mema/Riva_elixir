defmodule RivaAshWeb.Components.Molecules.SearchBarTest do
  @moduledoc """
  Test suite for Molecules.SearchBar component.
  """
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.Molecules.SearchBar

  describe "search_bar/1" do
    test "renders search bar with default placeholder" do
      assigns = %{}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar />
      """)

      assert html =~ "Search..."
      assert html =~ "pl-10"
      assert html =~ "relative"
    end

    test "renders search bar with custom placeholder" do
      assigns = %{placeholder: "Find users..."}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar placeholder={@placeholder} />
      """)

      assert html =~ "Find users..."
    end

    test "renders search bar with value" do
      assigns = %{value: "test query"}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar value={@value} />
      """)

      assert html =~ "test query"
    end

    test "renders search bar with disabled state" do
      assigns = %{disabled: true}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar disabled={@disabled} />
      """)

      assert html =~ "opacity-50"
      assert html =~ "cursor-not-allowed"
      assert html =~ "disabled"
    end

    test "renders search bar with custom class" do
      assigns = %{class: "custom-search-class"}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar class={@class} />
      """)

      assert html =~ "custom-search-class"
    end

    test "renders search bar with rest attributes" do
      assigns = %{}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar data-role="global-search" />
      """)

      assert html =~ ~r/data-role="global-search"/
    end

    test "renders search bar with icon" do
      assigns = %{}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar />
      """)

      assert html =~ "search-icon"
      assert html =~ "text-muted-foreground"
    end

    test "renders search bar with primary button when enabled" do
      assigns = %{disabled: false}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar disabled={@disabled} />
      """)

      assert html =~ "bg-primary"
      assert html =~ "hover:bg-primary/80"
    end

    test "renders search bar with secondary button when disabled" do
      assigns = %{disabled: true}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar disabled={@disabled} />
      """)

      assert html =~ "bg-secondary"
      assert html =~ "hover:bg-secondary/80"
    end

    test "renders search bar with inner block content" do
      assigns = %{}

      html = rendered_to_string(~H"""
      <SearchBar.search_bar>
        Extra content
      </SearchBar.search_bar>
      """)

      assert html =~ "Extra content"
    end

    test "validates placeholder attribute" do
      assigns = %{}
      assert_raise ArgumentError, "Invalid search bar attributes: placeholder must be a string", fn ->
        rendered_to_string(~H"""
        <SearchBar.search_bar placeholder={123} />
        """)
      end
    end

    test "validates value attribute" do
      assigns = %{}
      assert_raise ArgumentError, "Invalid search bar attributes: value must be a string", fn ->
        rendered_to_string(~H"""
        <SearchBar.search_bar value={123} />
        """)
      end
    end
  end
end