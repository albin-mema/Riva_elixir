defmodule RivaAshWeb.Components.Molecules.CardTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  import RivaAshWeb.Components.Molecules.Card

  describe "card/1" do
    test "renders basic card" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card>
          Card content
        </.card>
        """)

      assert html =~ "Card content"
      assert html =~ "rounded-lg border bg-card"
    end

    test "renders card with header" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card>
          <:header>
            Card Header
          </:header>
          Card content
        </.card>
        """)

      assert html =~ "Card Header"
      assert html =~ "Card content"
      assert html =~ "flex flex-col space-y-1.5 p-6"
    end

    test "renders card with footer" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card>
          Card content
          <:footer>
            Card Footer
          </:footer>
        </.card>
        """)

      assert html =~ "Card content"
      assert html =~ "Card Footer"
      assert html =~ "flex items-center p-6 pt-0"
    end

    test "renders card with header and footer" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card>
          <:header>
            Card Header
          </:header>
          Card content
          <:footer>
            Card Footer
          </:footer>
        </.card>
        """)

      assert html =~ "Card Header"
      assert html =~ "Card content"
      assert html =~ "Card Footer"
    end

    test "renders with custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card class="custom-card">
          Card content
        </.card>
        """)

      assert html =~ "Card content"
      assert html =~ "custom-card"
    end

    test "renders with global attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card id="test-card" data-testid="card">
          Card content
        </.card>
        """)

      assert html =~ "Card content"
      assert html =~ ~s(id="test-card")
      assert html =~ ~s(data-testid="card")
    end
  end

  describe "card_title/1" do
    test "renders card title" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_title>
          Test Title
        </.card_title>
        """)

      assert html =~ "Test Title"
      # Should use UI.Text with h3 variant
      assert html =~ "scroll-m-20 text-2xl font-semibold"
    end

    test "renders card title with custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_title class="custom-title">
          Test Title
        </.card_title>
        """)

      assert html =~ "Test Title"
      assert html =~ "custom-title"
    end
  end

  describe "card_description/1" do
    test "renders card description" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_description>
          Test Description
        </.card_description>
        """)

      assert html =~ "Test Description"
      # Should use UI.Text with small variant and muted color
      assert html =~ "text-sm font-medium"
      assert html =~ "text-muted-foreground"
    end

    test "renders card description with custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_description class="custom-description">
          Test Description
        </.card_description>
        """)

      assert html =~ "Test Description"
      assert html =~ "custom-description"
    end
  end

  describe "card integration with UI components" do
    test "uses UI.Card internally" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card>
          <:header>
            <.card_title>Title</.card_title>
            <.card_description>Description</.card_description>
          </:header>
          Content
          <:footer>
            Footer content
          </:footer>
        </.card>
        """)

      assert html =~ "Title"
      assert html =~ "Description"
      assert html =~ "Content"
      assert html =~ "Footer content"
      # Should contain UI.Card classes
      assert html =~ "rounded-lg border bg-card"
      # Should contain UI.Text classes for title and description
      assert html =~ "scroll-m-20 text-2xl font-semibold"
      assert html =~ "text-muted-foreground"
    end
  end
end
