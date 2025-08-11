defmodule RivaAshWeb.Components.Molecules.UserProfileTest do
  @moduledoc """
  Test suite for Molecules.UserProfile component.
  """
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.Molecules.UserProfile

  describe "user_profile/1" do
    test "renders user profile with name only" do
      assigns = %{name: "John Doe"}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} />
      """)

      assert html =~ "John Doe"
      assert html =~ "flex items-center space-x-3"
    end

    test "renders user profile with name and email" do
      assigns = %{name: "John Doe", email: "john@example.com"}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} email={@email} />
      """)

      assert html =~ "John Doe"
      assert html =~ "john@example.com"
      assert html =~ "text-muted-foreground"
    end

    test "renders user profile with avatar source" do
      assigns = %{name: "John Doe", avatar_src: "/avatar.jpg"}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} avatar_src={@avatar_src} />
      """)

      assert html =~ "John Doe"
      assert html =~ "/avatar.jpg"
    end

    test "renders user profile with sm size" do
      assigns = %{name: "John Doe", size: "sm"}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} size={@size} />
      """)

      assert html =~ "John Doe"
      assert html =~ "w-8 h-8"
    end

    test "renders user profile with default size" do
      assigns = %{name: "John Doe", size: "default"}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} size={@size} />
      """)

      assert html =~ "John Doe"
      assert html =~ "w-10 h-10"
    end

    test "renders user profile without actions" do
      assigns = %{name: "John Doe", show_actions: false}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} show_actions={@show_actions} />
      """)

      assert html =~ "John Doe"
      refute html =~ "cog_6_tooth"
    end

    test "renders user profile with actions" do
      assigns = %{name: "John Doe", show_actions: true}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} show_actions={@show_actions} />
      """)

      assert html =~ "John Doe"
      assert html =~ "cog_6_tooth"
    end

    test "renders user profile with custom class" do
      assigns = %{name: "John Doe", class: "custom-profile-class"}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} class={@class} />
      """)

      assert html =~ "John Doe"
      assert html =~ "custom-profile-class"
    end

    test "renders user profile with rest attributes" do
      assigns = %{name: "John Doe"}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} data_role="user-profile" />
      """)

      assert html =~ "John Doe"
      assert html =~ ~r/data_role="user-profile"/
    end

    test "renders user profile with action slots" do
      assigns = %{name: "John Doe"}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name}>
        Custom action
      </UserProfile.user_profile>
      """)

      assert html =~ "John Doe"
      assert html =~ "Custom action"
    end

    test "validates name attribute" do
      assigns = %{}
      assert_raise ArgumentError, "Invalid user profile attributes: name must be a non-empty string", fn ->
        rendered_to_string(~H"""
        <UserProfile.user_profile name="" />
        """)
      end
    end

    test "validates email attribute" do
      assigns = %{}
      assert_raise ArgumentError, "Invalid user profile attributes: email must be a string or nil", fn ->
        rendered_to_string(~H"""
        <UserProfile.user_profile name="John" email={123} />
        """)
      end
    end

    test "handles nil email gracefully" do
      assigns = %{name: "John Doe", email: nil}

      html = rendered_to_string(~H"""
      <UserProfile.user_profile name={@name} email={@email} />
      """)

      assert html =~ "John Doe"
      refute html =~ "text-muted-foreground"
    end
  end
end