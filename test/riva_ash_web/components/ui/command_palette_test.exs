defmodule RivaAshWeb.Components.UI.CommandPaletteTest do
  use RivaAshWeb.ConnCase, async: true

  import Phoenix.Component
  import Phoenix.LiveViewTest

  @commands [
    %{id: "1", label: "New Document", category: "file"},
    %{id: "2", label: "Open Settings", category: "preferences"},
    %{id: "3", label: "Search Files", category: "search"}
  ]

  test "renders command palette with proper structure" do
    assigns = %{
      commands: @commands,
      recent_commands: [],
      is_open: true,
      on_open_command_palette: "open_cmd",
      on_sidebar_collapse: "collapse_cmd",
      variant: :default,
      size: :md,
      density: "comfortable"
    }

    html = render_component(&RivaAsh.Components.UI.CommandPalette.render/1, assigns)
    assert html =~ "Type a command or search..."
    assert html =~ "Recent Commands"
    assert html =~ "Cmd+K"
  end

  test "handles command selection" do
    socket = %Phoenix.LiveView.Socket{}
    command = hd(@commands)

    html = render_component(&RivaAsh.Components.UI.CommandPalette.render/1, %{
      commands: @commands,
      recent_commands: [],
      is_open: true,
      on_open_command_palette: "open_cmd",
      on_sidebar_collapse: "collapse_cmd",
      active_command_id: command.id
    })

    assert html =~ "aria-selected=\"true\""
    assert html =~ "data-command=\"1\""
  end

  test "shows loading state during search" do
    assigns = %{
      commands: [],
      recent_commands: [],
      is_open: true,
      on_open_command_palette: "open_cmd",
      on_sidebar_collapse: "collapse_cmd",
      loading: true
    }

    html = render_component(&RivaAsh.Components.UI.CommandPalette.render/1, assigns)
    assert html =~ "animate-pulse"
    assert html =~ "No commands found"
  end
end
