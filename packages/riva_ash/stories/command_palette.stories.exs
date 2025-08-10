import RivaAsh.Components.UI.CommandPalette
import RivaAsh.Components.UI.AppShell
import RivaAsh.Components.UI.Kbd

defmodule CommandPaletteStories do
  use Surface.LiveView

  @doc """
  Basic command palette with command set
  """
  def basic(assigns) do
    ~H"""
    <div class="p-4">
      <CommandPalette
        commands={[
          %{id: "new", label: "New Document", icon: "document"},
          %{id: "search", label: "Search", icon: "search"},
          %{id: "settings", label: "Settings", icon: "settings"}
        ]}
        is_open={true}
      />
    </div>
    """
  end

  @doc """
  Loading and empty states demonstration
  """
  def loading_empty(assigns) do
    ~H"""
    <div class="space-y-4 p-4">
      <div>
        <h3 class="mb-2 text-lg">Loading State</h3>
        <CommandPalette is_open={true} loading={true} />
      </div>
      <div>
        <h3 class="mb-2 text-lg">Empty State</h3>
        <CommandPalette is_open={true} commands={[]} />
      </div>
    </div>
    """
  end

  @doc """
  Keyboard navigation demonstration
  """
  def keyboard_navigation(assigns) do
    ~H"""
    <div class="p-4">
      <h3 class="mb-2 text-lg">Keyboard Navigation Flow</h3>
      <p class="mb-4 text-muted-foreground">
        Use arrow keys to navigate, Enter to select, Escape to close
      </p>
      <CommandPalette
        is_open={true}
        commands={[
          %{id: "cmd1", label: "First Command"},
          %{id: "cmd2", label: "Second Command"},
          %{id: "cmd3", label: "Third Command"}
        ]}
      />
    </div>
    """
  end

  @doc """
  Integration with AppShell
  """
  def app_shell_integration(assigns) do
    ~H"""
    <AppShell>
      <:sidebar>
        <div class="p-4">Sidebar Content</div>
      </:sidebar>
      <div class="p-4">
        <h1 class="mb-4 text-2xl">AppShell Integration</h1>
        <p class="mb-4">Command palette works within AppShell context</p>
        <CommandPalette is_open={true} />
      </div>
    </AppShell>
    """
  end

  @doc """
  Mobile viewport behavior
  """
  def mobile_viewport(assigns) do
    ~H"""
    <div class="mx-auto p-4 border rounded-lg max-w-md" style="height: 600px;">
      <div class="flex justify-between items-center p-2 border-b">
        <h2 class="text-lg">Mobile View</h2>
        <Kbd>Cmd+K</Kbd>
      </div>
      <div class="p-2">
        <CommandPalette is_open={true} size="sm" />
      </div>
    </div>
    """
  end
end
