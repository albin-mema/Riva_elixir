defmodule AppShellStories do
  use Surface.LiveView

  alias RivaAsh.Components.UI.AppShell
  alias RivaAsh.Components.UI.Sidebar
  alias RivaAsh.Components.UI.CommandPalette

  data sidebar_collapsed, :boolean, default: false
  data right_rail_open, :boolean, default: false

  def render(assigns) do
    ~H"""
    <div class="bg-surface-50 p-4 min-h-screen">
      <AppShell
        sidebar_collapsed={@sidebar_collapsed}
        on_sidebar_collapse="toggle_sidebar"
        right_rail_open={@right_rail_open}
        on_right_rail_toggle="toggle_right_rail"
        on_open_command_palette="open_command_palette"
      >
        <!-- Header slot -->
        <:header>
          <div class="flex justify-between items-center bg-surface-100 px-4 h-16">
            <.icon_button icon="menu" phx_click="toggle_sidebar" class="lg:hidden" />
            <div class="font-semibold text-lg">AppShell Demo</div>
            <div class="flex items-center gap-2">
              <.icon_button icon="search" />
              <.icon_button icon="bell" />
              <.icon_button icon="user" />
            </div>
          </div>
        </:header>

        <!-- Sidebar slot -->
        <:sidebar :let={density} collapsed={@sidebar_collapsed} on_collapse="toggle_sidebar">
          <Sidebar
            sections={[
              %{id: "dashboard", label: "Dashboard", icon: "dashboard", links: [%{id: "overview", label: "Overview", icon: "chart"}]},
              %{id: "calendar", label: "Calendar", icon: "calendar", links: [%{id: "schedule", label: "Schedule", icon: "calendar"}]}
            ]}
            active_section="dashboard"
            active_link="overview"
            collapsed={@sidebar_collapsed}
            on_collapse="toggle_sidebar"
            density={density}
          />
        </:sidebar>

        <!-- Content slot -->
        <:content>
          <div class="bg-surface-100 p-4 rounded-lg min-h-[500px]">
            <h1 class="mb-4 font-bold text-2xl">Main Content Area</h1>
            <p class="text-surface-700">This is the primary content area of the application.</p>
            <.button class="mt-4" phx-click="toggle_right_rail">Toggle RightRail</.button>
          </div>
        </:content>

        <!-- RightRail slot -->
        <:right_rail open={@right_rail_open} on_toggle="toggle_right_rail" density={if(@sidebar_collapsed, do: "compact", else: "comfortable")}>
          <div class="bg-surface-100 p-4 border-surface-300 border-l h-full">
            <h2 class="mb-4 font-bold text-xl">Context Panel</h2>
            <p>Additional information and controls appear here.</p>
          </div>
        </:right_rail>
      </AppShell>

      <CommandPalette id="command-palette" />
    </div>
    """
  end

  def handle_event("toggle_sidebar", _, socket) do
    {:noreply, assign(socket, sidebar_collapsed: !socket.assigns.sidebar_collapsed)}
  end

  def handle_event("toggle_right_rail", _, socket) do
    {:noreply, assign(socket, right_rail_open: !socket.assigns.right_rail_open)}
  end

  def handle_event("open_command_palette", _, socket) do
    # In a real implementation, this would open the command palette
    {:noreply, socket}
  end
end
