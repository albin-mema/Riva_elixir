defmodule DropdownMenuStories do
  use Phoenix.Component
  import RivaAsh.Components.UI.DropdownMenu
  import RivaAsh.Components.UI.AppShell
  import RivaAsh.Components.UI.Header

  def basic_menu(assigns) do
    ~H"""
    <.dropdown_menu
      trigger={~H[<.button variant={:outline}>Menu<.icon name="chevron-down" /></.button>]}
      options={[
        %{id: "new", label: "New", icon: "plus"},
        %{id: "edit", label: "Edit", icon: "pencil"},
        %{id: "delete", label: "Delete", icon: "trash", variant: :destructive}
      ]}
    />
    """
  end

  def nested_submenus(assigns) do
    ~H"""
    <.dropdown_menu
      trigger={~H[<.button variant={:outline}>More<.icon name="chevron-down" /></.button>]}
      options={[
        %{id: "new", label: "New", icon: "plus"},
        %{
          id: "export",
          label: "Export",
          icon: "arrow-up-tray",
          children: [
            %{id: "csv", label: "CSV", icon: "document-arrow-up"},
            %{id: "pdf", label: "PDF", icon: "document-arrow-up"},
            %{
              id: "advanced",
              label: "Advanced",
              icon: "wrench",
              children: [
                %{id: "json", label: "JSON", icon: "code-bracket"},
                %{id: "xml", label: "XML", icon: "code-bracket"}
              ]
            }
          ]
        },
        %{id: "settings", label: "Settings", icon: "cog-6-tooth"}
      ]}
    />
    """
  end

  def keyboard_navigation(assigns) do
    ~H"""
    <.dropdown_menu
      trigger={~H[<.button variant={:outline}>Keyboard<.icon name="chevron-down" /></.button>]}
      options={[
        %{id: "up", label: "Arrow Up", description: "Navigate up"},
        %{id: "down", label: "Arrow Down", description: "Navigate down"},
        %{id: "enter", label: "Enter", description: "Select item"},
        %{id: "escape", label: "Escape", description: "Close menu"},
        %{id: "typeahead", label: "Typeahead", description: "Quick search"}
      ]}
    />
    """
  end

  def typeahead_demo(assigns) do
    ~H"""
    <.dropdown_menu
      trigger={~H[<.button variant={:outline}>Typeahead<.icon name="chevron-down" /></.button>]}
      options={[
        %{id: "apple", label: "Apple"},
        %{id: "banana", label: "Banana"},
        %{id: "cherry", label: "Cherry"},
        %{id: "date", label: "Date"},
        %{id: "elderberry", label: "Elderberry"},
        %{id: "fig", label: "Fig"},
        %{id: "grape", label: "Grape"}
      ]}
    />
    """
  end

  def app_shell_integration(assigns) do
    ~H"""
    <.app_shell>
      <:header>
        <.header>
          <:actions>
            <.dropdown_menu
              trigger={~H[<.icon_button icon="bars-3" aria_label="Navigation" />]}
              options={[
                %{id: "dashboard", label: "Dashboard", icon: "home"},
                %{id: "reservations", label: "Reservations", icon: "calendar-days"},
                %{id: "inventory", label: "Inventory", icon: "archive-box"},
                %{id: "people", label: "People", icon: "users"}
              ]}
            />
          </:actions>
        </.header>
      </:header>
      <:content>
        <div class="p-6">Content area</div>
      </:content>
    </.app_shell>
    """
  end

  def mobile_viewport(assigns) do
    ~H"""
    <div class="bg-background p-4 w-screen h-screen">
      <.dropdown_menu
        trigger={~H[<.button class="w-full">Mobile Menu<.icon name="chevron-down" /></.button>]}
        options={[
          %{id: "profile", label: "Profile", icon: "user-circle"},
          %{id: "notifications", label: "Notifications", icon: "bell"},
          %{id: "settings", label: "Settings", icon: "cog-6-tooth"},
          %{id: "logout", label: "Logout", icon: "arrow-left-on-rectangle", variant: :destructive}
        ]}
      />
    </div>
    """
  end
end
