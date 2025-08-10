defmodule RivaAsh.Components.UI.SidebarStories do
  use RivaAsh.Storybook

  import RivaAsh.Components.UI.Sidebar
  import RivaAsh.Components.UI.AppShell

  def default(assigns) do
    ~H"""
    <.app_shell
      class="min-h-screen"
      on_sidebar_collapse="collapse_sidebar"
    >
      <:header>
        <.link href="#" label="Logo" />
      </:header>
      <:sidebar>
        <.sidebar
          sections={[
            %{
              id: "dashboard",
              label: "Dashboard",
              links: [
                %{label: "Home", href: "#", badge: 3},
                %{label: "Analytics", href: "#", badge: 99}
              ]
            },
            %{
              id: "settings",
              label: "Settings",
              links: [
                %{label: "Profile", href: "#", active: true},
                %{label: "Notifications", href: "#"}
              ]
            }
          ]}
          active_section="settings"
          active_link="Profile"
          on_collapse="collapse_sidebar"
        />
      </:sidebar>
      <:content>
        <div id="app-content" class="p-4">Main content area</div>
      </:content>
    </.app_shell>
    """
  end

  def compact_density(assigns) do
    ~H"""
    <.app_shell
      class="min-h-screen"
      on_sidebar_collapse="collapse_sidebar"
    >
      <:header>
        <.link href="#" label="Logo" />
      </:header>
      <:sidebar>
        <.sidebar
          density="compact"
          sections={[
            %{
              id: "dashboard",
              label: "Dashboard",
              links: [
                %{label: "Home", href: "#", badge: 3},
                %{label: "Analytics", href: "#", badge: 99}
              ]
            }
          ]}
          active_section="dashboard"
          active_link="Home"
          on_collapse="collapse_sidebar"
        />
      </:sidebar>
      <:content>
        <div id="app-content" class="p-4">Main content area</div>
      </:content>
    </.app_shell>
    """
  end

  def mobile_collapsed(assigns) do
    ~H"""
    <.app_shell
      class="min-h-screen"
      on_sidebar_collapse="collapse_sidebar"
      sidebar_collapsed={true}
    >
      <:header>
        <.link href="#" label="Logo" />
      </:header>
      <:sidebar>
        <.sidebar
          sections={[
            %{id: "dashboard", label: "Dashboard", links: [%{label: "Home", href: "#"}]}
          ]}
          active_section="dashboard"
          active_link="Home"
          on_collapse="collapse_sidebar"
        />
      </:sidebar>
      <:content>
        <div id="app-content" class="p-4">Main content area</div>
      </:content>
    </.app_shell>
    """
  end

  def rtl_layout(assigns) do
    ~H"""
    <div dir="rtl" class="min-h-screen">
      <.sidebar
        sections={[
          %{
            id: "dashboard",
            label: "لوحة القيادة",
            links: [
              %{label: "الرئيسية", href: "#", badge: 3},
              %{label: "الإحصائيات", href: "#", badge: 99}
            ]
          }
        ]}
        active_section="dashboard"
        active_link="الرئيسية"
      />
    </div>
    """
  end
end
