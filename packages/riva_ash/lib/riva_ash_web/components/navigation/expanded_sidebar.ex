# alias RivaAshWeb.Components.Navigation, as: Navigation
# alias RivaAshWeb.Components.Atoms, as: Atoms
# alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Navigation.ExpandedSidebar do
  import RivaAshWeb.Gettext, only: [dgettext: 2]

  @moduledoc """
  Expanded sidebar navigation with all resources.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon
  # removed unused import to reduce warnings

  @doc """
  Renders an expanded sidebar navigation.
  """
  attr(:current_user, :map, required: true)
  attr(:current_path, :string, required: true)
  attr(:collapsed, :boolean, default: false)
  attr(:on_toggle, :string, default: nil)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec expanded_sidebar(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def expanded_sidebar(assigns) do
    # Render expanded sidebar using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class, assigns.collapsed))
    |> Map.put_new(:header_class, build_header_class(assigns.collapsed))
    |> Map.put_new(:content_class, build_content_class(assigns.collapsed))
    |> Map.put_new(:footer_class, build_footer_class(assigns.collapsed))
    |> render_expanded_sidebar_component()
  end

  # Private helper for expanded sidebar rendering
  @spec render_expanded_sidebar_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_expanded_sidebar_component(assigns) do
    ~H"""
    <!-- Expanded sidebar implementation will go here -->
    <nav {@rest} class={@container_class}>
      <div class={@header_class}>
        <div :if={!@collapsed}>
          <h2>Riva Ash</h2>
          <p><%= @current_user.business.name %></p>
        </div>
        <button :if={@on_toggle} phx-click={@on_toggle}>
          <.icon name={if @collapsed, do: :chevron_right, else: :chevron_left} />
        </button>
      </div>

      <div class={@content_class}>
        <!-- Dashboard -->
        <div class="nav-section">
          <.nav_item
            path="/dashboard"
            current_path={@current_path}
            icon={:home}
            label={dgettext("navigation", "Dashboard")}
            collapsed={@collapsed}
          />
        </div>

        <!-- Core Management -->
        <div class="nav-section">
          <.nav_section_header label={dgettext("navigation", "Management")} collapsed={@collapsed} />

          <.nav_item
            path="/plots"
            current_path={@current_path}
            icon={:map}
            label={dgettext("navigation", "Plots")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/sections"
            current_path={@current_path}
            icon={:squares_2x2}
            label={dgettext("navigation", "Sections")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/items"
            current_path={@current_path}
            icon={:cube}
            label={dgettext("navigation", "Items")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/item-types"
            current_path={@current_path}
            icon={:tag}
            label={dgettext("navigation", "Item Types")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/layouts"
            current_path={@current_path}
            icon={:view_columns}
            label={dgettext("navigation", "Layouts")}
            collapsed={@collapsed}
          />
        </div>

        <!-- Reservations -->
        <div class="nav-section">
          <.nav_section_header label={dgettext("navigation", "Reservations")} collapsed={@collapsed} />

          <.nav_item
            path="/reservations"
            current_path={@current_path}
            icon={:calendar_days}
            label={dgettext("navigation", "Reservations")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/calendar"
            current_path={@current_path}
            icon={:calendar}
            label={dgettext("navigation", "Calendar")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/clients"
            current_path={@current_path}
            icon={:users}
            label={dgettext("navigation", "Clients")}
            collapsed={@collapsed}
          />
        </div>

        <!-- Financial -->
        <div class="nav-section">
          <.nav_section_header label={dgettext("navigation", "Financial")} collapsed={@collapsed} />

          <.nav_item
            path="/pricing"
            current_path={@current_path}
            icon={:currency_dollar}
            label={dgettext("navigation", "Pricing")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/payments"
            current_path={@current_path}
            icon={:credit_card}
            label={dgettext("navigation", "Payments")}
            collapsed={@collapsed}
          />
        </div>

        <!-- Administration -->
        <div class="nav-section">
          <.nav_section_header label={dgettext("navigation", "Administration")} collapsed={@collapsed} />

          <.nav_item
            path="/employees"
            current_path={@current_path}
            icon={:user_group}
            label={dgettext("navigation", "Employees")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/permissions"
            current_path={@current_path}
            icon={:shield_check}
            label={dgettext("navigation", "Permissions")}
            collapsed={@collapsed}
          />

          <.nav_item
            path="/businesses"
            current_path={@current_path}
            icon={:building_office}
            label={dgettext("navigation", "Business")}
            collapsed={@collapsed}
          />
        </div>
      </div>

      <div class={@footer_class}>
        <.nav_item
          path="/profile"
          current_path={@current_path}
          icon={:user_circle}
          label={dgettext("navigation", "Profile")}
          collapsed={@collapsed}
        />

        <button phx-click="sign_out" class="nav-item sign-out">
          <.icon name={:arrow_right_on_rectangle} />
          <span :if={!@collapsed}><%= dgettext("navigation", "Sign Out") %></span>
        </button>
      </div>
    </nav>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t(), boolean()) :: String.t()
  defp build_container_class(class, collapsed) do
    ["sidebar", if(collapsed, do: "collapsed", else: "expanded"), class]
    |> Enum.filter(&(&1 != ""))
    |> Enum.join(" ")
  end

  # Helper function to build header classes
  @spec build_header_class(boolean()) :: String.t()
  defp build_header_class(_collapsed) do
    "sidebar-header"
  end

  # Helper function to build content classes
  @spec build_content_class(boolean()) :: String.t()
  defp build_content_class(_collapsed) do
    "sidebar-content"
  end

  # Helper function to build footer classes
  @spec build_footer_class(boolean()) :: String.t()
  defp build_footer_class(_collapsed) do
    "sidebar-footer"
  end

  defp nav_section_header(assigns) do
    ~H"""
    <div :if={!@collapsed} class="nav-section-header">
      <span><%= @label %></span>
    </div>
    """
  end

  defp nav_item(assigns) do
    ~H"""
    <a
      href={@path}
      class={[
        "nav-item",
        if(@current_path == @path, do: "active", else: "")
      ]}
    >
      <.icon name={@icon} />
      <span :if={!@collapsed}><%= @label %></span>
    </a>
    """
  end
end
