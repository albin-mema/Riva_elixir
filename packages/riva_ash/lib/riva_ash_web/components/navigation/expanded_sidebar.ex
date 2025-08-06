defmodule RivaAshWeb.Components.Navigation.ExpandedSidebar do
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
            label="Dashboard"
            collapsed={@collapsed}
          />
        </div>

        <!-- Core Management -->
        <div class="nav-section">
          <.nav_section_header label="Management" collapsed={@collapsed} />

          <.nav_item
            path="/plots"
            current_path={@current_path}
            icon={:map}
            label="Plots"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/sections"
            current_path={@current_path}
            icon={:squares_2x2}
            label="Sections"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/items"
            current_path={@current_path}
            icon={:cube}
            label="Items"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/item-types"
            current_path={@current_path}
            icon={:tag}
            label="Item Types"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/layouts"
            current_path={@current_path}
            icon={:view_columns}
            label="Layouts"
            collapsed={@collapsed}
          />
        </div>

        <!-- Reservations -->
        <div class="nav-section">
          <.nav_section_header label="Reservations" collapsed={@collapsed} />

          <.nav_item
            path="/reservations"
            current_path={@current_path}
            icon={:calendar_days}
            label="Reservations"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/calendar"
            current_path={@current_path}
            icon={:calendar}
            label="Calendar"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/clients"
            current_path={@current_path}
            icon={:users}
            label="Clients"
            collapsed={@collapsed}
          />
        </div>

        <!-- Financial -->
        <div class="nav-section">
          <.nav_section_header label="Financial" collapsed={@collapsed} />

          <.nav_item
            path="/pricing"
            current_path={@current_path}
            icon={:currency_dollar}
            label="Pricing"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/payments"
            current_path={@current_path}
            icon={:credit_card}
            label="Payments"
            collapsed={@collapsed}
          />
        </div>

        <!-- Administration -->
        <div class="nav-section">
          <.nav_section_header label="Administration" collapsed={@collapsed} />

          <.nav_item
            path="/employees"
            current_path={@current_path}
            icon={:user_group}
            label="Employees"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/permissions"
            current_path={@current_path}
            icon={:shield_check}
            label="Permissions"
            collapsed={@collapsed}
          />

          <.nav_item
            path="/businesses"
            current_path={@current_path}
            icon={:building_office}
            label="Business"
            collapsed={@collapsed}
          />
        </div>
      </div>

      <div class={@footer_class}>
        <.nav_item
          path="/profile"
          current_path={@current_path}
          icon={:user_circle}
          label="Profile"
          collapsed={@collapsed}
        />

        <button phx-click="sign_out" class="nav-item sign-out">
          <.icon name={:arrow_right_on_rectangle} />
          <span :if={!@collapsed}>Sign Out</span>
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
  defp build_header_class(collapsed) do
    "sidebar-header"
  end

  # Helper function to build content classes
  @spec build_content_class(boolean()) :: String.t()
  defp build_content_class(collapsed) do
    "sidebar-content"
  end

  # Helper function to build footer classes
  @spec build_footer_class(boolean()) :: String.t()
  defp build_footer_class(collapsed) do
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
