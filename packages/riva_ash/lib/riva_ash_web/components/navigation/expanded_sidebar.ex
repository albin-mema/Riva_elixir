defmodule RivaAshWeb.Components.Navigation.ExpandedSidebar do
  @moduledoc """
  Expanded sidebar navigation with all resources.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon
  import RivaAshWeb.Components.Atoms.Badge

  @doc """
  Renders an expanded sidebar navigation.
  """
  attr :current_user, :map, required: true
  attr :current_path, :string, required: true
  attr :collapsed, :boolean, default: false
  attr :on_toggle, :string, default: nil
  attr :class, :string, default: ""
  attr :rest, :global

  def expanded_sidebar(assigns) do
    ~H"""
    <!-- Expanded sidebar implementation will go here -->
    <nav {@rest} class={["sidebar", if(@collapsed, do: "collapsed", else: "expanded"), @class]}>
      <div class="sidebar-header">
        <div :if={!@collapsed}>
          <h2>Riva Ash</h2>
          <p><%= @current_user.business.name %></p>
        </div>
        <button :if={@on_toggle} phx-click={@on_toggle}>
          <.icon name={if @collapsed, do: :chevron_right, else: :chevron_left} />
        </button>
      </div>

      <div class="sidebar-content">
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

      <div class="sidebar-footer">
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
