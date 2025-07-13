defmodule RivaAshWeb.Components.Templates.CalendarTemplate do
  @moduledoc """
  Calendar page template with view controls and filters.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.CalendarView
  import RivaAshWeb.Components.Molecules.FilterPanel
  import RivaAshWeb.Components.Molecules.TabNavigation

  @doc """
  Renders a calendar template.
  """
  attr :title, :string, default: "Calendar"
  attr :description, :string, default: nil
  attr :current_date, :string, required: true
  attr :view_mode, :string, default: "month"
  attr :events, :list, default: []
  attr :filters, :list, default: []
  attr :filter_values, :map, default: %{}
  attr :on_date_click, :string, required: true
  attr :on_event_click, :string, default: nil
  attr :on_view_change, :string, required: true
  attr :on_navigate, :string, required: true
  attr :show_filters, :boolean, default: true
  attr :class, :string, default: ""
  attr :rest, :global

  slot :actions, required: false
  slot :sidebar_content, required: false

  def calendar_template(assigns) do
    ~H"""
    <!-- Calendar template implementation will go here -->
    <div {@rest} class={["calendar-template", @class]}>
      <.page_header title={@title} description={@description}>
        <:action :for={action <- @actions}>
          <%= render_slot(action) %>
        </:action>
      </.page_header>
      
      <div class="calendar-controls">
        <.tab_navigation
          tabs={[
            %{id: "day", label: "Day"},
            %{id: "week", label: "Week"},
            %{id: "month", label: "Month"}
          ]}
          active_tab={@view_mode}
          on_tab_change={@on_view_change}
        />
        
        <div :if={@show_filters && @filters != []} class="calendar-filters">
          <.filter_panel
            filters={@filters}
            values={@filter_values}
            on_apply="apply_filters"
            on_clear="clear_filters"
            collapsible={true}
          />
        </div>
      </div>
      
      <div class="calendar-layout">
        <main class="calendar-main">
          <.calendar_view
            events={@events}
            current_date={@current_date}
            view_mode={@view_mode}
            on_date_click={@on_date_click}
            on_event_click={@on_event_click}
            on_view_change={@on_view_change}
            on_navigate={@on_navigate}
            editable={true}
          />
        </main>
        
        <aside :if={@sidebar_content != []} class="calendar-sidebar">
          <%= render_slot(@sidebar_content) %>
        </aside>
      </div>
    </div>
    """
  end
end
