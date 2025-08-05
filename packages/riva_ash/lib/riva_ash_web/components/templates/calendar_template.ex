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
  attr(:title, :string, default: "Calendar")
  attr(:description, :string, default: nil)
  attr(:current_date, :string, required: true)
  attr(:view_mode, :string, default: "month")
  attr(:events, :list, default: [])
  attr(:filters, :list, default: [])
  attr(:filter_values, :map, default: %{})
  attr(:on_date_click, :string, required: true)
  attr(:on_event_click, :string, default: nil)
  attr(:on_view_change, :string, required: true)
  attr(:on_navigate, :string, required: true)
  attr(:show_filters, :boolean, default: true)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:actions, required: false)
  slot(:sidebar_content, required: false)

  @spec calendar_template(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def calendar_template(assigns) do
    # Render calendar template using functional composition
    assigns
    |> Map.put_new(:template_class, build_template_class(assigns.class, assigns.variant))
    |> Map.put_new(:header_class, build_header_class(assigns.actions))
    |> Map.put_new(:title_class, build_title_class(assigns.title))
    |> Map.put_new(:actions_class, build_actions_class(assigns.actions))
    |> Map.put_new(:description_class, build_description_class(assigns.description))
    |> Map.put_new(:controls_class, build_controls_class(assigns.filters))
    |> Map.put_new(:tab_navigation_class, build_tab_navigation_class(assigns.view_mode))
    |> Map.put_new(:filters_class, build_filters_class(assigns.show_filters, assigns.filters))
    |> Map.put_new(:layout_class, build_layout_class(assigns.sidebar_content))
    |> Map.put_new(:main_class, build_main_class(assigns.events))
    |> Map.put_new(:sidebar_class, build_sidebar_class(assigns.sidebar_content))
    |> render_calendar_template_component()
  end

  # Private helper for calendar template rendering
  @spec render_calendar_template_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_calendar_template_component(assigns) do
    ~H"""
    <div class={@template_class} {@rest}>
      <.page_header title={@title} description={@description} class={@header_class}>
        <:action :for={action <- @actions}>
          <%= render_slot(action) %>
        </:action>
      </.page_header>
      
      <div class={@controls_class}>
        <.tab_navigation
          tabs={[
            %{id: "day", label: "Day"},
            %{id: "week", label: "Week"},
            %{id: "month", label: "Month"}
          ]}
          active_tab={@view_mode}
          on_tab_change={@on_view_change}
          class={@tab_navigation_class}
        />
        
        <div :if={@show_filters && @filters != []} class={@filters_class}>
          <.filter_panel
            filters={@filters}
            values={@filter_values}
            on_apply="apply_filters"
            on_clear="clear_filters"
            collapsible={true}
          />
        </div>
      </div>
      
      <div class={@layout_class}>
        <main class={@main_class}>
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
        
        <aside :if={@sidebar_content != []} class={@sidebar_class}>
          <%= render_slot(@sidebar_content) %>
        </aside>
      </div>
    </div>
    """
  end

  # Helper function to build template classes
  @spec build_template_class(String.t(), String.t()) :: String.t()
  defp build_template_class(class, variant) do
    base =
      case variant do
        "compact" -> "space-y-4"
        "card" -> "bg-card rounded-lg p-6 shadow-sm space-y-6"
        _ -> "space-y-6"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build header classes
  @spec build_header_class(list()) :: String.t()
  defp build_header_class(actions) do
    if actions != [], do: "mb-6", else: "mb-4"
  end

  # Helper function to build title classes
  @spec build_title_class(String.t()) :: String.t()
  defp build_title_class(title) do
    if title, do: "text-2xl font-bold", else: "hidden"
  end

  # Helper function to build actions classes
  @spec build_actions_class(list()) :: String.t()
  defp build_actions_class(actions) do
    if actions != [], do: "flex gap-2", else: "hidden"
  end

  # Helper function to build description classes
  @spec build_description_class(String.t() | nil) :: String.t()
  defp build_description_class(description) do
    if description, do: "text-muted-foreground", else: "hidden"
  end

  # Helper function to build controls classes
  @spec build_controls_class(list()) :: String.t()
  defp build_controls_class(filters) do
    if filters != [], do: "calendar-controls space-y-4", else: "calendar-controls"
  end

  # Helper function to build tab navigation classes
  @spec build_tab_navigation_class(String.t()) :: String.t()
  defp build_tab_navigation_class(view_mode) do
    "w-full"
  end

  # Helper function to build filters classes
  @spec build_filters_class(boolean(), list()) :: String.t()
  defp build_filters_class(show_filters, filters) do
    if show_filters and filters != [], do: "calendar-filters", else: "hidden"
  end

  # Helper function to build layout classes
  @spec build_layout_class(list()) :: String.t()
  defp build_layout_class(sidebar_content) do
    "calendar-layout grid grid-cols-1 gap-6 lg:grid-cols-4"
  end

  # Helper function to build main classes
  @spec build_main_class(list()) :: String.t()
  defp build_main_class(events) do
    if events != [], do: "calendar-main lg:col-span-3", else: "calendar-main lg:col-span-4"
  end

  # Helper function to build sidebar classes
  @spec build_sidebar_class(list()) :: String.t()
  defp build_sidebar_class(sidebar_content) do
    if sidebar_content != [], do: "calendar-sidebar lg:col-span-1", else: "hidden"
  end
end
