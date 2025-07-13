defmodule RivaAshWeb.Components.Navigation.QuickActions do
  @moduledoc """
  Quick action shortcuts component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders quick action shortcuts.
  """
  attr :actions, :list, default: []
  attr :layout, :string, default: "horizontal", values: ~w(horizontal vertical grid)
  attr :show_labels, :boolean, default: true
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :class, :string, default: ""
  attr :rest, :global

  def quick_actions(assigns) do
    assigns = assign(assigns, :default_actions, get_default_actions())
    
    ~H"""
    <!-- Quick actions implementation will go here -->
    <div {@rest} class={["quick-actions", "layout-#{@layout}", @class]}>
      <div :if={@actions == []} class="default-actions">
        <.action_button
          :for={action <- @default_actions}
          action={action}
          show_label={@show_labels}
          size={@size}
        />
      </div>
      
      <div :if={@actions != []} class="custom-actions">
        <.action_button
          :for={action <- @actions}
          action={action}
          show_label={@show_labels}
          size={@size}
        />
      </div>
    </div>
    """
  end

  defp action_button(assigns) do
    ~H"""
    <.button
      variant={@action[:variant] || "outline"}
      size={@size}
      phx-click={@action[:event]}
      phx-value-target={@action[:target]}
      class="quick-action-btn"
    >
      <.icon :if={@action[:icon]} name={@action[:icon]} />
      <span :if={@show_label}><%= @action[:label] %></span>
    </.button>
    """
  end

  defp get_default_actions do
    [
      %{
        label: "New Reservation",
        icon: :plus,
        event: "new_reservation",
        variant: "primary"
      },
      %{
        label: "Add Client",
        icon: :user_plus,
        event: "new_client",
        variant: "outline"
      },
      %{
        label: "View Calendar",
        icon: :calendar,
        event: "view_calendar",
        variant: "outline"
      },
      %{
        label: "Quick Payment",
        icon: :credit_card,
        event: "quick_payment",
        variant: "outline"
      },
      %{
        label: "Reports",
        icon: :chart_bar,
        event: "view_reports",
        variant: "outline"
      }
    ]
  end
end
