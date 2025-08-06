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
  attr(:actions, :list, default: [])
  attr(:layout, :string, default: "horizontal", values: ~w(horizontal vertical grid))
  attr(:show_labels, :boolean, default: true)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec quick_actions(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def quick_actions(assigns) do
    # Render quick actions using functional composition
    assigns
    |> Map.put_new(:default_actions, get_default_actions())
    |> Map.put_new(:container_class, build_container_class(assigns.class, assigns.layout))
    |> Map.put_new(:default_actions_class, build_default_actions_class(assigns.actions))
    |> Map.put_new(:custom_actions_class, build_custom_actions_class(assigns.actions))
    |> render_quick_actions_component()
  end

  # Private helper for quick actions rendering
  @spec render_quick_actions_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_quick_actions_component(assigns) do
    ~H"""
    <!-- Quick actions implementation will go here -->
    <div {@rest} class={@container_class}>
      <div :if={@actions == []} class={@default_actions_class}>
        <.action_button
          :for={action <- @default_actions}
          action={action}
          show_label={@show_labels}
          size={@size}
        />
      </div>

      <div :if={@actions != []} class={@custom_actions_class}>
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

  # Helper function to build container classes
  @spec build_container_class(String.t(), String.t()) :: String.t()
  defp build_container_class(class, layout) do
    ["quick-actions", "layout-#{layout}", class]
    |> Enum.filter(&(&1 != ""))
    |> Enum.join(" ")
  end

  # Helper function to build default actions classes
  @spec build_default_actions_class(list()) :: String.t()
  defp build_default_actions_class(actions) do
    if actions == [], do: "default-actions", else: "hidden"
  end

  # Helper function to build custom actions classes
  @spec build_custom_actions_class(list()) :: String.t()
  defp build_custom_actions_class(actions) do
    if actions != [], do: "custom-actions", else: "hidden"
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
