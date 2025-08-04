defmodule RivaAshWeb.Components.Molecules.ActionMenu do
  @moduledoc """
  Dropdown action menu component.
  """
  use Phoenix.Component
  alias Phoenix.LiveView.JS
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon

  @doc """
  Renders a dropdown action menu.
  """
  attr(:trigger_label, :string, default: "Actions")
  attr(:trigger_icon, :atom, default: :ellipsis_vertical)
  attr(:actions, :list, required: true)

  attr(:position, :string,
    default: "bottom-right",
    values: ~w(bottom-left bottom-right top-left top-right)
  )

  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def action_menu(assigns) do
    ~H"""
    <div class={["relative inline-block text-left", @class]} {@rest}>
      <UIButton.button
        variant="ghost"
        size={map_legacy_size(@size)}
        class="flex items-center gap-2"
        phx-click={JS.toggle(to: "#action-menu-#{@trigger_label |> String.replace(" ", "-") |> String.downcase}")}
      >
        <%= if @trigger_icon do %>
          <UIIcon.icon name={@trigger_icon} size="sm" />
        <% end %>
        <%= @trigger_label %>
      </UIButton.button>

      <div
        id={"action-menu-#{@trigger_label |> String.replace(" ", "-") |> String.downcase}"}
        class={[
          "absolute z-50 mt-2 w-56 rounded-md border bg-popover p-1 text-popover-foreground shadow-md hidden",
          position_classes(@position)
        ]}
      >
        <%= for action <- @actions do %>
          <button
            type="button"
            class="relative flex w-full cursor-default select-none items-center rounded-sm px-2 py-1.5 text-sm outline-none hover:bg-accent hover:text-accent-foreground focus:bg-accent focus:text-accent-foreground disabled:pointer-events-none disabled:opacity-50"
            phx-click={action[:phx_click]}
            disabled={action[:disabled] || false}
          >
            <%= if action[:icon] do %>
              <UIIcon.icon name={action.icon} size="sm" class="mr-2" />
            <% end %>
            <%= action.label %>
          </button>
        <% end %>
      </div>
    </div>
    """
  end

  defp map_legacy_size("sm"), do: "sm"
  defp map_legacy_size("md"), do: "default"
  defp map_legacy_size("lg"), do: "lg"
  defp map_legacy_size(_), do: "default"

  defp position_classes(position) do
    case position do
      "bottom-left" -> "left-0 top-full"
      "bottom-right" -> "right-0 top-full"
      "top-left" -> "left-0 bottom-full mb-2"
      "top-right" -> "right-0 bottom-full mb-2"
      _ -> "right-0 top-full"
    end
  end
end
