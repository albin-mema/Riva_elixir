defmodule RivaAsh.Components.UI.Tooltip do
  @moduledoc """
  Accessible tooltip component with multiple positioning options and collision detection.
  Follows atomic design principles with proper focus states and ARIA attributes.
  """

  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms
  alias RivaAsh.Components.Helpers, as: H

  @directions [:top, :bottom, :left, :right]
  @default_delay 200

  @doc """
  Renders a tooltip component that appears on hover/focus.

  ## Props

    * `text` - The text content of the tooltip
    * `direction` - Position relative to target: :top, :bottom, :left, :right
    * `delay` - Delay in milliseconds before showing/hiding (default: 200)
    * `disabled` - Whether the tooltip is disabled
    * `rest` - Additional HTML attributes

  ## Examples

      <.tooltip text="Help text">
        <button>Hover me</button>
      </.tooltip>
  """
  def tooltip(assigns) do
    assigns =
      assigns
      |> standard_assigns()
      |> assign_new(:direction, fn -> validate_direction(assigns[:direction] || :top) end)
      |> assign_new(:delay, fn -> validate_delay(assigns[:delay] || @default_delay) end)
      |> assign_new(:text, fn -> assigns[:text] || "" end)
      |> assign_new(:id, fn -> "tooltip-#{System.unique_integer([:positive])}" end)
      |> assign_new(:aria_attributes, fn ->
        Map.merge(
          %{"aria-describedby" => "#{@id}-content"},
          Atoms.aria_attributes(assigns)
        )
      end)

    ~H"""
    <div
      class="inline-block relative"
      phx-hook="Tooltip"
      data-direction={@direction}
      data-delay={@delay}
      data-id={@id}
      {@aria_attributes}
    >
      <%= render_slot(@inner_block) %>
      <div
        id={"#{@id}-content"}
        class={[
          "invisible absolute z-50 rounded-md px-2 py-1 text-sm font-medium",
          "bg-popover text-popover-foreground shadow-md",
          "opacity-0",
          "pointer-events-none",
          "transition-all",
          "duration-#{@delay |> Integer.to_string()}",
          "ease-in-out"
        ]}
        role="tooltip"
        aria-hidden="true"
      >
        <%= @text %>
      </div>
    </div>
    """
  end

  defp validate_direction(direction) when direction in @directions, do: direction
  defp validate_direction(_), do: :top

  defp validate_delay(delay) when is_integer(delay) and delay >= 0, do: delay
  defp validate_delay(_), do: @default_delay
end
