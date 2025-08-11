defmodule RivaAshWeb.Components.UIWrapped.Container do
  @moduledoc """
  App-level Container wrapper around SaladUI.Container.
  
  Provides responsive container layouts with consistent spacing.
  """
  use Phoenix.Component

  @doc """
  Renders a responsive container component.
  """
  attr :size, :string,
    default: "default",
    values: ~w(default sm lg xl full),
    doc: "Container size variant"

  attr :fluid, :boolean, default: false, doc: "Whether the container should be full width"
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def container(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_size, fn -> size_to_salad(assigns.size) end)

    ~H"""
    <SaladUI.Container.container
      size={@_salad_size}
      fluid={@fluid}
      class={[
        case @size do
          "sm" -> "max-w-screen-sm"
          "lg" -> "max-w-screen-lg"
          "xl" -> "max-w-screen-xl"
          "full" -> "max-w-full"
          _ -> "max-w-screen-md"
        end,
        @fluid && "w-full",
        @class
      ]}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </SaladUI.Container.container>
    """
  end

  # Map our stable API to SaladUI expected props
  defp size_to_salad("default"), do: "default"
  defp size_to_salad(s) when s in ["sm", "lg", "xl", "full"], do: s
  defp size_to_salad(_), do: "default"
end