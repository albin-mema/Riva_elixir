alias RivaAshWeb.Components.UI, as: UI

defmodule RivaAshWeb.Components.UI.CardFooter do
  @moduledoc """
  Implements a card footer component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card footer component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_footer(assigns) when is_map(assigns) do
    assigns
    |> assign_footer_class()
    |> render_footer()
  end

  defp assign_footer_class(assigns) do
    assign(assigns, :footer_class, build_footer_class(assigns))
  end

  defp render_footer(assigns) do
    ~H"""
    <div class={@footer_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp build_footer_class(assigns) when is_map(assigns) do
    assigns
    |> Map.get(:class, "")
    |> prepend_base_class()
  end

  defp prepend_base_class(class) when is_binary(class) do
    base = "flex items-center p-6 pt-0"
    [base, class] |> Enum.reject(&(&1 == "")) |> Enum.join(" ")
  end
end
