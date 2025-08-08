alias RivaAshWeb.Components.UI, as: UI

defmodule RivaAshWeb.Components.UI.CardTitle do
  @moduledoc """
  Implements a card title component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card title component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_title(assigns) when is_map(assigns) do
    assigns
    |> assign_title_class()
    |> render_title()
  end

  defp assign_title_class(assigns) do
    assign(assigns, :title_class, build_title_class(assigns))
  end

  defp render_title(assigns) do
    ~H"""
    <h3 class={@title_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </h3>
    """
  end

  defp build_title_class(assigns) when is_map(assigns) do
    assigns
    |> Map.get(:class, "")
    |> prepend_base_class()
  end

  defp prepend_base_class(class) when is_binary(class) do
    base = "text-lg font-semibold leading-none tracking-tight"
    [base, class] |> Enum.reject(&(&1 == "")) |> Enum.join(" ")
  end
end
