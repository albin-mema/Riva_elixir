alias RivaAshWeb.Components.UI, as: UI

defmodule RivaAshWeb.Components.UI.CardDescription do
  @moduledoc """
  Implements a card description component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card description component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_description(assigns) when is_map(assigns) do
    assigns
    |> assign_description_class()
    |> render_description()
  end

  defp assign_description_class(assigns) do
    assign(assigns, :description_class, build_description_class(assigns))
  end

  defp render_description(assigns) do
    ~H"""
    <p class={@description_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </p>
    """
  end

  defp build_description_class(assigns) when is_map(assigns) do
    assigns
    |> Map.get(:class, "")
    |> prepend_base_class()
  end

  defp prepend_base_class(class) when is_binary(class) do
    base = "text-sm text-muted-foreground"
    [base, class] |> Enum.reject(&(&1 == "")) |> Enum.join(" ")
  end
end
