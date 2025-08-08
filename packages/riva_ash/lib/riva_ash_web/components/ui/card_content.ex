alias RivaAshWeb.Components.UI, as: UI

defmodule RivaAshWeb.Components.UI.CardContent do
  @moduledoc """
  Implements a card content component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card content component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_content(assigns) when is_map(assigns) do
    assigns
    |> assign_content_class()
    |> render_content()
  end

  defp assign_content_class(assigns) do
    assign(assigns, :content_class, build_content_class(assigns))
  end

  defp render_content(assigns) do
    ~H"""
    <div class={@content_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp build_content_class(assigns) when is_map(assigns) do
    assigns
    |> Map.get(:class, "")
    |> prepend_base_class()
  end

  defp prepend_base_class(class) when is_binary(class) do
    base = "p-6 pt-0"
    [base, class] |> Enum.reject(&(&1 == "")) |> Enum.join(" ")
  end
end
