defmodule RivaAshWeb.Components.UI.CardHeader do
  @moduledoc """
  Implements a card header component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card header component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_header(assigns) when is_map(assigns) do
    assigns
    |> assign_header_class()
    |> render_header()
  end

  defp assign_header_class(assigns) do
    assign(assigns, :header_class, build_header_class(assigns))
  end

  defp render_header(assigns) do
    ~H"""
    <div class={@header_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp build_header_class(assigns) when is_map(assigns) do
    assigns
    |> Map.get(:class, "")
    |> prepend_base_class()
  end

  defp prepend_base_class(class) when is_binary(class) do
    base = "flex flex-col space-y-1.5 p-6"
    [base, class] |> Enum.reject(&(&1 == "")) |> Enum.join(" ")
  end
end
