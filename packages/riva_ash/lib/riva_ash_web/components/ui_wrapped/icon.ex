defmodule RivaAshWeb.Components.UIWrapped.Icon do
  @moduledoc """
  App-level Icon wrapper around SaladUI.Icon.
  
  Provides consistent icon handling with various sizes and colors.
  """
  use Phoenix.Component

  @doc """
  Renders an icon with specified name, size, and color.
  """
  attr :name, :atom, required: true, doc: "Icon name identifier"
  attr :size, :string,
    default: "default",
    values: ~w(xs sm default lg),
    doc: "Icon size variant"

  attr :color, :string,
    default: "default",
    values: ~w(default primary secondary destructive muted),
    doc: "Icon color variant"

  attr :class, :string, default: ""
  attr :rest, :global

  def icon(assigns) do
    assigns =
      assigns
      |> assign_new(:_icon_name, fn -> Atom.to_string(assigns.name) end)
      |> assign_new(:_icon_color, fn -> map_color(assigns.color) end)

    ~H"""
    <svg
      class={[
        case @size do
          "xs" -> "w-3 h-3"
          "sm" -> "w-4 h-4"
          "lg" -> "w-6 h-6"
          _ -> "w-5 h-5"
        end,
        case @color do
          "primary" -> "text-primary"
          "secondary" -> "text-secondary"
          "destructive" -> "text-destructive"
          "muted" -> "text-muted-foreground"
          _ -> "text-foreground"
        end,
        @class
      ]}
      fill="none"
      stroke="currentColor"
      viewBox="0 0 24 24"
      xmlns="http://www.w3.org/2000/svg"
      {@rest}
    >
      <%= render_icon_content(@_icon_name) %>
    </svg>
    """
  end

  # Map our stable API to SaladUI expected props
  defp size_to_salad("default"), do: "default"
  defp size_to_salad(s) when s in ["xs", "sm", "lg"], do: s
  defp size_to_salad(_), do: "default"

  defp map_color("default"), do: "default"
  defp map_color(v) when v in ["primary", "secondary", "destructive", "muted"], do: v
  defp map_color(_), do: "default"

  # Render SVG path content for different icons
  defp render_icon_content("home") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6" />)
  end

  defp render_icon_content("search") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />)
  end

  defp render_icon_content("cog_6_tooth") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" /><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />)
  end

  defp render_icon_content("user") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />)
  end

  defp render_icon_content("calendar") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z" />)
  end

  defp render_icon_content("chevron_left") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 19l-7-7 7-7" />)
  end

  defp render_icon_content("chevron_right") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7" />)
  end

  defp render_icon_content("chevron_up") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 15l7-7 7 7" />)
  end

  defp render_icon_content("chevron_down") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />)
  end

  defp render_icon_content("plus") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4" />)
  end

  defp render_icon_content("minus") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 12H4" />)
  end

  defp render_icon_content("check") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />)
  end

  defp render_icon_content("x_mark") do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />)
  end

  defp render_icon_content(_other) do
    ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />)
  end
end