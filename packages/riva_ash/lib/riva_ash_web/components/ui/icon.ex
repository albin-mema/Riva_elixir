defmodule RivaAshWeb.Components.UI.Icon do
  @moduledoc """
  Implements an icon component using the design system.

  A canonical icon component that provides consistent sizing and styling
  for SVG icons across the application.
  """
  use Phoenix.Component

  @doc """
  Renders an icon component using the design system.

  Supports heroicons and custom SVG icons with consistent sizing.
  """
  attr :name, :atom, required: true, doc: "The icon name"
  attr :variant, :string, default: "outline", values: ~w(outline solid mini micro), doc: "The icon variant"
  attr :size, :string, default: "default", values: ~w(default xs sm lg xl), doc: "The icon size"
  attr :class, :string, default: "", doc: "Additional CSS classes"
  attr :rest, :global, doc: "Any additional HTML attributes"

  @spec icon(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def icon(assigns) do
    # Render icon using functional composition
    assigns
    |> Map.put_new(:icon_class, icon_class(assigns))
    |> Map.put_new(:svg_class, build_svg_class(assigns.size))
    |> Map.put_new(:container_class, build_container_class(assigns.size))
    |> render_icon_component()
  end

  # Private helper for icon rendering
  @spec render_icon_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_icon_component(assigns) do
    ~H"""
    <span class={@icon_class} {@rest}>
      <svg class={@svg_class} fill="none" viewBox="0 0 24 24" stroke="currentColor">
        <%= render_icon_path(@name) %>
      </svg>
    </span>
    """
  end

  # Helper function to build SVG classes
  @spec build_svg_class(String.t()) :: String.t()
  defp build_svg_class(size) do
    case size do
      "xs" -> "w-3 h-3"
      "sm" -> "w-4 h-4"
      "lg" -> "w-6 h-6"
      "xl" -> "w-8 h-8"
      _ -> "w-5 h-5"
    end
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(size) do
    case size do
      "xs" -> "inline-block align-middle"
      "sm" -> "inline-block align-middle"
      "lg" -> "inline-block align-middle"
      "xl" -> "inline-block align-middle"
      _ -> "inline-block align-middle"
    end
  end

  defp render_icon_path(name) do
    icon_paths()
    |> Map.get(name, &default_icon_path/0)
    |> Phoenix.HTML.raw()
  end

  # Private map of icon names to their SVG paths
  defp icon_paths do
    %{
      building_office_2: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4" />),
      plus: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4" />),
      x_mark: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />),
      check: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />),
      home: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6" />),
      cog: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" /><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />),
      magnifying_glass: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />),
      user: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />),
      ellipsis_vertical: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 5v.01M12 12v.01M12 19v.01" />),
      chevron_left: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 19l-7-7 7-7" />),
      chevron_right: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7" />),
      chevron_up: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 15l-7-7-7 7" />),
      chevron_down: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 9l7 7 7-7" />),
      clock: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />),
      information_circle: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />),
      circle: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />)
    }
  end

  # Default icon path for unknown icons
  defp default_icon_path, do: ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />)

  defp icon_class(assigns) do
    base = "inline-block"
    size = size_classes(assigns.size)

    Enum.join([base, size, assigns.class], " ")
  end

  defp size_classes(size) do
    case size do
      "xs" -> "h-3 w-3"
      "sm" -> "h-4 w-4"
      "lg" -> "h-6 w-6"
      "xl" -> "h-8 w-8"
      # default
      _ -> "h-5 w-5"
    end
  end
end
