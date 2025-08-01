defmodule RivaAshWeb.Components.Atoms.Avatar do
  @moduledoc """
  Avatar component for users and businesses with fallbacks.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders an avatar with image or initials fallback.
  """
  attr(:src, :string, default: nil)
  attr(:alt, :string, default: "")
  attr(:initials, :string, default: nil)
  attr(:name, :string, default: nil)
  attr(:size, :string, default: "md", values: ~w(xs sm md lg xl 2xl))
  attr(:shape, :string, default: "circle", values: ~w(circle square rounded))
  attr(:status, :string, default: nil)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def avatar(assigns) do
    assigns = assign(assigns, :avatar_class, avatar_class(assigns))

    ~H"""
    <div class={@avatar_class} {@rest}>
      <%= if @src do %>
        <img src={@src} alt={@alt} class="w-full h-full object-cover" />
      <% else %>
        <%= if @initials do %>
          <span class={initials_class(assigns)}><%= @initials %></span>
        <% else %>
          <.icon name={:user} size={icon_size(@size)} class="text-muted-foreground" />
        <% end %>
      <% end %>

      <%= if @status do %>
        <div class={status_indicator_class(@status, @size)}></div>
      <% end %>
    </div>
    """
  end

  defp avatar_class(assigns) do
    base = "relative inline-flex items-center justify-center overflow-hidden bg-muted"
    size = size_classes(assigns.size)
    shape = shape_classes(assigns.shape)

    Enum.join([base, size, shape, assigns.class], " ")
  end

  defp size_classes(size) do
    case size do
      "xs" -> "h-6 w-6 text-xs"
      "sm" -> "h-8 w-8 text-sm"
      "md" -> "h-10 w-10 text-base"
      "lg" -> "h-12 w-12 text-lg"
      "xl" -> "h-16 w-16 text-xl"
      "2xl" -> "h-20 w-20 text-2xl"
      _ -> "h-10 w-10 text-base"
    end
  end

  defp shape_classes(shape) do
    case shape do
      "circle" -> "rounded-full"
      "square" -> "rounded-none"
      "rounded" -> "rounded-md"
      _ -> "rounded-full"
    end
  end

  defp initials_class(_assigns) do
    "font-medium text-foreground select-none"
  end

  defp status_indicator_class(status, size) do
    base = "absolute border-2 border-background rounded-full"
    position = status_position(size)
    color = status_color(status)

    Enum.join([base, position, color], " ")
  end

  defp status_position(size) do
    case size do
      "xs" -> "bottom-0 right-0 h-2 w-2"
      "sm" -> "bottom-0 right-0 h-2.5 w-2.5"
      "md" -> "bottom-0 right-0 h-3 w-3"
      "lg" -> "bottom-0 right-0 h-3.5 w-3.5"
      "xl" -> "bottom-0 right-0 h-4 w-4"
      "2xl" -> "bottom-0 right-0 h-5 w-5"
      _ -> "bottom-0 right-0 h-3 w-3"
    end
  end

  defp status_color(status) do
    case status do
      "online" -> "bg-green-500"
      "away" -> "bg-yellow-500"
      "busy" -> "bg-red-500"
      "offline" -> "bg-gray-400"
      _ -> "bg-gray-400"
    end
  end

  defp icon_size(size) do
    case size do
      "xs" -> "xs"
      "sm" -> "sm"
      "md" -> "md"
      "lg" -> "lg"
      "xl" -> "xl"
      "2xl" -> "xl"
      _ -> "md"
    end
  end
end
