defmodule RivaAsh.Components.UI.Link do
  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms

  @variants [:inline, :external, :muted]
  @sizes [:sm, :md, :lg]

  @doc """
  Standard link component with multiple variants and accessibility features.

  ## Variants
    - :inline - Default text link with underline on hover
    - :external - External link indicator (typically with icon)
    - :muted - Low emphasis link

  ## Props
    - variant: [:inline, :external, :muted] (default: :inline)
    - size: [:sm, :md, :lg] (default: :md)
    - disabled: boolean (default: false)
    - aria_label: string
    - icon: atom (optional) - icon name to display
    - icon_position: [:left, :right] (default: :left)
    - href: string (required)

  ## Example
      <.link href="#" variant={:inline}>Link</.link>
      <.link href="#" variant={:external} icon={:external} icon_position={:right}>External</.link>
  """
  def link(assigns) do
    assigns =
      assigns
      |> standard_assigns(variant: :inline)
      |> assign_new(:icon, fn -> nil end)
      |> assign_new(:icon_position, fn -> :left end)

    classes = [
      base_classes(),
      variant_classes(assigns.variant),
      size_classes(assigns.size)
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join(" ")

    aria_attrs = Atoms.aria_attributes(assigns)

    ~H"""
    <%= if @disabled do %>
      <span
        class={classes}
        role="link"
        {@aria_attrs}
        {@rest}
      >
        <%= if @icon && @icon_position == :left do %>
          <.icon name={@icon} class="inline-block mr-1 align-middle" />
        <% end %>
        <%= render_slot(@inner_block) %>
        <%= if @icon && @icon_position == :right do %>
          <.icon name={@icon} class="inline-block ml-1 align-middle" />
        <% end %>
      </span>
    <% else %>
      <a
        href={@href}
        class={classes}
        role="link"
        {@aria_attrs}
        {@rest}
      >
        <%= if @icon && @icon_position == :left do %>
          <.icon name={@icon} class="inline-block mr-1 align-middle" />
        <% end %>
        <%= render_slot(@inner_block) %>
        <%= if @icon && @icon_position == :right do %>
          <.icon name={@icon} class="inline-block ml-1 align-middle" />
        <% end %>
      </a>
    <% end %>
    """
  end

  defp base_classes do
    "transition-colors duration-200"
  end

  defp variant_classes(:inline) do
    "text-link hover:underline focus:underline visited:text-link-visited"
  end

  defp variant_classes(:external) do
    "text-link hover:underline focus:underline"
  end

  defp variant_classes(:muted) do
    "text-muted-foreground hover:text-foreground focus:text-foreground focus:underline"
  end

  defp size_classes(:sm) do
    "text-xs"
  end

  defp size_classes(:md) do
    "text-sm"
  end

  defp size_classes(:lg) do
    "text-base"
  end
end
