defmodule RivaAshWeb.Components.Molecules.EmptyState do
  @moduledoc """
  EmptyState component for displaying when no data is available.
  A molecule component that provides a consistent empty state experience.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Text
  import RivaAshWeb.Components.Atoms.Icon


  @doc """
  Renders an empty state with icon, title, description, and optional action.

  ## Examples

      <.empty_state
        icon={:building_office_2}
        title="No businesses found"
        description="Create your first business to get started"
      >
        <:action>
          <.button variant="primary" icon_left="lucide-plus">
            Create Business
          </.button>
        </:action>
      </.empty_state>

      <.empty_state
        icon={:magnifying_glass}
        title="No results found"
        description="Try adjusting your search or filters"
      />
  """
  attr(:icon, :atom, required: true)
  attr(:title, :string, required: true)
  attr(:description, :string, default: nil)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default bordered card))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:action)

  def empty_state(assigns) do
    assigns = assign(assigns, :container_class, container_class(assigns))

    ~H"""
    <div class={@container_class} {@rest}>
      <div class={content_class(@size)}>
        <div class={icon_wrapper_class(@size)}>
          <.icon name={@icon} size={icon_size(@size)} class="text-muted-foreground" />
        </div>

        <div class="space-y-2 text-center">
          <.text variant={title_variant(@size)} weight="semibold">
            <%= @title %>
          </.text>

          <%= if @description do %>
            <.text variant={description_variant(@size)} color="muted">
              <%= @description %>
            </.text>
          <% end %>
        </div>

        <%= if @action != [] do %>
          <div class="flex items-center justify-center gap-3 mt-6">
            <%= render_slot(@action) %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @doc """
  A simplified empty state for inline use (e.g., in dropdowns or small containers).
  """
  attr(:text, :string, required: true)
  attr(:icon, :atom, default: nil)
  attr(:class, :string, default: "")

  def inline_empty_state(assigns) do
    ~H"""
    <div class={["flex items-center justify-center gap-2 py-4 px-6 text-muted-foreground", @class]}>
      <%= if @icon do %>
        <.icon name={@icon} size="sm" />
      <% end %>
      <.text variant="small" color="muted">
        <%= @text %>
      </.text>
    </div>
    """
  end

  defp container_class(assigns) do
    base = "flex items-center justify-center"

    variant_classes =
      case assigns.variant do
        "default" -> "py-12"
        "bordered" -> "py-12 border-2 border-dashed border-border rounded-lg"
        "card" -> "py-12 bg-card/50 rounded-lg shadow-sm"
        _ -> "py-12"
      end

    Enum.join([base, variant_classes, assigns.class], " ")
  end

  defp content_class(size) do
    case size do
      "sm" -> "max-w-sm mx-auto"
      "md" -> "max-w-md mx-auto"
      "lg" -> "max-w-lg mx-auto"
      _ -> "max-w-md mx-auto"
    end
  end

  defp icon_wrapper_class(size) do
    base = "mx-auto mb-4 flex items-center justify-center rounded-full bg-muted/30"

    size_classes =
      case size do
        "sm" -> "h-12 w-12"
        "md" -> "h-16 w-16"
        "lg" -> "h-20 w-20"
        _ -> "h-16 w-16"
      end

    "#{base} #{size_classes}"
  end

  defp icon_size(size) do
    case size do
      "sm" -> "md"
      "md" -> "lg"
      "lg" -> "xl"
      _ -> "lg"
    end
  end

  defp title_variant(size) do
    case size do
      "sm" -> "h6"
      "md" -> "h5"
      "lg" -> "h4"
      _ -> "h5"
    end
  end

  defp description_variant(size) do
    case size do
      "sm" -> "small"
      "md" -> "p"
      "lg" -> "lead"
      _ -> "p"
    end
  end
end
