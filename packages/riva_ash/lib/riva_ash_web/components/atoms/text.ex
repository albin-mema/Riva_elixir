defmodule RivaAshWeb.Components.Atoms.Text do
  @moduledoc """
  Text component for consistent typography across the application.
  Encapsulates all text styling without exposing Tailwind classes.
  """
  use Phoenix.Component

  @doc """
  Renders text with consistent styling.

  ## Examples

      <.text>Default paragraph text</.text>
      <.text variant="h1">Page Title</.text>
      <.text variant="small" color="muted">Subtitle text</.text>
      <.text variant="label" required>Field Label</.text>
  """
  attr(:variant, :string,
    default: "p",
    values: ~w(h1 h2 h3 h4 h5 h6 p lead small label caption code)
  )

  attr(:color, :string,
    default: "default",
    values: ~w(default primary secondary muted destructive success warning)
  )

  attr(:align, :string, default: "left", values: ~w(left center right justify))
  attr(:weight, :string, default: "normal", values: ~w(light normal medium semibold bold))
  attr(:italic, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:inner_block, required: true)

  def text(assigns) do
    assigns = assign(assigns, :text_class, text_class(assigns))

    case assigns.variant do
      "h1" ->
        ~H"""
        <h1 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required && @variant == "label" do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h1>
        """

      "h2" ->
        ~H"""
        <h2 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required && @variant == "label" do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h2>
        """

      "h3" ->
        ~H"""
        <h3 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required && @variant == "label" do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h3>
        """

      "h4" ->
        ~H"""
        <h4 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required && @variant == "label" do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h4>
        """

      "h5" ->
        ~H"""
        <h5 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required && @variant == "label" do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h5>
        """

      "h6" ->
        ~H"""
        <h6 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required && @variant == "label" do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h6>
        """

      "label" ->
        ~H"""
        <label class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </label>
        """

      "code" ->
        ~H"""
        <code class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required && @variant == "label" do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </code>
        """

      _ ->
        ~H"""
        <p class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required && @variant == "label" do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </p>
        """
    end
  end

  @doc """
  Renders a heading with consistent styling.
  Convenience function for headings.
  """
  attr(:level, :integer, default: 1, values: [1, 2, 3, 4, 5, 6])
  attr(:color, :string, default: "default")
  attr(:align, :string, default: "left")
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:inner_block, required: true)

  def heading(assigns) do
    assigns = assign(assigns, :variant, "h#{assigns.level}")

    ~H"""
    <.text variant={@variant} color={@color} align={@align} class={@class} {@rest}>
      <%= render_slot(@inner_block) %>
    </.text>
    """
  end

  defp text_class(assigns) do
    base = base_classes(assigns.variant)
    color = color_classes(assigns.color)
    align = align_classes(assigns.align)
    weight = weight_classes(assigns.weight)
    style = style_classes(assigns)

    Enum.join([base, color, align, weight, style, assigns.class], " ")
  end

  defp base_classes(variant) do
    case variant do
      "h1" ->
        "text-4xl lg:text-5xl tracking-tight"

      "h2" ->
        "text-3xl lg:text-4xl tracking-tight"

      "h3" ->
        "text-2xl lg:text-3xl tracking-tight"

      "h4" ->
        "text-xl lg:text-2xl"

      "h5" ->
        "text-lg lg:text-xl"

      "h6" ->
        "text-base lg:text-lg"

      "p" ->
        "text-base leading-7"

      "lead" ->
        "text-xl text-muted-foreground"

      "small" ->
        "text-sm leading-6"

      "label" ->
        "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"

      "caption" ->
        "text-xs text-muted-foreground"

      "code" ->
        "font-mono text-sm bg-muted px-[0.3rem] py-[0.2rem] rounded"

      _ ->
        "text-base"
    end
  end

  defp color_classes(color) do
    case color do
      "default" -> "text-foreground"
      "primary" -> "text-primary"
      "secondary" -> "text-secondary-foreground"
      "muted" -> "text-muted-foreground"
      "destructive" -> "text-destructive"
      "success" -> "text-green-600 dark:text-green-500"
      "warning" -> "text-yellow-600 dark:text-yellow-500"
      _ -> "text-foreground"
    end
  end

  defp align_classes(align) do
    case align do
      "left" -> "text-left"
      "center" -> "text-center"
      "right" -> "text-right"
      "justify" -> "text-justify"
      _ -> "text-left"
    end
  end

  defp weight_classes(weight) do
    case weight do
      "light" -> "font-light"
      "normal" -> "font-normal"
      "medium" -> "font-medium"
      "semibold" -> "font-semibold"
      "bold" -> "font-bold"
      _ -> ""
    end
  end

  defp style_classes(assigns) do
    if assigns.italic, do: "italic", else: ""
  end
end
