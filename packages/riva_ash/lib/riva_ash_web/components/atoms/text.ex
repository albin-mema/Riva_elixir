defmodule RivaAshWeb.Components.Atoms.Text do
  @moduledoc """
  Text component for consistent typography across the application.
  Encapsulates all text styling without exposing Tailwind classes.
  
  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type variant :: :h1 | :h2 | :h3 | :h4 | :h5 | :h6 | :p | :lead | :small | :label | :caption | :code
  @type color :: :default | :primary | :secondary | :muted | :destructive | :success | :warning
  @type align :: :left | :center | :right | :justify
  @type weight :: :light | :normal | :medium | :semibold | :bold

  @doc """
  Renders text with consistent styling.

  ## Examples

      <.text>Default paragraph text</.text>
      <.text variant="h1">Page Title</.text>
      <.text variant="small" color="muted">Subtitle text</.text>
      <.text variant="label" required>Field Label</.text>
  """
  @spec text(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
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
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         text_class <- build_text_class(validated_assigns) do
      assigns = validated_assigns |> assign(:text_class, text_class)
      render_text(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  @doc """
  Renders a heading with consistent styling.
  Convenience function for headings.
  """
  @spec heading(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:level, :integer, default: 1, values: [1, 2, 3, 4, 5, 6])
  attr(:color, :string, default: "default")
  attr(:align, :string, default: "left")
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:inner_block, required: true)

  def heading(assigns) do
    with {:ok, validated_assigns} <- validate_heading_assigns(assigns),
         variant <- "h#{validated_assigns.level}" do
      assigns = validated_assigns |> assign(:variant, variant)
      render_heading(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_variant(assigns.variant),
         :ok <- validate_color(assigns.color),
         :ok <- validate_align(assigns.align),
         :ok <- validate_weight(assigns.weight),
         :ok <- validate_italic(assigns.italic),
         :ok <- validate_required(assigns.required) do
      {:ok, assigns}
    end
  end

  @spec validate_heading_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_heading_assigns(assigns) do
    with :ok <- validate_level(assigns.level),
         :ok <- validate_color(assigns.color),
         :ok <- validate_align(assigns.align) do
      {:ok, assigns}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(h1 h2 h3 h4 h5 h6 p lead small label caption code) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: h1, h2, h3, h4, h5, h6, p, lead, small, label, caption, code"}
    end
  end

  @spec validate_color(color :: String.t()) :: :ok | {:error, String.t()}
  defp validate_color(color) do
    if color in ~w(default primary secondary muted destructive success warning) do
      :ok
    else
      {:error, "Invalid color. Must be one of: default, primary, secondary, muted, destructive, success, warning"}
    end
  end

  @spec validate_align(align :: String.t()) :: :ok | {:error, String.t()}
  defp validate_align(align) do
    if align in ~w(left center right justify) do
      :ok
    else
      {:error, "Invalid align. Must be one of: left, center, right, justify"}
    end
  end

  @spec validate_weight(weight :: String.t()) :: :ok | {:error, String.t()}
  defp validate_weight(weight) do
    if weight in ~w(light normal medium semibold bold) do
      :ok
    else
      {:error, "Invalid weight. Must be one of: light, normal, medium, semibold, bold"}
    end
  end

  @spec validate_level(level :: integer()) :: :ok | {:error, String.t()}
  defp validate_level(level) do
    if level in [1, 2, 3, 4, 5, 6] do
      :ok
    else
      {:error, "Invalid level. Must be one of: 1, 2, 3, 4, 5, 6"}
    end
  end

  @spec validate_italic(italic :: boolean()) :: :ok | {:error, String.t()}
  defp validate_italic(italic) do
    if is_boolean(italic) do
      :ok
    else
      {:error, "Italic must be a boolean value"}
    end
  end

  @spec validate_required(required :: boolean()) :: :ok | {:error, String.t()}
  defp validate_required(required) do
    if is_boolean(required) do
      :ok
    else
      {:error, "Required must be a boolean value"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_text_class(assigns :: assigns()) :: String.t()
  defp build_text_class(assigns) do
    base = base_classes(assigns.variant)
    color = color_classes(assigns.color)
    align = align_classes(assigns.align)
    weight = weight_classes(assigns.weight)
    style = style_classes(assigns)

    [base, color, align, weight, style, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(variant :: String.t()) :: String.t()
  defp base_classes(variant) do
    case variant do
      "h1" -> "text-4xl lg:text-5xl tracking-tight"
      "h2" -> "text-3xl lg:text-4xl tracking-tight"
      "h3" -> "text-2xl lg:text-3xl tracking-tight"
      "h4" -> "text-xl lg:text-2xl"
      "h5" -> "text-lg lg:text-xl"
      "h6" -> "text-base lg:text-lg"
      "p" -> "text-base leading-7"
      "lead" -> "text-xl text-muted-foreground"
      "small" -> "text-sm leading-6"
      "label" -> "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"
      "caption" -> "text-xs text-muted-foreground"
      "code" -> "font-mono text-sm bg-muted px-[0.3rem] py-[0.2rem] rounded"
      _ -> "text-base"
    end
  end

  @spec color_classes(color :: String.t()) :: String.t()
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

  @spec align_classes(align :: String.t()) :: String.t()
  defp align_classes(align) do
    case align do
      "left" -> "text-left"
      "center" -> "text-center"
      "right" -> "text-right"
      "justify" -> "text-justify"
      _ -> "text-left"
    end
  end

  @spec weight_classes(weight :: String.t()) :: String.t()
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

  @spec style_classes(assigns :: assigns()) :: String.t()
  defp style_classes(assigns) do
    if assigns.italic, do: "italic", else: ""
  end

  # Imperative Shell: Rendering functions
  defp render_text(assigns) do
    tag = assigns.variant |> String.to_atom()
    content = render_slot(assigns.inner_block)
    required_indicator = if assigns.required && assigns.variant == "label", do: render_required_indicator(), else: ""

    assigns = assigns |> assign(:content, content) |> assign(:required_indicator, required_indicator)

    render_text_tag(assigns, tag)
  end

  defp render_text_tag(assigns, tag) do
    classes = assigns.text_class
    rest = assigns.rest

    case tag do
      :h1 -> ~H"""
        <h1 class={classes} {rest}>
          <%= @content %><%= @required_indicator %>
        </h1>
        """
      :h2 -> ~H"""
        <h2 class={classes} {rest}>
          <%= @content %><%= @required_indicator %>
        </h2>
        """
      :h3 -> ~H"""
        <h3 class={classes} {rest}>
          <%= @content %><%= @required_indicator %>
        </h3>
        """
      :h4 -> ~H"""
        <h4 class={classes} {rest}>
          <%= @content %><%= @required_indicator %>
        </h4>
        """
      :h5 -> ~H"""
        <h5 class={classes} {rest}>
          <%= @content %><%= @required_indicator %>
        </h5>
        """
      :h6 -> ~H"""
        <h6 class={classes} {rest}>
          <%= @content %><%= @required_indicator %>
        </h6>
        """
      :label -> ~H"""
        <label class={classes} {rest}>
          <%= @content %><%= render_required_indicator(assigns.required) %>
        </label>
        """
      :code -> ~H"""
        <code class={classes} {rest}>
          <%= @content %><%= @required_indicator %>
        </code>
        """
      _ -> ~H"""
        <p class={classes} {rest}>
          <%= @content %><%= @required_indicator %>
        </p>
        """
    end
  end

  defp render_heading(assigns) do
    ~H"""
    <.text variant={@variant} color={@color} align={@align} class={@class} {@rest}>
      <%= render_slot(@inner_block) %>
    </.text>
    """
  end

  defp render_required_indicator() do
    ~H"""
    <span class="text-destructive ml-1">*</span>
    """
  end

  defp render_required_indicator(required) do
    if required, do: render_required_indicator(), else: ""
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback text or error state
    IO.puts("Text error: #{reason}")
    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end
