defmodule RivaAshWeb.Components.UI.Text do
  @moduledoc """
  Implements a text component using the design system.
  
  A canonical text component that provides consistent typography
  across the application with proper semantic HTML elements.
  """
  use Phoenix.Component

  @doc """
  Renders a text component using the design system.
  
  Supports various text variants with consistent styling and semantic HTML.
  """
  attr :variant, :string, default: "p", values: ~w(h1 h2 h3 h4 h5 h6 p lead small label caption code)
  attr :color, :string, default: "default", values: ~w(default primary secondary muted destructive success warning)
  attr :align, :string, default: "left", values: ~w(left center right justify)
  attr :weight, :string, default: "normal", values: ~w(light normal medium semibold bold)
  attr :italic, :boolean, default: false
  attr :required, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def text(assigns) do
    assigns = assign(assigns, :text_class, text_class(assigns))

    case assigns.variant do
      "h1" ->
        ~H"""
        <h1 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h1>
        """

      "h2" ->
        ~H"""
        <h2 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h2>
        """

      "h3" ->
        ~H"""
        <h3 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h3>
        """

      "h4" ->
        ~H"""
        <h4 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h4>
        """

      "h5" ->
        ~H"""
        <h5 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h5>
        """

      "h6" ->
        ~H"""
        <h6 class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </h6>
        """

      "p" ->
        ~H"""
        <p class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </p>
        """

      "lead" ->
        ~H"""
        <p class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </p>
        """

      "small" ->
        ~H"""
        <small class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </small>
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

      "caption" ->
        ~H"""
        <caption class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </caption>
        """

      "code" ->
        ~H"""
        <code class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </code>
        """

      _ ->
        ~H"""
        <span class={@text_class} {@rest}>
          <%= render_slot(@inner_block) %>
          <%= if @required do %>
            <span class="text-destructive ml-1">*</span>
          <% end %>
        </span>
        """
    end
  end

  defp text_class(assigns) do
    base = base_classes(assigns.variant)
    color = color_classes(assigns.color)
    align = align_classes(assigns.align)
    weight = weight_classes(assigns.weight)
    italic = if assigns.italic, do: "italic", else: ""

    Enum.join([base, color, align, weight, italic, assigns.class], " ")
  end

  defp base_classes(variant) do
    case variant do
      "h1" -> "scroll-m-20 text-4xl font-extrabold tracking-tight lg:text-5xl"
      "h2" -> "scroll-m-20 border-b pb-2 text-3xl font-semibold tracking-tight first:mt-0"
      "h3" -> "scroll-m-20 text-2xl font-semibold tracking-tight"
      "h4" -> "scroll-m-20 text-xl font-semibold tracking-tight"
      "h5" -> "scroll-m-20 text-lg font-semibold tracking-tight"
      "h6" -> "scroll-m-20 text-base font-semibold tracking-tight"
      "p" -> "leading-7 [&:not(:first-child)]:mt-6"
      "lead" -> "text-xl text-muted-foreground"
      "small" -> "text-sm font-medium leading-none"
      "label" -> "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"
      "caption" -> "mt-4 text-sm text-muted-foreground"
      "code" -> "relative rounded bg-muted px-[0.3rem] py-[0.2rem] font-mono text-sm font-semibold"
      _ -> ""
    end
  end

  defp color_classes(color) do
    case color do
      "default" -> ""
      "primary" -> "text-primary"
      "secondary" -> "text-secondary-foreground"
      "muted" -> "text-muted-foreground"
      "destructive" -> "text-destructive"
      "success" -> "text-green-600 dark:text-green-400"
      "warning" -> "text-yellow-600 dark:text-yellow-400"
      _ -> ""
    end
  end

  defp align_classes(align) do
    case align do
      "left" -> "text-left"
      "center" -> "text-center"
      "right" -> "text-right"
      "justify" -> "text-justify"
      _ -> ""
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
end
