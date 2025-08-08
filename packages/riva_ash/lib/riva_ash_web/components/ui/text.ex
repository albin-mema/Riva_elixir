alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.LiveView.Rendered, as: Rendered

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

  @spec text(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def text(assigns) do
    # Render text using functional composition
    assigns
    |> Map.put_new(:text_class, text_class(assigns))
    |> Map.put_new(:required_class, build_required_class(assigns.required))
    |> Map.put_new(:wrapper_class, build_wrapper_class(assigns.variant))
    |> render_text_component(assigns.variant)
  end

  # Private helper for text rendering
  @spec render_text_component(assigns :: map(), String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_text_component(assigns, variant) do
    assigns = Map.put(assigns, :variant, variant)
    tag = text_tag(variant)

    ~H"""
    <#{tag} class={@text_class} {@rest}>
      <%= render_slot(@inner_block) %>
      <%= if @required do %>
        <span class={@required_class}>*</span>
      <% end %>
    </#{tag}>
    """
  end

  # Private helper to get the HTML tag for a variant
  defp text_tag(variant) do
    tag_map()
    |> Map.get(variant, "span")
  end

  # Private map of variant to HTML tags
  defp tag_map do
    %{
      "h1" => "h1",
      "h2" => "h2",
      "h3" => "h3",
      "h4" => "h4",
      "h5" => "h5",
      "h6" => "h6",
      "p" => "p",
      "lead" => "p",
      "small" => "small",
      "label" => "label",
      "caption" => "caption",
      "code" => "code"
    }
  end

  # Helper function to build required classes
  @spec build_required_class(boolean()) :: String.t()
  defp build_required_class(required) do
    class =
      if required do
        "text-destructive ml-1"
      else
        "hidden"
      end

    class
  end

  # Helper function to build wrapper classes
  @spec build_wrapper_class(String.t()) :: String.t()
  defp build_wrapper_class(variant) do
    variant_classes()
    |> Map.get(variant, "")
  end

  defp text_class(assigns) do
    base = base_classes(assigns.variant)
    color = color_classes(assigns.color)
    align = align_classes(assigns.align)
    weight = weight_classes(assigns.weight)

    italic =
      if assigns.italic do
        "italic"
      else
        ""
      end

    Enum.join([base, color, align, weight, italic, assigns.class], " ")
  end

  defp base_classes(variant) do
    variant_classes()
    |> Map.get(variant, "")
  end

  # Private map of variant classes
  defp variant_classes do
    %{
      "h1" => "scroll-m-20 text-4xl font-extrabold tracking-tight lg:text-5xl",
      "h2" => "scroll-m-20 border-b pb-2 text-3xl font-semibold tracking-tight first:mt-0",
      "h3" => "scroll-m-20 text-2xl font-semibold tracking-tight",
      "h4" => "scroll-m-20 text-xl font-semibold tracking-tight",
      "h5" => "scroll-m-20 text-lg font-semibold tracking-tight",
      "h6" => "scroll-m-20 text-base font-semibold tracking-tight",
      "p" => "leading-7 [&:not(:first-child)]:mt-6",
      "lead" => "text-xl text-muted-foreground",
      "small" => "text-sm font-medium leading-none",
      "label" => "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70",
      "caption" => "mt-4 text-sm text-muted-foreground",
      "code" => "relative rounded bg-muted px-[0.3rem] py-[0.2rem] font-mono text-sm font-semibold"
    }
  end

  defp color_classes(color) do
    color_map()
    |> Map.get(color, "")
  end

  defp align_classes(align) do
    align_map()
    |> Map.get(align, "")
  end

  defp weight_classes(weight) do
    weight_map()
    |> Map.get(weight, "")
  end

  # Private map of color classes
  defp color_map do
    %{
      "default" => "",
      "primary" => "text-primary",
      "secondary" => "text-secondary-foreground",
      "muted" => "text-muted-foreground",
      "destructive" => "text-destructive",
      "success" => "text-green-600 dark:text-green-400",
      "warning" => "text-yellow-600 dark:text-yellow-400"
    }
  end

  # Private map of alignment classes
  defp align_map do
    %{
      "left" => "text-left",
      "center" => "text-center",
      "right" => "text-right",
      "justify" => "text-justify"
    }
  end

  # Private map of weight classes
  defp weight_map do
    %{
      "light" => "font-light",
      "normal" => "font-normal",
      "medium" => "font-medium",
      "semibold" => "font-semibold",
      "bold" => "font-bold"
    }
  end
end
