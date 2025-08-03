defmodule RivaAshWeb.Components.Molecules.Card do
  @moduledoc """
  Card component for grouping related content.
  A molecule component that combines multiple atoms to create a content container.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Text

  @doc """
  Renders a card container with optional header and footer.

  ## Examples

      <.card>
        <:header>
          <.card_title>Card Title</.card_title>
          <.card_description>Card description</.card_description>
        </:header>
        <:body>
          Card content goes here
        </:body>
        <:footer>
          Footer content
        </:footer>
      </.card>

      <.card variant="bordered">
        <:body>Simple card with just body content</:body>
      </.card>
  """
  attr(:variant, :string, default: "elevated", values: ~w(elevated bordered ghost))
  attr(:padding, :string, default: "normal", values: ~w(none compact normal spacious))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:header)
  slot(:body)
  slot(:footer)

  def card(assigns) do
    assigns = assign(assigns, :card_class, card_class(assigns))

    ~H"""
    <div class={@card_class} {@rest}>
      <%= if @header != [] do %>
        <div class={header_class(@padding)}>
          <%= render_slot(@header) %>
        </div>
      <% end %>

      <div class={body_class(@padding)}>
        <%= render_slot(@body) %>
      </div>

      <%= if @footer != [] do %>
        <div class={footer_class(@padding)}>
          <%= render_slot(@footer) %>
        </div>
      <% end %>
    </div>
    """
  end

  @doc """
  Card title component for use in card headers.
  """
  attr(:class, :string, default: "")
  slot(:inner_block, required: true)

  def card_title(assigns) do
    ~H"""
    <.heading level={3} class={["text-foreground", @class]}>
      <%= render_slot(@inner_block) %>
    </.heading>
    """
  end

  @doc """
  Card description component for use in card headers.
  """
  attr(:class, :string, default: "")
  slot(:inner_block, required: true)

  def card_description(assigns) do
    ~H"""
    <.text variant="small" color="muted" class={["mt-1", @class]}>
      <%= render_slot(@inner_block) %>
    </.text>
    """
  end

  defp card_class(assigns) do
    base = "rounded-lg overflow-hidden"
    variant = variant_classes(assigns.variant)

    Enum.join([base, variant, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "elevated" ->
        "bg-card text-card-foreground shadow-sm hover:shadow-md transition-shadow"

      "bordered" ->
        "bg-card text-card-foreground border border-border"

      "ghost" ->
        "bg-transparent"

      _ ->
        "bg-card text-card-foreground shadow-sm"
    end
  end

  defp header_class(padding) do
    base = "border-b border-border"
    padding_class = padding_classes(padding)

    "#{base} #{padding_class}"
  end

  defp body_class(padding) do
    padding_classes(padding)
  end

  defp footer_class(padding) do
    base = "border-t border-border bg-muted/50"
    padding_class = padding_classes(padding)

    "#{base} #{padding_class}"
  end

  defp padding_classes(padding) do
    case padding do
      "none" -> ""
      "compact" -> "p-4"
      "normal" -> "p-6"
      "spacious" -> "p-8"
      _ -> "p-6"
    end
  end
end
