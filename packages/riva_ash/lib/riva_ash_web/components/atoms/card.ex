defmodule RivaAshWeb.Components.Atoms.Card do
  @moduledoc """
  Card component for displaying content in a styled container.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type variant :: :default | :outlined | :elevated
  @type size :: :sm | :md | :lg | :xl
  @type padding :: :none | :sm | :md | :lg

  @doc """
  Renders a card component with different variants and sizes.

  ## Examples

      <.card>
        <.card_header>
          <h3 class="text-lg font-semibold">Card Title</h3>
        </.card_header>
        <.card_body>
          <p>Card content goes here</p>
        </.card_body>
        <.card_footer>
          <button class="bg-blue-500 text-white px-4 py-2 rounded">Action</button>
        </.card_footer>
      </.card>

      <.card variant="elevated" size="lg">
        <.card_header>
          <h3 class="text-xl font-bold">Large Elevated Card</h3>
        </.card_header>
        <.card_body>
          <p>Content with elevated styling</p>
        </.card_body>
      </.card>
  """
  @spec card(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :variant, :string, default: "default", values: ~w(default outlined elevated)
  attr :size, :string, default: "md", values: ~w(sm md lg xl)
  attr :padding, :string, default: "md", values: ~w(none sm md lg)
  attr :class, :string, default: ""
  attr :rest, :global

  slot :header
  slot :body, required: true
  slot :footer

  def card(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         card_class <- build_card_class(validated_assigns) do
      assigns = validated_assigns |> assign(:card_class, card_class)
      render_card(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_padding(assigns.padding) do
      {:ok, assigns}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(default outlined elevated) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: default, outlined, elevated"}
    end
  end

  @spec validate_size(size :: String.t()) :: :ok | {:error, String.t()}
  defp validate_size(size) do
    if size in ~w(sm md lg xl) do
      :ok
    else
      {:error, "Invalid size. Must be one of: sm, md, lg, xl"}
    end
  end

  @spec validate_padding(padding :: String.t()) :: :ok | {:error, String.t()}
  defp validate_padding(padding) do
    if padding in ~w(none sm md lg) do
      :ok
    else
      {:error, "Invalid padding. Must be one of: none, sm, md, lg"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_card_class(assigns :: assigns()) :: String.t()
  defp build_card_class(assigns) do
    base = base_classes(assigns.variant)
    size = size_classes(assigns.size)
    padding = padding_classes(assigns.padding)

    [base, size, padding, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(variant :: String.t()) :: String.t()
  defp base_classes(variant) do
    case variant do
      "default" -> "bg-white rounded-lg shadow-sm border border-gray-200"
      "outlined" -> "bg-white rounded-lg border-2 border-gray-300"
      "elevated" -> "bg-white rounded-lg shadow-lg border border-gray-100"
      _ -> "bg-white rounded-lg shadow-sm border border-gray-200"
    end
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "sm" -> "w-64"
      "md" -> "w-96"
      "lg" -> "w-full max-w-2xl"
      "xl" -> "w-full max-w-4xl"
      _ -> "w-96"
    end
  end

  @spec padding_classes(padding :: String.t()) :: String.t()
  defp padding_classes(padding) do
    case padding do
      "none" -> "p-0"
      "sm" -> "p-3"
      "md" -> "p-6"
      "lg" -> "p-8"
      _ -> "p-6"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_card(assigns) do
    ~H"""
    <div class={@card_class} {@rest}>
      <%= if @header do %>
        <.card_header>
          <%= render_slot(@header) %>
        </.card_header>
      <% end %>

      <.card_body>
        <%= render_slot(@body) %>
      </.card_body>

      <%= if @footer do %>
        <.card_footer>
          <%= render_slot(@footer) %>
        </.card_footer>
      <% end %>
    </div>
    """
  end

  # Card Header
  attr :class, :string, default: ""

  defp card_header(assigns) do
    ~H"""
    <div class={["border-b border-gray-200 pb-4 mb-4", @class]}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  # Card Body
  attr :class, :string, default: ""

  defp card_body(assigns) do
    ~H"""
    <div class={@class}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  # Card Footer
  attr :class, :string, default: ""

  defp card_footer(assigns) do
    ~H"""
    <div class={["border-t border-gray-200 pt-4 mt-4", @class]}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback card or error state
    IO.puts("Card error: #{reason}")

    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end
