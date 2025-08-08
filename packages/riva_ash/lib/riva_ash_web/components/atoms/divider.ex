alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Atoms.Divider do
  @moduledoc """
  Divider component for creating visual separations between content.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type variant :: :horizontal | :vertical
  @type size :: :sm | :md | :lg
  @type color :: :default | :primary | :secondary | :success | :warning | :error

  @doc """
  Renders a divider component with different variants and styles.

  ## Examples

      <.divider />

      <.divider variant="vertical" size="lg" color="primary" />

      <.divider>
        <span class="px-2 text-gray-500 text-sm">Or</span>
      </.divider>
  """
  @spec divider(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :variant, :string, default: "horizontal", values: ~w(horizontal vertical)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :color, :string, default: "default", values: ~w(default primary secondary success warning error)
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block

  def divider(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         divider_class <- build_divider_class(validated_assigns) do
      assigns = validated_assigns |> assign(:divider_class, divider_class)
      render_divider(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_color(assigns.color) do
      {:ok, assigns}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(horizontal vertical) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: horizontal, vertical"}
    end
  end

  @spec validate_size(size :: String.t()) :: :ok | {:error, String.t()}
  defp validate_size(size) do
    if size in ~w(sm md lg) do
      :ok
    else
      {:error, "Invalid size. Must be one of: sm, md, lg"}
    end
  end

  @spec validate_color(color :: String.t()) :: :ok | {:error, String.t()}
  defp validate_color(color) do
    if color in ~w(default primary secondary success warning error) do
      :ok
    else
      {:error, "Invalid color. Must be one of: default, primary, secondary, success, warning, error"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_divider_class(assigns :: assigns()) :: String.t()
  defp build_divider_class(assigns) do
    base = base_classes(assigns.variant)
    size = size_classes(assigns.size)
    color = color_classes(assigns.color)

    [base, size, color, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(variant :: String.t()) :: String.t()
  defp base_classes(variant) do
    case variant do
      "horizontal" -> "w-full my-4"
      "vertical" -> "h-full mx-4"
      _ -> "w-full my-4"
    end
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "sm" -> "h-px"
      "md" -> "h-px"
      "lg" -> "h-px"
      _ -> "h-px"
    end
  end

  @spec color_classes(color :: String.t()) :: String.t()
  defp color_classes(color) do
    case color do
      "default" -> "border-gray-300"
      "primary" -> "border-blue-500"
      "secondary" -> "border-gray-500"
      "success" -> "border-green-500"
      "warning" -> "border-yellow-500"
      "error" -> "border-red-500"
      _ -> "border-gray-300"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_divider(assigns) do
    ~H"""
    <div class={@divider_class} {@rest}>
      <%= if @inner_block do %>
        <div class="flex items-center">
          <div class={["flex-grow", divider_border_classes(@variant, @color)]}></div>
          <div class="flex-shrink-0 px-2">
            <%= render_slot(@inner_block) %>
          </div>
          <div class={["flex-grow", divider_border_classes(@variant, @color)]}></div>
        </div>
      <% else %>
        <div class={divider_border_classes(@variant, @color)}></div>
      <% end %>
    </div>
    """
  end

  @spec divider_border_classes(variant :: String.t(), color :: String.t()) :: String.t()
  defp divider_border_classes(variant, color) do
    case variant do
      "horizontal" -> "border-t"
      "vertical" -> "border-l"
      _ -> "border-t"
    end
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback divider or error state
    IO.puts("Divider error: #{reason}")

    assigns = %{reason: reason}

    ~H"""
    <div class="relative bg-red-100 px-4 py-3 border border-red-400 rounded text-red-700">
      <span class="block sm:inline">Error: <%= @reason %></span>
    </div>
    """
  end
end
