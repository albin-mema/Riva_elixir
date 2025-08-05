defmodule RivaAshWeb.Components.Atoms.Link do
  @moduledoc """
  Link component for navigation and interactive elements.
  
  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type variant :: :default | :primary | :secondary | :success | :warning | :error
  @type size :: :sm | :md | :lg
  @type weight :: :normal | :medium | :semibold | :bold
  @type underline :: :always | :hover | :none

  @doc """
  Renders a link component with different variants and styles.
  
  ## Examples
    
      <.link href="/about">About Us</.link>
      
      <.link variant="primary" size="lg" weight="bold" underline="hover">
        Get Started
      </.link>
      
      <.link variant="success" to={~p"/dashboard"}>
        Dashboard
      </.link>
  """
  @spec link(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :href, :string, default: nil
  attr :to, :string, default: nil
  attr :variant, :string, default: "default", values: ~w(default primary secondary success warning error)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :weight, :string, default: "normal", values: ~w(normal medium semibold bold)
  attr :underline, :string, default: "hover", values: ~w(always hover none)
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def link(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         link_class <- build_link_class(validated_assigns) do
      assigns = validated_assigns |> assign(:link_class, link_class)
      render_link(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_href(assigns.href),
         :ok <- validate_to(assigns.to),
         :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_weight(assigns.weight),
         :ok <- validate_underline(assigns.underline) do
      {:ok, assigns}
    end
  end

  @spec validate_href(href :: String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_href(href) do
    if is_nil(href) or String.trim(href) != "" do
      :ok
    else
      {:error, "Href cannot be empty string"}
    end
  end

  @spec validate_to(to :: String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_to(to) do
    if is_nil(to) or String.trim(to) != "" do
      :ok
    else
      {:error, "To cannot be empty string"}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(default primary secondary success warning error) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: default, primary, secondary, success, warning, error"}
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

  @spec validate_weight(weight :: String.t()) :: :ok | {:error, String.t()}
  defp validate_weight(weight) do
    if weight in ~w(normal medium semibold bold) do
      :ok
    else
      {:error, "Invalid weight. Must be one of: normal, medium, semibold, bold"}
    end
  end

  @spec validate_underline(underline :: String.t()) :: :ok | {:error, String.t()}
  defp validate_underline(underline) do
    if underline in ~w(always hover none) do
      :ok
    else
      {:error, "Invalid underline. Must be one of: always, hover, none"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_link_class(assigns :: assigns()) :: String.t()
  defp build_link_class(assigns) do
    base = base_classes(assigns.variant)
    size = size_classes(assigns.size)
    weight = weight_classes(assigns.weight)
    underline = underline_classes(assigns.underline)

    [base, size, weight, underline, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(variant :: String.t()) :: String.t()
  defp base_classes(variant) do
    case variant do
      "default" -> "text-blue-600 hover:text-blue-800"
      "primary" -> "text-blue-600 hover:text-blue-800"
      "secondary" -> "text-gray-600 hover:text-gray-800"
      "success" -> "text-green-600 hover:text-green-800"
      "warning" -> "text-yellow-600 hover:text-yellow-800"
      "error" -> "text-red-600 hover:text-red-800"
      _ -> "text-blue-600 hover:text-blue-800"
    end
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "sm" -> "text-sm"
      "md" -> "text-base"
      "lg" -> "text-lg"
      _ -> "text-base"
    end
  end

  @spec weight_classes(weight :: String.t()) :: String.t()
  defp weight_classes(weight) do
    case weight do
      "normal" -> "font-normal"
      "medium" -> "font-medium"
      "semibold" -> "font-semibold"
      "bold" -> "font-bold"
      _ -> "font-normal"
    end
  end

  @spec underline_classes(underline :: String.t()) :: String.t()
  defp underline_classes(underline) do
    case underline do
      "always" -> "underline"
      "hover" -> "hover:underline"
      "none" -> "no-underline"
      _ -> "hover:underline"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_link(assigns) do
    ~H"""
    <%= if assigns.to do %>
      <%= live_patch(render_slot(@inner_block), to: @to, class: @link_class, @rest) %>
    <% else %>
      <a href={@href} class={@link_class} {@rest}>
        <%= render_slot(@inner_block) %>
      </a>
    <% end %>
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback link or error state
    IO.puts("Link error: #{reason}")
    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end