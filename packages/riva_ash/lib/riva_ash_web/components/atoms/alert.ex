defmodule RivaAshWeb.Components.Atoms.Alert do
  @moduledoc """
  Alert component for displaying important messages to users.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type variant :: :info | :success | :warning | :error
  @type size :: :sm | :md | :lg
  @type closable :: boolean()

  @doc """
  Renders an alert component with different variants and sizes.

  ## Examples

      <.alert variant="info">This is an informational message.alert>
      <.alert variant="success" closable={true}>Success!</.alert>
      <.alert variant="warning" size="lg">Warning message</.alert>
      <.alert variant="error">Error occurred</.alert>
  """
  @spec alert(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :variant, :string, default: "info", values: ~w(info success warning error)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :closable, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def alert(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         alert_class <- build_alert_class(validated_assigns) do
      assigns = validated_assigns |> assign(:alert_class, alert_class)
      render_alert(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_closable(assigns.closable) do
      {:ok, assigns}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(info success warning error) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: info, success, warning, error"}
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

  @spec validate_closable(closable :: boolean()) :: :ok | {:error, String.t()}
  defp validate_closable(closable) do
    if is_boolean(closable) do
      :ok
    else
      {:error, "Closable must be a boolean value"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_alert_class(assigns :: assigns()) :: String.t()
  defp build_alert_class(assigns) do
    base = base_classes(assigns.variant)
    size = size_classes(assigns.size)
    color = color_classes(assigns.variant)

    [base, size, color, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(variant :: String.t()) :: String.t()
  defp base_classes(variant) do
    case variant do
      "info" -> "relative flex items-center justify-between p-4 rounded-lg border"
      "success" -> "relative flex items-center justify-between p-4 rounded-lg border"
      "warning" -> "relative flex items-center justify-between p-4 rounded-lg border"
      "error" -> "relative flex items-center justify-between p-4 rounded-lg border"
      _ -> "relative flex items-center justify-between p-4 rounded-lg border"
    end
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "sm" -> "p-3 text-sm"
      "md" -> "p-4"
      "lg" -> "p-6 text-lg"
      _ -> "p-4"
    end
  end

  @spec color_classes(variant :: String.t()) :: String.t()
  defp color_classes(variant) do
    case variant do
      "info" -> "bg-blue-50 border-blue-200 text-blue-800"
      "success" -> "bg-green-50 border-green-200 text-green-800"
      "warning" -> "bg-yellow-50 border-yellow-200 text-yellow-800"
      "error" -> "bg-red-50 border-red-200 text-red-800"
      _ -> "bg-gray-50 border-gray-200 text-gray-800"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_alert(assigns) do
    ~H"""
    <div class={@alert_class} {@rest}>
      <div class="flex items-center">
        <%= render_slot(@inner_block) %>
      </div>
      <%= if @closable do %>
        <button class="ml-auto -mx-1.5 -my-1.5 rounded-lg focus:ring-2 p-1.5 inline-flex h-8 w-8 <%= close_button_color(@variant) %>" type="button">
          <span class="sr-only">Close</span>
          <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
            <path fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd" />
          </svg>
        </button>
      <% end %>
    </div>
    """
  end

  @spec close_button_color(variant :: String.t()) :: String.t()
  defp close_button_color(variant) do
    case variant do
      "info" -> "bg-blue-200 text-blue-500 hover:bg-blue-300 focus:ring-blue-400"
      "success" -> "bg-green-200 text-green-500 hover:bg-green-300 focus:ring-green-400"
      "warning" -> "bg-yellow-200 text-yellow-500 hover:bg-yellow-300 focus:ring-yellow-400"
      "error" -> "bg-red-200 text-red-500 hover:bg-red-300 focus:ring-red-400"
      _ -> "bg-gray-200 text-gray-500 hover:bg-gray-300 focus:ring-gray-400"
    end
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback alert or error state
    IO.puts("Alert error: #{reason}")

    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end
