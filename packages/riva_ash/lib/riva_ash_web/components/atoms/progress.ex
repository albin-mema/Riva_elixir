alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Atoms.Progress do
  @moduledoc """
  Progress component for displaying loading states and completion percentages.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type variant :: :default | :success | :warning | :error
  @type size :: :sm | :md | :lg
  @type type :: :linear | :circular
  @value_min 0
  @value_max 100

  @doc """
  Renders a progress component with different variants and types.

  ## Examples

      <.progress value={75} />

      <.progress value={100} variant="success" size="lg" />

      <.progress variant="warning" type="circular" size="md">
        <.progress_label>Loading...</.progress_label>
      </.progress>
  """
  @spec progress(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :value, :integer, default: 0
  attr :variant, :string, default: "default", values: ~w(default success warning error)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :type, :string, default: "linear", values: ~w(linear circular)
  attr :class, :string, default: ""
  attr :rest, :global

  slot :label

  def progress(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         progress_class <- build_progress_class(validated_assigns) do
      assigns = validated_assigns |> assign(:progress_class, progress_class)
      render_progress(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_value(assigns.value),
         :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_type(assigns.type) do
      {:ok, assigns}
    end
  end

  @spec validate_value(value :: integer()) :: :ok | {:error, String.t()}
  defp validate_value(value) do
    if value >= @value_min and value <= @value_max do
      :ok
    else
      {:error, "Value must be between #{@value_min} and #{@value_max}"}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(default success warning error) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: default, success, warning, error"}
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

  @spec validate_type(type :: String.t()) :: :ok | {:error, String.t()}
  defp validate_type(type) do
    if type in ~w(linear circular) do
      :ok
    else
      {:error, "Invalid type. Must be one of: linear, circular"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_progress_class(assigns :: assigns()) :: String.t()
  defp build_progress_class(assigns) do
    base = base_classes(assigns.type)
    size = size_classes(assigns.size)
    color = color_classes(assigns.variant)

    [base, size, color, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(type :: String.t()) :: String.t()
  defp base_classes(type) do
    case type do
      "linear" -> "w-full"
      "circular" -> "relative"
      _ -> "w-full"
    end
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "sm" -> "h-1"
      "md" -> "h-2"
      "lg" -> "h-3"
      _ -> "h-2"
    end
  end

  @spec color_classes(variant :: String.t()) :: String.t()
  defp color_classes(variant) do
    case variant do
      "default" -> "bg-blue-500"
      "success" -> "bg-green-500"
      "warning" -> "bg-yellow-500"
      "error" -> "bg-red-500"
      _ -> "bg-blue-500"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_progress(assigns) do
    case assigns.type do
      "linear" -> render_linear_progress(assigns)
      "circular" -> render_circular_progress(assigns)
    end
  end

  defp render_linear_progress(assigns) do
    ~H"""
    <div class={@progress_class} {@rest}>
      <div class="bg-gray-200 rounded-full w-full">
        <div
          class="rounded-full h-full transition-all duration-300 ease-in-out"
          style={"width: #{@value}%"}
          role="progressbar"
          aria-valuenow={@value}
          aria-valuemin={@value_min}
          aria-valuemax={@value_max}
        >
        </div>
      </div>
      <%= if @label do %>
        <.progress_label>
          <%= render_slot(@label) %>
        </.progress_label>
      <% end %>
    </div>
    """
  end

  defp render_circular_progress(assigns) do
    ~H"""
    <div class={@progress_class} {@rest}>
      <div class="relative">
        <svg class="w-16 h-16" viewBox="0 0 36 36">
          <path
            d="M18 2.0845 a 15.9155 15.9155 0 0 1 0 31.831 a 15.9155 15.9155 0 0 1 0 -31.831"
            fill="none"
            stroke="#e5e7eb"
            stroke-width="3"
          />
          <path
            d="M18 2.0845 a 15.9155 15.9155 0 0 1 0 31.831 a 15.9155 15.9155 0 0 1 0 -31.831"
            fill="none"
            stroke={stroke_color(@variant)}
            stroke-width="3"
            stroke-dasharray={@value}, 100
            stroke-linecap="round"
            class="transition-all duration-300 ease-in-out"
          />
        </svg>
        <div class="absolute inset-0 flex justify-center items-center">
          <span class="font-medium text-gray-700 text-sm"><%= @value %>%</span>
        </div>
      </div>
      <%= if @label do %>
        <.progress_label>
          <%= render_slot(@label) %>
        </.progress_label>
      <% end %>
    </div>
    """
  end

  @spec stroke_color(variant :: String.t()) :: String.t()
  defp stroke_color(variant) do
    case variant do
      "default" -> "#3b82f6"
      "success" -> "#10b981"
      "warning" -> "#f59e0b"
      "error" -> "#ef4444"
      _ -> "#3b82f6"
    end
  end

  # Progress Label
  attr :class, :string, default: ""

  defp progress_label(assigns) do
    ~H"""
    <div class={["mt-2 text-sm text-gray-600", @class]}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback progress or error state
    IO.puts("Progress error: #{reason}")

    assigns = %{reason: reason}

    ~H"""
    <div class="relative bg-red-100 px-4 py-3 border border-red-400 rounded text-red-700">
      <span class="block sm:inline">Error: <%= @reason %></span>
    </div>
    """
  end
end
