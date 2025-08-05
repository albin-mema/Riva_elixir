defmodule RivaAshWeb.Components.Atoms.Label do
  @moduledoc """
  Label component for form elements and content labeling.
  
  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type variant :: :default | :required | :optional | :disabled
  @type size :: :sm | :md | :lg
  @type weight :: :normal | :medium | :semibold | :bold

  @doc """
  Renders a label component with different variants and styles.
  
  ## Examples
    
      <.label for="username">Username</.label>
      
      <.label variant="required" size="lg" weight="bold">
        Email Address
      </.label>
      
      <.label variant="disabled" for="disabled-field">
        Disabled Field
      </.label>
  """
  @spec label(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :for, :string, default: nil
  attr :variant, :string, default: "default", values: ~w(default required optional disabled)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :weight, :string, default: "normal", values: ~w(normal medium semibold bold)
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def label(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         label_class <- build_label_class(validated_assigns) do
      assigns = validated_assigns |> assign(:label_class, label_class)
      render_label(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_for(assigns.for),
         :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_weight(assigns.weight) do
      {:ok, assigns}
    end
  end

  @spec validate_for(for :: String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_for(for) do
    if is_nil(for) or String.trim(for) != "" do
      :ok
    else
      {:error, "For attribute cannot be empty string"}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(default required optional disabled) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: default, required, optional, disabled"}
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

  # Functional Core: Pure class building functions
  @spec build_label_class(assigns :: assigns()) :: String.t()
  defp build_label_class(assigns) do
    base = base_classes(assigns.variant)
    size = size_classes(assigns.size)
    weight = weight_classes(assigns.weight)

    [base, size, weight, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(variant :: String.t()) :: String.t()
  defp base_classes(variant) do
    case variant do
      "default" -> "text-gray-700"
      "required" -> "text-red-600"
      "optional" -> "text-gray-500"
      "disabled" -> "text-gray-400"
      _ -> "text-gray-700"
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

  # Imperative Shell: Rendering functions
  defp render_label(assigns) do
    ~H"""
    <label for={@for} class={@label_class} {@rest}>
      <%= render_slot(@inner_block) %>
      <%= if assigns.variant == "required" do %>
        <span class="text-red-500 ml-1">*</span>
      <% end %>
    </label>
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback label or error state
    IO.puts("Label error: #{reason}")
    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end