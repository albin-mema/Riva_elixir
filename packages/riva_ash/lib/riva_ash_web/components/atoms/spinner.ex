defmodule RivaAshWeb.Components.Atoms.Spinner do
  @moduledoc """
  Loading spinner component with various styles.
  
  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type size :: :xs | :sm | :md | :lg | :xl
  @type variant :: :default | :primary | :secondary

  @doc """
  Renders a loading spinner.
  
  ## Examples
    
      <.spinner />
      <.spinner size="lg" variant="primary" />
      <.spinner label="Loading..." show_label={true} />
  """
  @spec spinner(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:size, :string, default: "md", values: ~w(xs sm md lg xl))
  attr(:variant, :string, default: "default", values: ~w(default primary secondary))
  attr(:label, :string, default: "Loading...")
  attr(:show_label, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def spinner(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         spinner_class <- build_spinner_class(validated_assigns),
         container_class <- build_container_class(validated_assigns) do
      assigns = validated_assigns
        |> assign(:spinner_class, spinner_class)
        |> assign(:container_class, container_class)
      render_spinner(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_size(assigns.size),
         :ok <- validate_variant(assigns.variant),
         :ok <- validate_label(assigns.label),
         :ok <- validate_show_label(assigns.show_label) do
      {:ok, assigns}
    end
  end

  @spec validate_size(size :: String.t()) :: :ok | {:error, String.t()}
  defp validate_size(size) do
    if size in ~w(xs sm md lg xl) do
      :ok
    else
      {:error, "Invalid size. Must be one of: xs, sm, md, lg, xl"}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(default primary secondary) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: default, primary, secondary"}
    end
  end

  @spec validate_label(label :: String.t()) :: :ok | {:error, String.t()}
  defp validate_label(label) do
    if is_binary(label) do
      :ok
    else
      {:error, "Label must be a string"}
    end
  end

  @spec validate_show_label(show_label :: boolean()) :: :ok | {:error, String.t()}
  defp validate_show_label(show_label) do
    if is_boolean(show_label) do
      :ok
    else
      {:error, "Show label must be a boolean value"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_spinner_class(assigns :: assigns()) :: String.t()
  defp build_spinner_class(assigns) do
    base = spinner_color(assigns.variant)
    size = size_classes(assigns.size)

    [base, size]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec build_container_class(assigns :: assigns()) :: String.t()
  defp build_container_class(assigns) do
    base = "inline-flex items-center gap-2"

    [base, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec spinner_color(variant :: String.t()) :: String.t()
  defp spinner_color(variant) do
    case variant do
      "default" -> "text-primary"
      "primary" -> "text-primary"
      "secondary" -> "text-secondary"
      _ -> "text-primary"
    end
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "xs" -> "h-3 w-3"
      "sm" -> "h-4 w-4"
      "md" -> "h-5 w-5"
      "lg" -> "h-6 w-6"
      "xl" -> "h-8 w-8"
      _ -> "h-5 w-5"
    end
  end

  @spec label_class(size :: String.t()) :: String.t()
  defp label_class(size) do
    case size do
      "xs" -> "text-xs"
      "sm" -> "text-sm"
      "md" -> "text-sm"
      "lg" -> "text-base"
      "xl" -> "text-lg"
      _ -> "text-sm"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_spinner(assigns) do
    ~H"""
    <div class={@container_class} {@rest}>
      <div class={@spinner_class}>
        <svg class="w-full h-full animate-spin" fill="none" viewBox="0 0 24 24">
          <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
          <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
        </svg>
      </div>
      <%= if @show_label do %>
        <span class={label_class(@size)}><%= @label %></span>
      <% end %>
    </div>
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback spinner or error state
    IO.puts("Spinner error: #{reason}")
    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end
