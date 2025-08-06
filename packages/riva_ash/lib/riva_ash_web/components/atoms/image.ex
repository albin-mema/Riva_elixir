defmodule RivaAshWeb.Components.Atoms.Image do
  @moduledoc """
  Image component for displaying images with responsive and accessibility features.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type variant :: :default | :rounded | :circle | :bordered
  @type size :: :xs | :sm | :md | :lg | :xl
  @type fit :: :contain | :cover | :fill | :none | :scale - down
  @type loading :: :lazy | :eager

  @doc """
  Renders an image component with different variants and responsive options.

  ## Examples

      <.image src="/path/to/image.jpg" alt="Description" />

      <.image src="/path/to/image.jpg" variant="rounded" size="lg" fit="cover" />

      <.image src="/path/to/image.jpg" variant="circle" size="xl" loading="lazy">
        <.image_fallback>
          <span class="text-gray-500">No image</span>
        </.image_fallback>
      </.image>
  """
  @spec image(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :src, :string, required: true
  attr :alt, :string, required: true
  attr :variant, :string, default: "default", values: ~w(default rounded circle bordered)
  attr :size, :string, default: "md", values: ~w(xs sm md lg xl)
  attr :fit, :string, default: "cover", values: ~w(contain cover fill none scale-down)
  attr :loading, :string, default: "lazy", values: ~w(lazy eager)
  attr :class, :string, default: ""
  attr :rest, :global

  slot :fallback

  def image(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         image_class <- build_image_class(validated_assigns) do
      assigns = validated_assigns |> assign(:image_class, image_class)
      render_image(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_src(assigns.src),
         :ok <- validate_alt(assigns.alt),
         :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_fit(assigns.fit),
         :ok <- validate_loading(assigns.loading) do
      {:ok, assigns}
    end
  end

  @spec validate_src(src :: String.t()) :: :ok | {:error, String.t()}
  defp validate_src(src) do
    if String.trim(src) != "" do
      :ok
    else
      {:error, "Image source cannot be empty"}
    end
  end

  @spec validate_alt(alt :: String.t()) :: :ok | {:error, String.t()}
  defp validate_alt(alt) do
    if String.trim(alt) != "" do
      :ok
    else
      {:error, "Alt text cannot be empty"}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(default rounded circle bordered) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: default, rounded, circle, bordered"}
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

  @spec validate_fit(fit :: String.t()) :: :ok | {:error, String.t()}
  defp validate_fit(fit) do
    if fit in ~w(contain cover fill none scale-down) do
      :ok
    else
      {:error, "Invalid fit. Must be one of: contain, cover, fill, none, scale-down"}
    end
  end

  @spec validate_loading(loading :: String.t()) :: :ok | {:error, String.t()}
  defp validate_loading(loading) do
    if loading in ~w(lazy eager) do
      :ok
    else
      {:error, "Invalid loading. Must be one of: lazy, eager"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_image_class(assigns :: assigns()) :: String.t()
  defp build_image_class(assigns) do
    base = base_classes(assigns.variant)
    size = size_classes(assigns.size)
    fit = fit_classes(assigns.fit)

    [base, size, fit, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(variant :: String.t()) :: String.t()
  defp base_classes(variant) do
    case variant do
      "default" -> ""
      "rounded" -> "rounded-lg"
      "circle" -> "rounded-full"
      "bordered" -> "border-2 border-gray-300"
      _ -> ""
    end
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "xs" -> "w-16 h-16"
      "sm" -> "w-24 h-24"
      "md" -> "w-32 h-32"
      "lg" -> "w-48 h-48"
      "xl" -> "w-64 h-64"
      _ -> "w-32 h-32"
    end
  end

  @spec fit_classes(fit :: String.t()) :: String.t()
  defp fit_classes(fit) do
    case fit do
      "contain" -> "object-contain"
      "cover" -> "object-cover"
      "fill" -> "object-fill"
      "none" -> "object-none"
      "scale-down" -> "object-scale-down"
      _ -> "object-cover"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_image(assigns) do
    ~H"""
    <div class="relative inline-block">
      <img
        src={@src}
        alt={@alt}
        class={@image_class}
        loading={@loading}
        {@rest}
      />
      <%= if @fallback do %>
        <.image_fallback>
          <%= render_slot(@fallback) %>
        </.image_fallback>
      <% end %>
    </div>
    """
  end

  # Image Fallback
  attr :class, :string, default: ""

  defp image_fallback(assigns) do
    ~H"""
    <div class={["absolute inset-0 flex items-center justify-center bg-gray-100", @class]}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback image or error state
    IO.puts("Image error: #{reason}")

    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end
