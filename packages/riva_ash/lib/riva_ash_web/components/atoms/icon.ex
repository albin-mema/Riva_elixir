alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.HTML, as: HTML

defmodule RivaAshWeb.Components.Atoms.Icon do
  @moduledoc """
  Icon component that provides a consistent interface for rendering icons.
  Uses the heroicons library for Phoenix LiveView.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type name :: :building_office_2 | :plus | :x_mark | :check | :home | :cog | :magnifying_glass | :user | atom()
  @type variant :: :outline | :solid | :mini | :micro
  @type size :: :xs | :sm | :md | :lg | :xl

  @doc """
  Renders an icon with consistent styling.

  ## Examples

      <.icon name={:home} />
      <.icon name={:home} variant="solid" size="lg" class="text-primary" />
      <.icon name={:cog} variant="outline" size="sm" />
  """
  @spec icon(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:name, :atom, required: true)
  attr(:variant, :string, default: "outline", values: ~w(outline solid mini micro))
  attr(:size, :string, default: "md", values: ~w(xs sm md lg xl))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def icon(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         icon_class <- build_icon_class(validated_assigns) do
      assigns = validated_assigns |> assign(:icon_class, icon_class)
      render_icon(assigns)
    else
      {:error, reason} -> render_error(%{reason: reason})
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_name(assigns.name),
         :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size) do
      {:ok, assigns}
    end
  end

  @spec validate_name(name :: atom()) :: :ok | {:error, String.t()}
  defp validate_name(name) do
    if is_atom(name) do
      :ok
    else
      {:error, "Icon name must be an atom"}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(outline solid mini micro) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: outline, solid, mini, micro"}
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

  # Functional Core: Pure class building functions
  @spec build_icon_class(assigns :: assigns()) :: String.t()
  defp build_icon_class(assigns) do
    size_classes = size_class(assigns.size)
    variant_classes = variant_class(assigns.variant)

    [size_classes, variant_classes, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec size_class(size :: String.t()) :: String.t()
  defp size_class(size) do
    case size do
      "xs" -> "[&>svg]:h-3 [&>svg]:w-3"
      "sm" -> "[&>svg]:h-4 [&>svg]:w-4"
      "md" -> "[&>svg]:h-5 [&>svg]:w-5"
      "lg" -> "[&>svg]:h-6 [&>svg]:w-6"
      "xl" -> "[&>svg]:h-8 [&>svg]:w-8"
      _ -> "[&>svg]:h-5 [&>svg]:w-5"
    end
  end

  @spec variant_class(variant :: String.t()) :: String.t()
  defp variant_class(variant) do
    case variant do
      "outline" -> "stroke-current"
      "solid" -> "fill-current stroke-none"
      "mini" -> "stroke-current"
      "micro" -> "stroke-current"
      _ -> "stroke-current"
    end
  end

  # Functional Core: Pure icon path rendering functions
  @spec render_icon_path(name :: atom()) :: Phoenix.HTML.raw()
  defp render_icon_path(name) do
    case name do
      :building_office_2 ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4" />)
        )

      :plus ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4" />)
        )

      :x_mark ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />)
        )

      :check ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />)
        )

      :home ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6" />)
        )

      :cog ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" /><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />)
        )

      :magnifying_glass ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />)
        )

      :user ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />)
        )

      _ ->
        Phoenix.HTML.raw(
          ~s(<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />)
        )
    end
  end

  # Imperative Shell: Rendering functions
  defp render_icon(assigns) do
    ~H"""
    <span class={@icon_class} {@rest}>
      <svg class="w-full h-full" fill="none" viewBox="0 0 24 24" stroke="currentColor">
        <%= render_icon_path(@name) %>
      </svg>
    </span>
    """
  end

  defp render_error(assigns) do
    # In a real implementation, you might want to log this error
    # and render a fallback icon or error state
    reason = Map.get(assigns, :reason, "Invalid icon parameters")
    IO.puts("Icon error: #{reason}")

    assigns = %{reason: reason}

    ~H"""
    <div class="relative bg-red-100 px-4 py-3 border border-red-400 rounded text-red-700">
      <span class="block sm:inline">Error: <%= @reason %></span>
    </div>
    """
  end
end
