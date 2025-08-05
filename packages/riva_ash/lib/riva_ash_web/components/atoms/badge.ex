defmodule RivaAshWeb.Components.Atoms.Badge do
  @moduledoc """
  Deprecated wrapper around the canonical design-system badge.

  Use RivaAshWeb.Components.UI.Badge.badge/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.

  Note: Icon functionality is not supported in the canonical UI.Badge and will be ignored.
  
  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Badge, as: UIBadge

  @type assigns :: map()
  @type variant :: :default | :secondary | :success | :warning | :destructive | :outline
  @type size :: :sm | :md | :lg
  @type icon :: atom() | nil
  @type icon_position :: :left | :right
  @type pill :: boolean()

  @doc """
  Renders a badge component.

  Backwards-compatible API: maps legacy size to UI.Badge API.
  Note: icon, icon_position, and pill attributes are ignored as they're not supported in UI.Badge.
  
  ## Examples
    
      <.badge variant="success">Active</.badge>
      <.badge size="lg" variant="outline">Pending</.badge>
  """
  @spec badge(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :variant, :string, default: "default", values: ~w(default secondary success warning destructive outline)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :icon, :atom, default: nil
  attr :icon_position, :string, default: "left", values: ~w(left right)
  attr :pill, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def badge(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         ui_size <- map_legacy_size(validated_assigns.size) do
      assigns = validated_assigns |> assign(:ui_size, ui_size)
      render_badge(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_icon(assigns.icon),
         :ok <- validate_icon_position(assigns.icon_position) do
      {:ok, assigns}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(default secondary success warning destructive outline) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: default, secondary, success, warning, destructive, outline"}
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

  @spec validate_icon(icon :: atom() | nil) :: :ok | {:error, String.t()}
  defp validate_icon(icon) do
    if is_nil(icon) or is_atom(icon) do
      :ok
    else
      {:error, "Icon must be an atom or nil"}
    end
  end

  @spec validate_icon_position(icon_position :: String.t()) :: :ok | {:error, String.t()}
  defp validate_icon_position(icon_position) do
    if icon_position in ~w(left right) do
      :ok
    else
      {:error, "Icon position must be one of: left, right"}
    end
  end

  # Functional Core: Pure mapping functions
  @spec map_legacy_size(size :: String.t()) :: String.t()
  defp map_legacy_size(size) do
    case size do
      "sm" -> "sm"
      "md" -> "default"
      "lg" -> "lg"
      _ -> "default"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_badge(assigns) do
    ~H"""
    <UIBadge.badge
      variant={@variant}
      size={@ui_size}
      class={@class}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </UIBadge.badge>
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback badge or error state
    IO.puts("Badge error: #{reason}")
    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end
