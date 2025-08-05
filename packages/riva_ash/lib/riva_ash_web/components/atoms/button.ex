defmodule RivaAshWeb.Components.Atoms.Button do
  @moduledoc """
  Deprecated wrapper around the canonical design-system button.

  Use RivaAshWeb.Components.UI.Button.button/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.
  
  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Button, as: UIButton

  @type assigns :: map()
  @type type :: String.t()
  @type variant :: :primary | :secondary | :outline | :ghost | :link
  @type size :: :sm | :md | :lg
  @type loading :: boolean()
  @class :: String.t()
  @disabled :: boolean()

  @doc """
  Renders a button component.

  Backwards-compatible API: maps legacy variant/size to UI.Button API.
  
  ## Examples
    
      <.button variant="primary">Click me</.button>
      <.button size="lg" loading={true}>Loading...</.button>
      <.button variant="outline" disabled={true}>Disabled</.button>
  """
  @spec button(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :type, :string, default: "button"
  attr :variant, :string, default: "primary", values: ~w(primary secondary outline ghost link)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :rest, :global, include: ~w(phx-click phx-disable-with phx-value phx-value-id)
  slot :inner_block, required: true

  def button(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         ui_variant <- map_variant(validated_assigns.variant),
         ui_size <- legacy_size(validated_assigns.size) do
      assigns = validated_assigns
        |> assign(:ui_variant, ui_variant)
        |> assign(:ui_size, ui_size)
      render_button(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_type(assigns.type),
         :ok <- validate_variant(assigns.variant),
         :ok <- validate_size(assigns.size),
         :ok <- validate_loading(assigns.loading),
         :ok <- validate_disabled(assigns.disabled) do
      {:ok, assigns}
    end
  end

  @spec validate_type(type :: String.t()) :: :ok | {:error, String.t()}
  defp validate_type(type) do
    if type in ~w(button submit reset) do
      :ok
    else
      {:error, "Invalid button type. Must be one of: button, submit, reset"}
    end
  end

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(primary secondary outline ghost link) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: primary, secondary, outline, ghost, link"}
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

  @spec validate_loading(loading :: boolean()) :: :ok | {:error, String.t()}
  defp validate_loading(loading) do
    if is_boolean(loading) do
      :ok
    else
      {:error, "Loading must be a boolean value"}
    end
  end

  @spec validate_disabled(disabled :: boolean()) :: :ok | {:error, String.t()}
  defp validate_disabled(disabled) do
    if is_boolean(disabled) do
      :ok
    else
      {:error, "Disabled must be a boolean value"}
    end
  end

  # Functional Core: Pure mapping functions
  @spec map_variant(variant :: String.t()) :: String.t()
  defp map_variant(variant) do
    case variant do
      "primary" -> "default"
      "secondary" -> "secondary"
      "outline" -> "outline"
      "ghost" -> "ghost"
      "link" -> "link"
      _ -> "default"
    end
  end

  @spec legacy_size(size :: String.t()) :: String.t()
  defp legacy_size(size) do
    case size do
      "sm" -> "sm"
      "md" -> "default"
      "lg" -> "lg"
      _ -> "default"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_button(assigns) do
    ~H"""
    <UIButton.button
      type={@type}
      variant={@ui_variant}
      size={@ui_size}
      loading={@loading}
      class={@class}
      disabled={@disabled}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </UIButton.button>
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback button or error state
    IO.puts("Button error: #{reason}")
    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= reason %></span>
    </div>
    """
  end
end
