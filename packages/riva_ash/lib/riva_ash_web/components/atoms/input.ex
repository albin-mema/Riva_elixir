alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Components.UI.Input, as: Input
alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.HTML.FormField, as: FormField
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.HTML, as: HTML

defmodule RivaAshWeb.Components.Atoms.Input do
  @moduledoc """
  Deprecated wrapper around the canonical design-system input.

  Use RivaAshWeb.Components.UI.Input.input/1 directly in new code.
  This module delegates to the canonical component to maintain backward compatibility.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Input, as: UIInput

  @type assigns :: map()
  @type type :: String.t()
  @type field :: Phoenix.HTML.FormField.t() | nil
  @type value :: String.t() | nil
  @type placeholder :: String.t()
  @type disabled :: boolean()
  @type readonly :: boolean()
  @type required :: boolean()
  @type size :: :sm | :md | :lg
  @type variant :: :default | :error | :success

  @doc """
  Renders an input component.

  Backwards-compatible API: maps legacy size to UI.Input API.

  ## Examples

      <.input type="text" placeholder="Enter your name" />
      <.input type="email" required={true} variant="error" />
      <.input type="password" size="lg" />
  """
  @spec input(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :type, :string, default: "text"
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :value, :string, default: nil
  attr :placeholder, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :readonly, :boolean, default: false
  attr :required, :boolean, default: false
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :class, :string, default: ""
  attr :rest, :global

  def input(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         ui_size <- map_legacy_size(validated_assigns.size) do
      assigns = validated_assigns |> assign(:ui_size, ui_size)
      render_input(assigns)
    else
      {:error, reason} -> render_error(reason)
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_type(assigns.type),
         :ok <- validate_field(assigns.field),
         :ok <- validate_value(assigns.value),
         :ok <- validate_placeholder(assigns.placeholder),
         :ok <- validate_disabled(assigns.disabled),
         :ok <- validate_readonly(assigns.readonly),
         :ok <- validate_required(assigns.required),
         :ok <- validate_size(assigns.size),
         :ok <- validate_variant(assigns.variant) do
      {:ok, assigns}
    end
  end

  @spec validate_type(type :: String.t()) :: :ok | {:error, String.t()}
  defp validate_type(type) do
    valid_types = ~w(text email password number tel url search color date time datetime-local month week)

    if type in valid_types do
      :ok
    else
      {:error, "Invalid input type. Must be one of: #{Enum.join(valid_types, ", ")}"}
    end
  end

  @spec validate_field(field :: Phoenix.HTML.FormField.t() | nil) :: :ok | {:error, String.t()}
  defp validate_field(field) do
    if is_nil(field) or (is_map(field) and function_exported?(field, :__struct__, 0)) do
      :ok
    else
      {:error, "Field must be a Phoenix.HTML.FormField or nil"}
    end
  end

  @spec validate_value(value :: String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_value(value) do
    if is_nil(value) or is_binary(value) do
      :ok
    else
      {:error, "Value must be a string or nil"}
    end
  end

  @spec validate_placeholder(placeholder :: String.t()) :: :ok | {:error, String.t()}
  defp validate_placeholder(placeholder) do
    if is_binary(placeholder) do
      :ok
    else
      {:error, "Placeholder must be a string"}
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

  @spec validate_readonly(readonly :: boolean()) :: :ok | {:error, String.t()}
  defp validate_readonly(readonly) do
    if is_boolean(readonly) do
      :ok
    else
      {:error, "Readonly must be a boolean value"}
    end
  end

  @spec validate_required(required :: boolean()) :: :ok | {:error, String.t()}
  defp validate_required(required) do
    if is_boolean(required) do
      :ok
    else
      {:error, "Required must be a boolean value"}
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

  @spec validate_variant(variant :: String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(variant) do
    if variant in ~w(default error success) do
      :ok
    else
      {:error, "Invalid variant. Must be one of: default, error, success"}
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
  defp render_input(assigns) do
    ~H"""
    <UIInput.input
      type={@type}
      field={@field}
      value={@value}
      placeholder={@placeholder}
      disabled={@disabled}
      readonly={@readonly}
      required={@required}
      variant={@variant}
      size={@ui_size}
      class={@class}
      {@rest}
    />
    """
  end

  defp render_error(reason) do
    # In a real implementation, you might want to log this error
    # and render a fallback input or error state
    IO.puts("Input error: #{reason}")

    assigns = %{reason: reason}

    ~H"""
    <div class="relative bg-red-100 px-4 py-3 border border-red-400 rounded text-red-700">
      <span class="block sm:inline">Error: <%= @reason %></span>
    </div>
    """
  end
end
