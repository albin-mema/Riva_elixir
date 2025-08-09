alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.HTML.FormField, as: FormField
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.HTML, as: HTML

defmodule RivaAshWeb.Components.Atoms.ColorPicker do
  @moduledoc """
  Color picker component for color selection with validation and accessibility.
  """
  use Phoenix.Component
  import RivaAshWeb.CoreComponents

  @type assigns :: %{
          optional(:field) => Phoenix.HTML.FormField.t(),
          optional(:value) => String.t(),
          optional(:default_color) => String.t(),
          optional(:disabled_colors) => list(String.t()),
          optional(:placeholder) => String.t(),
          optional(:disabled) => boolean(),
          optional(:required) => boolean(),
          optional(:format) => String.t(),
          optional(:size) => String.t(),
          optional(:variant) => String.t(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a color picker input with validation and accessibility.
  """
  @spec color_picker(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:value, :string, default: nil)
  attr(:default_color, :string, default: "#3b82f6")
  attr(:disabled_colors, :list, default: [])
  attr(:placeholder, :string, default: "Select color")
  attr(:disabled, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:format, :string, default: "hex", values: ~w(hex rgb hsl))
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @impl true
  def color_picker(assigns) do
    assigns
    |> build_color_picker_attrs()
    |> render_color_picker()
  end

  @spec build_color_picker_attrs(assigns :: assigns()) :: assigns()
  defp build_color_picker_attrs(assigns) do
    default_format = Application.get_env(:riva_ash, :color_picker_format, "hex")
    default_color = Application.get_env(:riva_ash, :color_picker_default, "#3b82f6")

    assigns
    |> Map.put_new(:format, default_format)
    |> Map.put_new(:default_color, default_color)
    |> validate_color_picker_attrs()
  end

  @spec validate_color_picker_attrs(assigns :: assigns()) :: assigns()
  defp validate_color_picker_attrs(assigns) do
    with :ok <- validate_color_format(assigns[:format]),
         :ok <- validate_default_color(assigns[:default_color]),
         :ok <- validate_disabled_colors(assigns[:disabled_colors]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid color picker attributes: #{reason}"
    end
  end

  @spec validate_color_format(String.t()) :: :ok | {:error, String.t()}
  defp validate_color_format("hex"), do: :ok
  defp validate_color_format("rgb"), do: :ok
  defp validate_color_format("hsl"), do: :ok
  defp validate_color_format(_), do: {:error, "Format must be one of: hex, rgb, hsl"}

  @spec validate_default_color(String.t()) :: :ok | {:error, String.t()}
  defp validate_default_color(color) do
    case parse_color(color) do
      {:ok, _unmatched} -> :ok
      {:error, _unmatched} -> {:error, "Invalid default color format"}
    end
  end

  @spec validate_disabled_colors(list(String.t())) :: :ok | {:error, String.t()}
  defp validate_disabled_colors(colors) do
    Enum.reduce_while(colors, :ok, fn color, :ok ->
      case parse_color(color) do
        {:ok, _unmatched} -> {:cont, :ok}
        {:error, _unmatched} -> {:halt, {:error, "Invalid color in disabled_colors list"}}
      end
    end)
  end

  @spec parse_color(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp parse_color(color) do
    cond do
      String.starts_with?(color, "#") and String.length(color) in [4, 7] ->
        validate_hex_color(color)

      String.starts_with?(color, "rgb") ->
        validate_rgb_color(color)

      String.starts_with?(color, "hsl") ->
        validate_hsl_color(color)

      true ->
        {:error, "Unsupported color format"}
    end
  end

  @spec validate_hex_color(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp validate_hex_color("#" <> hex) do
    case Integer.parse(hex, 16) do
      {_, ""} when byte_size(hex) in [3, 6] -> {:ok, %{format: "hex", value: hex}}
      _ -> {:error, "Invalid hex color"}
    end
  end

  defp validate_hex_color(_), do: {:error, "Invalid hex color format"}

  @spec validate_rgb_color(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp validate_rgb_color("rgb(" <> rest) do
    case String.split(rest, ")") do
      [values, ""] ->
        parts = String.split(values, ",")

        case parse_rgb_parts(parts) do
          {:ok, rgb} -> {:ok, %{format: "rgb", value: rgb}}
          {:error, _} -> {:error, "Invalid RGB values"}
        end

      _ ->
        {:error, "Invalid RGB format"}
    end
  end

  defp validate_rgb_color(_), do: {:error, "Invalid RGB format"}

  @spec parse_rgb_parts(list(String.t())) :: {:ok, String.t()} | {:error, String.t()}
  defp parse_rgb_parts([r, g, b]) do
    with {r_val, ""} <- Integer.parse(String.trim(r)),
         {g_val, ""} <- Integer.parse(String.trim(g)),
         {b_val, ""} <- Integer.parse(String.trim(b)),
         true <- r_val >= 0 and r_val <= 255,
         true <- g_val >= 0 and g_val <= 255,
         true <- b_val >= 0 and b_val <= 255 do
      {:ok, "rgb(#{r_val}, #{g_val}, #{b_val})"}
    else
      _ -> {:error, "Invalid RGB values"}
    end
  end

  defp parse_rgb_parts(_), do: {:error, "Invalid RGB parts"}

  @spec validate_hsl_color(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp validate_hsl_color("hsl(" <> rest) do
    case String.split(rest, ")") do
      [values, ""] ->
        parts = String.split(values, ",")

        case parse_hsl_parts(parts) do
          {:ok, hsl} -> {:ok, %{format: "hsl", value: hsl}}
          {:error, _} -> {:error, "Invalid HSL values"}
        end

      _ ->
        {:error, "Invalid HSL format"}
    end
  end

  defp validate_hsl_color(_), do: {:error, "Invalid HSL format"}

  @spec parse_hsl_parts(list(String.t())) :: {:ok, String.t()} | {:error, String.t()}
  defp parse_hsl_parts([h, s, l]) do
    with {h_val, ""} <- Integer.parse(String.trim(h)),
         {s_val, "%"} <- Integer.parse(String.trim(s)),
         {l_val, "%"} <- Integer.parse(String.trim(l)),
         true <- h_val >= 0 and h_val <= 360,
         true <- s_val >= 0 and s_val <= 100,
         true <- l_val >= 0 and l_val <= 100 do
      {:ok, "hsl(#{h_val}, #{s_val}%, #{l_val}%)"}
    else
      _ -> {:error, "Invalid HSL values"}
    end
  end

  defp parse_hsl_parts(_), do: {:error, "Invalid HSL parts"}

  @spec render_color_picker(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_color_picker(assigns) do
    ~H"""
    <div class="color-picker-container #{@class}">
      <div class="flex items-center gap-2">
        <input
          type="color"
          id={@id || "color-picker-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"}
          name={@name || @field.name}
          value={@value || @default_color}
          placeholder={@placeholder}
          disabled={@disabled}
          required={@required}
          class={build_input_class(@size, @variant, @field)}
          phx-hook="ColorPicker"
          {@rest}
        />
        <input
          type="text"
          id={"color-text-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"}
          value={@value || @default_color}
          placeholder={@placeholder}
          disabled={@disabled}
          required={@required}
          class="color-text-input #{build_text_input_class(@size, @variant, @field)}"
          phx-hook="ColorTextInput"
          data-format={@format}
          data-disabled-colors={Jason.encode!(@disabled_colors)}
        />
      </div>
      <label for={@id || "color-picker-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"} class="sr-only">
        <%= @placeholder %>
      </label>
    </div>
    """
  end

  @spec build_input_class(String.t(), String.t(), Phoenix.HTML.FormField.t()) :: String.t()
  defp build_input_class(size, variant, field) do
    base_classes = "w-12 h-12 rounded-md border cursor-pointer focus:outline-none focus:ring-2 focus:ring-blue-500"

    size_classes =
      case size do
        "sm" -> "w-10 h-10"
        "lg" -> "w-14 h-14"
        _ -> "w-12 h-12"
      end

    variant_classes =
      case variant do
        "error" -> "border-red-300 focus:border-red-500 focus:ring-red-500"
        "success" -> "border-green-300 focus:border-green-500 focus:ring-green-500"
        _ -> "border-gray-300 focus:border-blue-500 focus:ring-blue-500"
      end

    error_classes = if field && field.errors != [], do: "border-red-500", else: ""

    "#{base_classes} #{size_classes} #{variant_classes} #{error_classes}"
  end

  @spec build_text_input_class(String.t(), String.t(), Phoenix.HTML.FormField.t()) :: String.t()
  defp build_text_input_class(size, variant, field) do
    base_classes = "flex-1 rounded-md border px-3 py-2 shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-500"

    size_classes =
      case size do
        "sm" -> "text-sm"
        "lg" -> "text-lg"
        _ -> "text-base"
      end

    variant_classes =
      case variant do
        "error" -> "border-red-300 focus:border-red-500 focus:ring-red-500"
        "success" -> "border-green-300 focus:border-green-500 focus:ring-green-500"
        _ -> "border-gray-300 focus:border-blue-500 focus:ring-blue-500"
      end

    error_classes = if field && field.errors != [], do: "border-red-500", else: ""

    "#{base_classes} #{size_classes} #{variant_classes} #{error_classes}"
  end
end
