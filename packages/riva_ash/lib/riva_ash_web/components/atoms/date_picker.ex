defmodule RivaAshWeb.Components.Atoms.DatePicker do
  @moduledoc """
  Date picker component with calendar popup.
  """
  use Phoenix.Component
  import RivaAshWeb.CoreComponents

  @type assigns :: %{
          optional(:field) => Phoenix.HTML.FormField.t(),
          optional(:value) => String.t(),
          optional(:min_date) => String.t(),
          optional(:max_date) => String.t(),
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
  Renders a date picker input with validation and accessibility.
  """
  @spec date_picker(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:value, :string, default: nil)
  attr(:min_date, :string, default: nil)
  attr(:max_date, :string, default: nil)
  attr(:placeholder, :string, default: "Select date")
  attr(:disabled, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:format, :string, default: "yyyy-mm-dd")
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @impl true
  def date_picker(assigns) do
    assigns
    |> build_date_picker_attrs()
    |> render_date_picker()
  end

  @spec build_date_picker_attrs(assigns :: assigns()) :: assigns()
  defp build_date_picker_attrs(assigns) do
    default_min_date = Application.get_env(:riva_ash, :date_picker_min_date, "1900-01-01")
    default_max_date = Application.get_env(:riva_ash, :date_picker_max_date, "2100-12-31")

    assigns
    |> Map.put_new(:min_date, default_min_date)
    |> Map.put_new(:max_date, default_max_date)
    |> validate_date_picker_attrs()
  end

  @spec validate_date_picker_attrs(assigns :: assigns()) :: assigns()
  defp validate_date_picker_attrs(assigns) do
    with :ok <- validate_date_format(assigns[:format]),
         :ok <- validate_date_range(assigns[:min_date], assigns[:max_date]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid date picker attributes: #{reason}"
    end
  end

  @spec validate_date_format(String.t()) :: :ok | {:error, String.t()}
  defp validate_date_format("yyyy-mm-dd"), do: :ok
  defp validate_date_format("mm/dd/yyyy"), do: :ok
  defp validate_date_format("dd-mm-yyyy"), do: :ok
  defp validate_date_format(_), do: {:error, "Unsupported date format"}

  @spec validate_date_range(String.t(), String.t()) :: :ok | {:error, String.t()}
  defp validate_date_range(min_date, max_date) do
    with {:ok, min} <- Date.from_iso8601(min_date),
         {:ok, max} <- Date.from_iso8601(max_date),
         :lt <- Date.compare(min, max) do
      :ok
    else
      {:error, _} -> {:error, "Invalid date format for min_date or max_date"}
      :gt -> {:error, "min_date must be before max_date"}
      :eq -> {:error, "min_date must be before max_date"}
    end
  end

  @spec render_date_picker(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_date_picker(assigns) do
    ~H"""
    <div class="date-picker-container #{@class}">
      <input
        type="date"
        id={@id || "date-picker-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"}
        name={@name || @field.name}
        value={@value || @field.value}
        min={@min_date}
        max={@max_date}
        placeholder={@placeholder}
        disabled={@disabled}
        required={@required}
        class={build_input_class(@size, @variant, @field)}
        phx-hook="DatePicker"
        {@rest}
      />
      <label for={@id || "date-picker-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"} class="sr-only">
        <%= @placeholder %>
      </label>
    </div>
    """
  end

  @spec build_input_class(String.t(), String.t(), Phoenix.HTML.FormField.t()) :: String.t()
  defp build_input_class(size, variant, field) do
    base_classes = "w-full rounded-md border px-3 py-2 shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-500"

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

    error_classes = if field && field.errors != [], "border-red-500", ""

    "#{base_classes} #{size_classes} #{variant_classes} #{error_classes}"
  end
end
