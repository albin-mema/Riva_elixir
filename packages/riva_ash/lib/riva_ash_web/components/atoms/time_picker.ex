defmodule RivaAshWeb.Components.Atoms.TimePicker do
  @moduledoc """
  Time picker component for time selection with validation and accessibility.
  """
  use Phoenix.Component
  import RivaAshWeb.CoreComponents

  @type assigns :: %{
          optional(:field) => Phoenix.HTML.FormField.t(),
          optional(:value) => String.t(),
          optional(:min_time) => String.t(),
          optional(:max_time) => String.t(),
          optional(:step) => integer(),
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
  Renders a time picker input with validation and accessibility.
  """
  @spec time_picker(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:value, :string, default: nil)
  attr(:min_time, :string, default: nil)
  attr(:max_time, :string, default: nil)
  attr(:step, :integer, default: 60)
  attr(:placeholder, :string, default: "Select time")
  attr(:disabled, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:format, :string, default: "24", values: ~w(12 24))
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @impl true
  def time_picker(assigns) do
    assigns
    |> build_time_picker_attrs()
    |> render_time_picker()
  end

  @spec build_time_picker_attrs(assigns :: assigns()) :: assigns()
  defp build_time_picker_attrs(assigns) do
    default_step = Application.get_env(:riva_ash, :time_picker_step, 60)
    default_format = Application.get_env(:riva_ash, :time_picker_format, "24")

    assigns
    |> Map.put_new(:step, default_step)
    |> Map.put_new(:format, default_format)
    |> validate_time_picker_attrs()
  end

  @spec validate_time_picker_attrs(assigns :: assigns()) :: assigns()
  defp validate_time_picker_attrs(assigns) do
    with :ok <- validate_step_value(assigns[:step]),
         :ok <- validate_time_format(assigns[:format]),
         :ok <- validate_time_range(assigns[:min_time], assigns[:max_time]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid time picker attributes: #{reason}"
    end
  end

  @spec validate_step_value(integer()) :: :ok | {:error, String.t()}
  defp validate_step_value(step) when step in [15, 30, 60, 300, 600, 900], do: :ok
  defp validate_step_value(_), do: {:error, "Step must be one of: 15, 30, 60, 300, 600, 900"}

  @spec validate_time_format(String.t()) :: :ok | {:error, String.t()}
  defp validate_time_format("12"), do: :ok
  defp validate_time_format("24"), do: :ok
  defp validate_time_format(_), do: {:error, "Format must be either '12' or '24'"}

  @spec validate_time_range(String.t(), String.t()) :: :ok | {:error, String.t()}
  defp validate_time_range(nil, nil), do: :ok
  defp validate_time_range(min_time, max_time) do
    with {:ok, min} <- parse_time(min_time),
         {:ok, max} <- parse_time(max_time),
         :lt <- Time.compare(min, max) do
      :ok
    else
      {:error, _} -> {:error, "Invalid time format for min_time or max_time"}
      :gt -> {:error, "min_time must be before max_time"}
      :eq -> {:error, "min_time must be before max_time"}
    end
  end

  @spec parse_time(String.t()) :: {:ok, Time.t()} | {:error, String.t()}
  defp parse_time(time_str) do
    case Time.from_iso8601(time_str) do
      {:ok, time} -> {:ok, time}
      {:error, _} ->
        # Try alternative time formats
        case String.split(time_str, ":") do
          [h, m] when byte_size(h) <= 2 and byte_size(m) <= 2 ->
            case {Integer.parse(h), Integer.parse(m)} do
              {{hour, ""}, {minute, ""}} when hour >= 0 and hour < 24 and minute >= 0 and minute < 60 ->
                {:ok, %Time{hour: hour, minute: minute, second: 0}}
              _ -> {:error, "Invalid time format"}
            end
          _ -> {:error, "Invalid time format"}
        end
    end
  end

  @spec render_time_picker(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_time_picker(assigns) do
    ~H"""
    <div class="time-picker-container #{@class}">
      <input
        type="time"
        id={@id || "time-picker-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"}
        name={@name || @field.name}
        value={@value || @field.value}
        min={@min_time}
        max={@max_time}
        step={@step}
        placeholder={@placeholder}
        disabled={@disabled}
        required={@required}
        class={build_input_class(@size, @variant, @field)}
        phx-hook="TimePicker"
        {@rest}
      />
      <label for={@id || "time-picker-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"} class="sr-only">
        <%= @placeholder %>
      </label>
    </div>
    """
  end

  @spec build_input_class(String.t(), String.t(), Phoenix.HTML.FormField.t()) :: String.t()
  defp build_input_class(size, variant, field) do
    base_classes = "w-full rounded-md border px-3 py-2 shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
    
    size_classes = case size do
      "sm" -> "text-sm"
      "lg" -> "text-lg"
      _ -> "text-base"
    end

    variant_classes = case variant do
      "error" -> "border-red-300 focus:border-red-500 focus:ring-red-500"
      "success" -> "border-green-300 focus:border-green-500 focus:ring-green-500"
      _ -> "border-gray-300 focus:border-blue-500 focus:ring-blue-500"
    end

    error_classes = if field && field.errors != [], do: "border-red-500", else: ""

    "#{base_classes} #{size_classes} #{variant_classes} #{error_classes}"
  end
end
