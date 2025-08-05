defmodule RivaAshWeb.Components.Molecules.StatusIndicator do
  @moduledoc """
  Status indicator component with colors and icons.

  Provides a configurable status indicator with appropriate styling,
  icons, and optional pulse animation for different status types.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Badge, as: UIBadge
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon

  @type status :: String.t()
  @type variant :: :auto | :success | :warning | :error | :info | :secondary | :default
  @type size :: :sm | :md | :lg
  @type status_config :: %{
          status: status(),
          label: String.t() | nil,
          show_icon: boolean(),
          show_pulse: boolean(),
          size: size(),
          variant: variant()
        }
  @type assigns :: %{
          required(:status) => status(),
          optional(:label) => String.t() | nil,
          optional(:show_icon) => boolean(),
          optional(:show_pulse) => boolean(),
          optional(:size) => size(),
          optional(:variant) => variant(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a status indicator with appropriate styling.

  ## Examples

      <.status_indicator
        status="online"
        label="Active"
        show_icon={true}
        show_pulse={false}
        size="md"
        variant="auto"
      />
  """
  @spec status_indicator(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:status, :string, required: true,
    doc: "Status to display")
  attr(:label, :string, default: nil,
    doc: "Optional label for the status")
  attr(:show_icon, :boolean, default: true,
    doc: "Whether to show an icon with the status")
  attr(:show_pulse, :boolean, default: false,
    doc: "Whether to show a pulse animation")
  attr(:size, :string, default: "md",
    values: ~w(sm md lg),
    doc: "Size of the status indicator")
  attr(:variant, :string, default: "auto",
    values: ~w(auto success warning error info secondary default),
    doc: "Visual variant of the status indicator")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the container")
  attr(:rest, :global)

  @impl true
  def status_indicator(assigns) do
    assigns
    |> build_status_indicator_attrs()
    |> validate_status_indicator_attrs()
    |> assign(:computed_variant, compute_variant(assigns.status, assigns.variant))
    |> assign(:status_icon, status_icon(assigns.status))
    |> render_status_indicator()
  end

  @spec build_status_indicator_attrs(assigns :: assigns()) :: assigns()
  defp build_status_indicator_attrs(assigns) do
    default_size = Application.get_env(:riva_ash, :status_indicator_size, "md")
    default_show_icon = Application.get_env(:riva_ash, :status_indicator_show_icon, true)
    default_show_pulse = Application.get_env(:riva_ash, :status_indicator_show_pulse, false)

    assigns
    |> Map.put_new(:size, default_size)
    |> Map.put_new(:show_icon, default_show_icon)
    |> Map.put_new(:show_pulse, default_show_pulse)
  end

  @spec validate_status_indicator_attrs(assigns :: assigns()) :: assigns()
  defp validate_status_indicator_attrs(assigns) do
    with :ok <- validate_status(assigns[:status]),
         :ok <- validate_label(assigns[:label]),
         :ok <- validate_show_icon(assigns[:show_icon]),
         :ok <- validate_show_pulse(assigns[:show_pulse]),
         :ok <- validate_size(assigns[:size]),
         :ok <- validate_variant(assigns[:variant]),
         :ok <- validate_class(assigns[:class]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid status indicator attributes: #{reason}"
    end
  end

  @spec validate_status(String.t()) :: :ok | {:error, String.t()}
  defp validate_status(status) when is_binary(status) and status != "", do: :ok
  defp validate_status(_), do: {:error, "status must be a non-empty string"}

  @spec validate_label(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_label(nil), do: :ok
  defp validate_label(label) when is_binary(label), do: :ok
  defp validate_label(_), do: {:error, "label must be a string or nil"}

  @spec validate_show_icon(boolean()) :: :ok | {:error, String.t()}
  defp validate_show_icon(show_icon) when is_boolean(show_icon), do: :ok
  defp validate_show_icon(_), do: {:error, "show_icon must be a boolean"}

  @spec validate_show_pulse(boolean()) :: :ok | {:error, String.t()}
  defp validate_show_pulse(show_pulse) when is_boolean(show_pulse), do: :ok
  defp validate_show_pulse(_), do: {:error, "show_pulse must be a boolean"}

  @spec validate_size(String.t()) :: :ok | {:error, String.t()}
  defp validate_size("sm"), do: :ok
  defp validate_size("md"), do: :ok
  defp validate_size("lg"), do: :ok
  defp validate_size(_), do: {:error, "size must be one of: sm, md, lg"}

  @spec validate_variant(String.t()) :: :ok | {:error, String.t()}
  defp validate_variant("auto"), do: :ok
  defp validate_variant("success"), do: :ok
  defp validate_variant("warning"), do: :ok
  defp validate_variant("error"), do: :ok
  defp validate_variant("info"), do: :ok
  defp validate_variant("secondary"), do: :ok
  defp validate_variant("default"), do: :ok
  defp validate_variant(_), do: {:error, "variant must be one of: auto, success, warning, error, info, secondary, default"}

  @spec validate_class(String.t()) :: :ok | {:error, String.t()}
  defp validate_class(class) when is_binary(class), do: :ok
  defp validate_class(_), do: {:error, "class must be a string"}

  @spec render_status_indicator(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_status_indicator(assigns) do
    # Render status indicator using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:badge_class, build_badge_class(assigns.size))
    |> render_status_indicator_component()
  end

  # Private helper for status indicator rendering
  @spec render_status_indicator_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_status_indicator_component(assigns) do
    ~H"""
    <div class={["status-indicator-container inline-flex items-center gap-1.5", @container_class]} {@rest}>
      <%= if @show_pulse do %>
        <.render_pulse variant={@computed_variant} />
      <% end %>
      <UIBadge.badge variant={@computed_variant} size={@size} class={@badge_class}>
        <%= if @show_icon && !@show_pulse do %>
          <UIIcon.icon name={@status_icon} size="xs" />
        <% end %>
        <%= @label || String.capitalize(@status) %>
      </UIBadge.badge>
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build badge classes
  @spec build_badge_class(String.t()) :: String.t()
  defp build_badge_class(size) do
    ""
  end

  @spec render_pulse(variant()) :: Phoenix.LiveView.Rendered.t()
  defp render_pulse(variant) do
    assigns = %{variant: variant}
    ~H"""
    <div class={["status-indicator-pulse relative flex h-2 w-2", pulse_color(@variant)]}>
      <span class="animate-ping absolute inline-flex h-full w-full rounded-full opacity-75"></span>
      <span class="status-indicator-pulse-dot relative inline-flex rounded-full h-2 w-2"></span>
    </div>
    """
  end

  @spec compute_variant(String.t(), variant()) :: variant()
  defp compute_variant(status, "auto") do
    case String.downcase(status) do
      s when s in ~w(active online success completed approved) -> "success"
      s when s in ~w(pending warning processing) -> "warning"
      s when s in ~w(error failed rejected offline) -> "destructive"
      s when s in ~w(info draft) -> "secondary"
      _ -> "default"
    end
  end

  defp compute_variant(_status, variant), do: variant

  @spec status_icon(String.t()) :: atom()
  defp status_icon(status) do
    case String.downcase(status) do
      s when s in ~w(active online success completed approved) -> :check
      s when s in ~w(pending warning processing) -> :clock
      s when s in ~w(error failed rejected offline) -> :x_mark
      s when s in ~w(info draft) -> :information_circle
      _ -> :circle
    end
  end

  @spec pulse_color(variant()) :: String.t()
  defp pulse_color("success"), do: "text-green-400"
  defp pulse_color("warning"), do: "text-yellow-400"
  defp pulse_color("destructive"), do: "text-red-400"
  defp pulse_color("secondary"), do: "text-blue-400"
  defp pulse_color(_), do: "text-gray-400"
end
