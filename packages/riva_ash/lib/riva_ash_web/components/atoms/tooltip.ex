defmodule RivaAshWeb.Components.Atoms.Tooltip do
  @moduledoc """
  Tooltip component for hover help text with accessibility support.
  """
  use Phoenix.Component
  import RivaAshWeb.CoreComponents

  @type assigns :: %{
          required(:content) => String.t(),
          optional(:position) => String.t(),
          optional(:trigger) => String.t(),
          optional(:delay) => integer(),
          optional(:class) => String.t(),
          optional(:rest) => map(),
          optional(:inner_block) => any()
        }

  @doc """
  Renders a tooltip with trigger content and accessibility features.
  """
  @spec tooltip(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:content, :string, required: true)
  attr(:position, :string, default: "top", values: ~w(top bottom left right))
  attr(:trigger, :string, default: "hover", values: ~w(hover click focus))
  attr(:delay, :integer, default: 200)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:inner_block, required: true)

  @impl true
  def tooltip(assigns) do
    assigns
    |> build_tooltip_attrs()
    |> render_tooltip()
  end

  @spec build_tooltip_attrs(assigns :: assigns()) :: assigns()
  defp build_tooltip_attrs(assigns) do
    default_delay = Application.get_env(:riva_ash, :tooltip_delay, 200)
    default_position = Application.get_env(:riva_ash, :tooltip_position, "top")

    assigns
    |> Map.put_new(:delay, default_delay)
    |> Map.put_new(:position, default_position)
    |> validate_tooltip_attrs()
  end

  @spec validate_tooltip_attrs(assigns :: assigns()) :: assigns()
  defp validate_tooltip_attrs(assigns) do
    with :ok <- validate_delay(assigns[:delay]),
         :ok <- validate_position(assigns[:position]),
         :ok <- validate_trigger(assigns[:trigger]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid tooltip attributes: #{reason}"
    end
  end

  @spec validate_delay(integer()) :: :ok | {:error, String.t()}
  defp validate_delay(delay) when delay >= 0 and delay <= 5000, do: :ok
  defp validate_delay(_), do: {:error, "Delay must be between 0 and 5000 milliseconds"}

  @spec validate_position(String.t()) :: :ok | {:error, String.t()}
  defp validate_position("top"), do: :ok
  defp validate_position("bottom"), do: :ok
  defp validate_position("left"), do: :ok
  defp validate_position("right"), do: :ok
  defp validate_position(_), do: {:error, "Position must be one of: top, bottom, left, right"}

  @spec validate_trigger(String.t()) :: :ok | {:error, String.t()}
  defp validate_trigger("hover"), do: :ok
  defp validate_trigger("click"), do: :ok
  defp validate_trigger("focus"), do: :ok
  defp validate_trigger(_), do: {:error, "Trigger must be one of: hover, click, focus"}

  @spec render_tooltip(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_tooltip(assigns) do
    ~H"""
    <div class="tooltip-container #{@class}" {@rest}>
      <div
        class="tooltip-trigger"
        data-tooltip-content={@content}
        data-tooltip-position={@position}
        data-tooltip-trigger={@trigger}
        data-tooltip-delay={@delay}
        role="tooltip"
        aria-live="polite"
      >
        <%= render_slot(@inner_block) %>
      </div>

      <div
        class="tooltip-content"
        id={"tooltip-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"}
        role="tooltip"
        aria-hidden="true"
      >
        <%= @content %>
      </div>
    </div>
    """
  end
end
