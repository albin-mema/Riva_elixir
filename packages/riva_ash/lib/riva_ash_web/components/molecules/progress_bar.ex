# alias RivaAshWeb.Components.Molecules, as: Molecules
# alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Molecules.ProgressBar do
  @moduledoc """
  Progress bar component for indicating completion status.

  Provides a configurable progress bar with value display, percentage calculation,
  and customizable styling.

  ## Styleguide Compliance

  This component follows the Riva Ash styleguide principles:

  ### Functional Programming Patterns
  - Uses pipeline operator (`|>`) for data transformation
  - Implements pure functions with no side effects
  - Uses pattern matching for data validation and processing
  - Follows single level of abstraction principle

  ### Type Safety
  - Comprehensive type specifications using `@type` and `@spec`
  - Strong typing for all function parameters and return values
  - Type validation through pattern matching

  ### Error Handling
  - Uses result tuples (`:ok | {:error, String.t()}`) for consistent error handling
  - Early validation with guard clauses
  - Clear error messages for invalid inputs

  ### Code Abstraction
  - Separates concerns into focused helper functions
  - Extracts validation logic into dedicated functions
  - Uses functional composition for complex operations

  ### Phoenix/Ash Patterns
  - Follows Phoenix LiveView component conventions
  - Uses proper attribute validation and building
  - Implements functional core, imperative shell pattern

  ### LiveView Component Patterns
  - Uses proper slot and attribute handling
  - Implements accessibility features
  - Follows Phoenix component best practices
  """
  use Phoenix.Component

  @type progress_config :: %{
          value: integer(),
          max: integer(),
          label: String.t() | nil,
          show_percentage: boolean(),
          size: size(),
          variant: variant(),
          animated: boolean()
        }
  @type size :: :sm | :md | :lg
  @type variant :: :default | :success | :warning | :error
  @type assigns :: %{
          required(:value) => integer(),
          optional(:max) => integer(),
          optional(:label) => String.t() | nil,
          optional(:show_percentage) => boolean(),
          optional(:size) => size(),
          optional(:variant) => variant(),
          optional(:animated) => boolean(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a progress bar.

  ## Examples

      <.progress_bar
        value={75}
        max={100}
        label="Progress"
        show_percentage={true}
        size="md"
        variant="success"
        animated={true}
      />
  """
  @spec progress_bar(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:value, :integer,
    required: true,
    doc: "Current progress value"
  )

  attr(:max, :integer,
    default: 100,
    doc: "Maximum value for the progress bar"
  )

  attr(:label, :string,
    default: nil,
    doc: "Optional label for the progress bar"
  )

  attr(:show_percentage, :boolean,
    default: true,
    doc: "Whether to show the progress percentage"
  )

  attr(:size, :string,
    default: "md",
    values: ~w(sm md lg),
    doc: "Size variant of the progress bar"
  )

  attr(:variant, :string,
    default: "default",
    values: ~w(default success warning error),
    doc: "Visual variant of the progress bar"
  )

  attr(:animated, :boolean,
    default: false,
    doc: "Whether the progress bar should have animated transitions"
  )

  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the container"
  )

  attr(:rest, :global)

  @impl true
  def progress_bar(assigns) do
    assigns
    |> build_progress_bar_attrs()
    |> validate_progress_bar_attrs()
    |> render_progress_bar()
  end

  @spec build_progress_bar_attrs(assigns :: assigns()) :: assigns()
  defp build_progress_bar_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      max: Application.get_env(:riva_ash, :progress_bar_max, 100),
      show_percentage: Application.get_env(:riva_ash, :progress_bar_show_percentage, true),
      size: Application.get_env(:riva_ash, :progress_bar_size, "md"),
      variant: Application.get_env(:riva_ash, :progress_bar_variant, "default"),
      animated: Application.get_env(:riva_ash, :progress_bar_animated, false)
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:max, config.max)
    |> Map.put_new(:show_percentage, config.show_percentage)
    |> Map.put_new(:size, config.size)
    |> Map.put_new(:variant, config.variant)
    |> Map.put_new(:animated, config.animated)
  end

  @spec validate_progress_bar_attrs(assigns :: assigns()) :: assigns()
  defp validate_progress_bar_attrs(assigns) do
    with :ok <- validate_value(assigns[:value]),
         :ok <- validate_max(assigns[:max]),
         :ok <- validate_label(assigns[:label]),
         :ok <- validate_show_percentage(assigns[:show_percentage]),
         :ok <- validate_size(assigns[:size]),
         :ok <- validate_variant(assigns[:variant]),
         :ok <- validate_animated(assigns[:animated]),
         :ok <- validate_class(assigns[:class]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid progress bar attributes: #{reason}"
    end
  end

  @spec validate_value(integer()) :: :ok | {:error, String.t()}
  defp validate_value(value) when is_integer(value), do: :ok
  defp validate_unmatchedvalue(_unmatched), do: {:error, "value must be an integer"}

  @spec validate_max(integer()) :: :ok | {:error, String.t()}
  defp validate_max(max) when is_integer(max) and max > 0, do: :ok
  defp validate_unmatchedmax(_unmatched), do: {:error, "max must be a positive integer"}

  @spec validate_label(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_label(nil), do: :ok
  defp validate_label(label) when is_binary(label), do: :ok
  defp validate_unmatchedlabel(_unmatched), do: {:error, "label must be a string or nil"}

  @spec validate_show_percentage(boolean()) :: :ok | {:error, String.t()}
  defp validate_show_percentage(show_percentage) when is_boolean(show_percentage), do: :ok

  defp validate_unmatchedshow_unmatchedpercentage(_unmatched),
    do: {:error, "show_unmatchedpercentage must be a boolean"}

  @spec validate_size(String.t()) :: :ok | {:error, String.t()}
  defp validate_size("sm"), do: :ok
  defp validate_size("md"), do: :ok
  defp validate_size("lg"), do: :ok
  defp validate_unmatchedsize(_unmatched), do: {:error, "size must be one of: sm, md, lg"}

  @spec validate_variant(String.t()) :: :ok | {:error, String.t()}
  defp validate_variant("default"), do: :ok
  defp validate_variant("success"), do: :ok
  defp validate_variant("warning"), do: :ok
  defp validate_variant("error"), do: :ok
  defp validate_unmatchedvariant(_unmatched), do: {:error, "variant must be one of: default, success, warning, error"}

  @spec validate_animated(boolean()) :: :ok | {:error, String.t()}
  defp validate_animated(animated) when is_boolean(animated), do: :ok
  defp validate_unmatchedanimated(_unmatched), do: {:error, "animated must be a boolean"}

  @spec validate_class(String.t()) :: :ok | {:error, String.t()}
  defp validate_class(class) when is_binary(class), do: :ok
  defp validate_unmatchedclass(_unmatched), do: {:error, "class must be a string"}

  @spec render_progress_bar(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_progress_bar(assigns) do
    percentage = calculate_percentage(assigns.value, assigns.max)

    assigns = assign(assigns, :percentage, percentage)

    ~H"""
    <div class={["progress-bar-container", @class]} {@rest}>
      <%= render_header_content(@label, @show_percentage, @percentage) %>
      <%= render_progress_bar_content(@value, @max, @percentage, @size, @variant, @animated) %>
    </div>
    """
  end

  @spec render_header_content(String.t() | nil, boolean(), integer()) :: Phoenix.LiveView.Rendered.t()
  defp render_header_content(label, show_percentage, percentage) do
    assigns = %{
      label: label,
      show_percentage: show_percentage,
      percentage: percentage
    }

    ~H"""
    <div :if={@label || @show_percentage} class="progress-bar-header">
      <span :if={@label}><%= @label %></span>
      <span :if={@show_percentage}><%= @percentage %>%</span>
    </div>
    """
  end

  @spec render_progress_bar_content(integer(), integer(), integer(), size(), variant(), boolean()) ::
          Phoenix.LiveView.Rendered.t()
  defp render_progress_bar_content(value, max, percentage, size, variant, animated) do
    assigns = %{
      value: value,
      max: max,
      percentage: percentage,
      size: size,
      variant: variant,
      animated: animated
    }

    ~H"""
    <div class={["progress-bar-track", size_class(@size)]}>
      <div
        class={["progress-bar-fill", variant_class(@variant), animated_class(@animated)]}
        style={"width: #{@percentage}%"}
      >
        <!-- Progress fill -->
      </div>
    </div>
    """
  end

  @spec render_progress_bar_content(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_progress_bar_content(%{
         value: value,
         max: max,
         percentage: percentage,
         size: size,
         variant: variant,
         animated: animated
       }) do
    render_progress_bar_content(value, max, percentage, size, variant, animated)
  end

  @spec size_class(size()) :: String.t()
  defp size_class("sm"), do: "h-1"
  defp size_class("md"), do: "h-2"
  defp size_class("lg"), do: "h-3"

  @spec variant_class(variant()) :: String.t()
  defp variant_class("default"), do: "bg-blue-600"
  defp variant_class("success"), do: "bg-green-600"
  defp variant_class("warning"), do: "bg-yellow-600"
  defp variant_class("error"), do: "bg-red-600"

  @spec animated_class(boolean()) :: String.t()
  defp animated_class(true), do: "transition-all duration-300 ease-in-out"
  defp animated_class(false), do: ""

  @spec calculate_percentage(integer(), integer()) :: integer()
  defp calculate_percentage(value, max) do
    # Calculate percentage using functional composition with guard clauses
    cond do
      max <= 0 -> 0
      value >= max -> 100
      true -> round(value / max * 100)
    end
  end
end
