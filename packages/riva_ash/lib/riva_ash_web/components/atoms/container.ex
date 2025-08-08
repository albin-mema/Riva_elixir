alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Atoms.Container do
  @moduledoc """
  Container component for constraining content width and centering.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type size :: :sm | :md | :lg | :xl | :full
  @type fluid :: boolean()
  @type center :: boolean()

  @doc """
  Renders a container component with different size options.

  ## Examples

      <.container size="md">
        <p>This content is constrained to medium width</p>
      </.container>

      <.container fluid={true}>
        <p>This content spans the full width</p>
      </.container>

      <.container size="xl" center={true}>
        <p>Large centered container</p>
      </.container>
  """
  @spec container(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :size, :string, default: "md", values: ~w(sm md lg xl full)
  attr :fluid, :boolean, default: false
  attr :center, :boolean, default: true
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def container(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         container_class <- build_container_class(validated_assigns) do
      assigns = validated_assigns |> assign(:container_class, container_class)
      render_container(assigns)
    else
      {:error, reason} -> render_error(%{reason: reason})
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_size(assigns.size),
         :ok <- validate_fluid(assigns.fluid),
         :ok <- validate_center(assigns.center) do
      {:ok, assigns}
    end
  end

  @spec validate_size(size :: String.t()) :: :ok | {:error, String.t()}
  defp validate_size(size) do
    if size in ~w(sm md lg xl full) do
      :ok
    else
      {:error, "Invalid size. Must be one of: sm, md, lg, xl, full"}
    end
  end

  @spec validate_fluid(fluid :: boolean()) :: :ok | {:error, String.t()}
  defp validate_fluid(fluid) do
    if is_boolean(fluid) do
      :ok
    else
      {:error, "Fluid must be a boolean value"}
    end
  end

  @spec validate_center(center :: boolean()) :: :ok | {:error, String.t()}
  defp validate_center(center) do
    if is_boolean(center) do
      :ok
    else
      {:error, "Center must be a boolean value"}
    end
  end

  # Functional Core: Pure class building functions
  @spec build_container_class(assigns :: assigns()) :: String.t()
  defp build_container_class(assigns) do
    base = base_classes(assigns.fluid, assigns.center)
    size = size_classes(assigns.size)

    [base, size, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec base_classes(fluid :: boolean(), center :: boolean()) :: String.t()
  defp base_classes(fluid, center) do
    cond do
      fluid -> "w-full"
      center -> "mx-auto"
      true -> ""
    end
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "sm" -> "max-w-screen-sm px-4"
      "md" -> "max-w-screen-md px-4"
      "lg" -> "max-w-screen-lg px-4"
      "xl" -> "max-w-screen-xl px-4"
      "full" -> "max-w-full px-4"
      _ -> "max-w-screen-md px-4"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_container(assigns) do
    ~H"""
    <div class={@container_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp render_error(assigns) do
    # In a real implementation, you might want to log this error
    # and render a fallback container or error state
    reason = Map.get(assigns, :reason, "Invalid container parameters")
    IO.puts("Container error: #{reason}")

    assigns = %{reason: reason}

    ~H"""
    <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">
      <span class="block sm:inline">Error: <%= @reason %></span>
    </div>
    """
  end
end
