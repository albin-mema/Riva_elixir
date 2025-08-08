alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Atoms.Avatar do
  @moduledoc """
  Avatar component for users and businesses with fallbacks.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon

  @type assigns :: map()
  @type size :: String.t()
  @type shape :: String.t()
  @type status :: String.t() | nil

  @doc """
  Renders an avatar with image or initials fallback.

  ## Examples

      <.avatar src="/path/to/image.jpg" alt="User" />
      <.avatar initials="JD" name="John Doe" size="lg" />
      <.avatar status="online" />
  """
  @spec avatar(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:src, :string, default: nil)
  attr(:alt, :string, default: "")
  attr(:initials, :string, default: nil)
  attr(:name, :string, default: nil)
  attr(:size, :string, default: "md", values: ~w(xs sm md lg xl 2xl))
  attr(:shape, :string, default: "circle", values: ~w(circle square rounded))
  attr(:status, :string, default: nil)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def avatar(assigns) do
    with {:ok, validated_assigns} <- validate_assigns(assigns),
         avatar_class <- build_avatar_class(validated_assigns) do
      assigns = validated_assigns |> assign(:avatar_class, avatar_class)
      render_avatar(assigns)
    else
      {:error, reason} -> render_error(%{reason: reason})
    end
  end

  # Functional Core: Pure validation functions
  @spec validate_assigns(assigns :: assigns()) :: {:ok, assigns()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with :ok <- validate_src_or_initials(assigns),
         :ok <- validate_status(assigns) do
      {:ok, assigns}
    end
  end

  @spec validate_src_or_initials(assigns :: assigns()) :: :ok | {:error, String.t()}
  defp validate_src_or_initials(assigns) do
    if is_nil(assigns.src) and is_nil(assigns.initials) and is_nil(assigns.name) do
      {:error, "Avatar must have either src, initials, or name"}
    else
      :ok
    end
  end

  @spec validate_status(assigns :: assigns()) :: :ok | {:error, String.t()}
  defp validate_status(assigns) do
    if assigns.status and assigns.status not in ~w(online away busy offline) do
      {:error, "Invalid status. Must be one of: online, away, busy, offline"}
    else
      :ok
    end
  end

  # Functional Core: Pure class building functions
  @spec build_avatar_class(assigns :: assigns()) :: String.t()
  defp build_avatar_class(assigns) do
    base = "relative inline-flex items-center justify-center overflow-hidden bg-muted"
    size = size_classes(assigns.size)
    shape = shape_classes(assigns.shape)

    [base, size, shape, assigns.class]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec size_classes(size :: String.t()) :: String.t()
  defp size_classes(size) do
    case size do
      "xs" -> "h-6 w-6 text-xs"
      "sm" -> "h-8 w-8 text-sm"
      "md" -> "h-10 w-10 text-base"
      "lg" -> "h-12 w-12 text-lg"
      "xl" -> "h-16 w-16 text-xl"
      "2xl" -> "h-20 w-20 text-2xl"
      _ -> "h-10 w-10 text-base"
    end
  end

  @spec shape_classes(shape :: String.t()) :: String.t()
  defp shape_classes(shape) do
    case shape do
      "circle" -> "rounded-full"
      "square" -> "rounded-none"
      "rounded" -> "rounded-md"
      _ -> "rounded-full"
    end
  end

  @spec initials_class(assigns :: assigns()) :: String.t()
  defp initials_class(_assigns) do
    "font-medium text-foreground select-none"
  end

  @spec status_indicator_class(status :: String.t(), size :: String.t()) :: String.t()
  defp status_indicator_class(status, size) do
    base = "absolute border-2 border-background rounded-full"
    position = status_position(size)
    color = status_color(status)

    [base, position, color]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  @spec status_position(size :: String.t()) :: String.t()
  defp status_position(size) do
    case size do
      "xs" -> "bottom-0 right-0 h-2 w-2"
      "sm" -> "bottom-0 right-0 h-2.5 w-2.5"
      "md" -> "bottom-0 right-0 h-3 w-3"
      "lg" -> "bottom-0 right-0 h-3.5 w-3.5"
      "xl" -> "bottom-0 right-0 h-4 w-4"
      "2xl" -> "bottom-0 right-0 h-5 w-5"
      _ -> "bottom-0 right-0 h-3 w-3"
    end
  end

  @spec status_color(status :: String.t()) :: String.t()
  defp status_color(status) do
    case status do
      "online" -> "bg-green-500"
      "away" -> "bg-yellow-500"
      "busy" -> "bg-red-500"
      "offline" -> "bg-gray-400"
      _ -> "bg-gray-400"
    end
  end

  @spec icon_size(size :: String.t()) :: String.t()
  defp icon_size(size) do
    case size do
      "xs" -> "xs"
      "sm" -> "sm"
      "md" -> "md"
      "lg" -> "lg"
      "xl" -> "xl"
      "2xl" -> "xl"
      _ -> "md"
    end
  end

  # Imperative Shell: Rendering functions
  defp render_avatar(assigns) do
    ~H"""
    <div class={@avatar_class} {@rest}>
      <%= render_avatar_content(assigns) %>
      <%= render_status_indicator(assigns) %>
    </div>
    """
  end

  defp render_avatar_content(assigns) do
    cond do
      assigns.src ->
        ~H"""
        <img src={@src} alt={@alt} class="w-full h-full object-cover" />
        """

      assigns.initials ->
        ~H"""
        <span class={initials_class(assigns)}><%= @initials %></span>
        """

      true ->
        ~H"""
        <.icon name={:user} size={icon_size(@size)} class="text-muted-foreground" />
        """
    end
  end

  defp render_status_indicator(assigns) do
    if assigns.status do
      ~H"""
      <div class={status_indicator_class(@status, @size)}></div>
      """
    end
  end

  defp render_error(assigns) do
    # In a real implementation, you might want to log this error
    # and render a fallback avatar or error state
    reason = Map.get(assigns, :reason, "Invalid avatar parameters")
    IO.puts("Avatar error: #{reason}")

    assigns = %{reason: reason}

    ~H"""
    <div class="relative bg-red-100 px-4 py-3 border border-red-400 rounded text-red-700">
      <span class="block sm:inline">Error: <%= @reason %></span>
    </div>
    """
  end
end
