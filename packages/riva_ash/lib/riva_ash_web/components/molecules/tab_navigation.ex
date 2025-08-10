# alias RivaAshWeb.Components.Molecules, as: Molecules
# alias RivaAshWeb.Components.Atoms, as: Atoms
# alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Molecules.TabNavigation do
  @moduledoc """
  Tab navigation component for switching between views with accessibility and keyboard navigation.
  """
  use Phoenix.Component
#  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Icon

  @type assigns :: %{
          required(:tabs) => list(map()),
          required(:active_tab) => String.t(),
          required(:on_tab_change) => String.t(),
          optional(:variant) => String.t(),
          optional(:size) => String.t(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders tab navigation with accessibility features and keyboard navigation.
  """
  @spec tab_navigation(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:tabs, :list,
    required: true,
    doc: "List of tabs with required :id and :label keys"
  )

  attr(:active_tab, :string,
    required: true,
    doc: "ID of the currently active tab"
  )

  attr(:on_tab_change, :string,
    required: true,
    doc: "Event handler for tab changes"
  )

  attr(:variant, :string,
    default: "default",
    values: ~w(default pills underline),
    doc: "Visual variant of the tab navigation"
  )

  attr(:size, :string, default: "md", values: ~w(sm md lg), doc: "Size variant of the tab navigation")

  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the container"
  )

  attr(:rest, :global)

  @impl true
  def tab_navigation(assigns) do
    assigns
    |> build_tab_navigation_attrs()
    |> validate_tab_navigation_attrs()
    |> render_tab_navigation()
  end

  @spec build_tab_navigation_attrs(assigns :: assigns()) :: assigns()
  defp build_tab_navigation_attrs(assigns) do
    default_variant = Application.get_env(:riva_ash, :tab_navigation_variant, "default")
    default_size = Application.get_env(:riva_ash, :tab_navigation_size, "md")

    assigns
    |> Map.put_new(:variant, default_variant)
    |> Map.put_new(:size, default_size)
  end

  @spec validate_tab_navigation_attrs(assigns :: assigns()) :: assigns()
  defp validate_tab_navigation_attrs(assigns) do
    with :ok <- validate_tabs(assigns[:tabs]),
         :ok <- validate_active_tab(assigns[:active_tab], assigns[:tabs]),
         :ok <- validate_variant(assigns[:variant]),
         :ok <- validate_size(assigns[:size]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid tab navigation attributes: #{reason}"
    end
  end

  @spec validate_tabs(list(map())) :: :ok | {:error, String.t()}
  defp validate_tabs(tabs) when is_list(tabs) and length(tabs) > 0 do
    case Enum.all?(tabs, &valid_tab?/1) do
      true -> :ok
      false -> {:error, "All tabs must have :id and :label keys"}
    end
  end

  defp validate_unmatchedtabs(_unmatched), do: {:error, "Tabs must be a non-empty list"}

  @spec valid_tab?(map()) :: boolean()
  defp valid_tab?(tab) do
    is_map(tab) and is_binary(tab[:id]) and is_binary(tab[:label])
  end

  @spec validate_active_tab(String.t(), list(map())) :: :ok | {:error, String.t()}
  defp validate_active_tab(active_tab, tabs) do
    if Enum.any?(tabs, &(&1[:id] == active_tab)) do
      :ok
    else
      {:error, "active_tab must match one of the tab IDs"}
    end
  end

  @spec validate_variant(String.t()) :: :ok | {:error, String.t()}
  defp validate_variant("default"), do: :ok
  defp validate_variant("pills"), do: :ok
  defp validate_variant("underline"), do: :ok
  defp validate_unmatchedvariant(_unmatched), do: {:error, "Variant must be one of: default, pills, underline"}

  @spec validate_size(String.t()) :: :ok | {:error, String.t()}
  defp validate_size("sm"), do: :ok
  defp validate_size("md"), do: :ok
  defp validate_size("lg"), do: :ok
  defp validate_unmatchedsize(_unmatched), do: {:error, "Size must be one of: sm, md, lg"}

  @spec render_tab_navigation(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_tab_navigation(assigns) do
    # Render tab navigation using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:nav_class, build_nav_class(assigns.variant, assigns.size))
    |> render_tab_navigation_component()
  end

  # Private helper for tab navigation rendering
  @spec render_tab_navigation_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_tab_navigation_component(assigns) do
    ~H"""
    <div
      class={@container_class}
      role="tablist"
      aria-label="Navigation tabs"
      {@rest}
    >
      <nav class={@nav_class}>
        <%= for tab <- @tabs do %>
          <%= render_tab(tab, @active_tab, @on_tab_change, @variant, @size) %>
        <% end %>
      </nav>
    </div>
    """
  end

  @spec render_tab(map(), String.t(), String.t(), String.t(), String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_tab(tab, active_tab, on_tab_change, variant, size) do
    assigns = %{
      tab: tab,
      active_tab: active_tab,
      on_tab_change: on_tab_change,
      variant: variant,
      size: size,
      is_active: tab[:id] == active_tab,
      tab_id: "tab-#{tab[:id]}",
      panel_id: "panel-#{tab[:id]}"
    }

    ~H"""
    <button
      id={@tab_id}
      role="tab"
      aria-selected={@is_active}
      aria-controls={@panel_id}
      aria-disabled={@tab[:disabled]}
      class={build_tab_class(@is_active, @variant, @size, @tab[:disabled])}
      phx-click={if @tab[:disabled] == false, do: @on_tab_change}
      phx-value-tab={@tab[:id]}
      disabled={@tab[:disabled]}
      data-tab-id={@tab[:id]}
    >
      <span class="tab-content">
        <%= if @tab[:icon] do %>
          <.icon name={@tab[:icon]} class="tab-icon" />
        <% end %>
        <span class="tab-label"><%= @tab[:label] %></span>
        <%= if @tab[:count] do %>
          <span class="tab-count"><%= @tab[:count] %></span>
        <% end %>
      </span>
    </button>
    """
  end

  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    "tab-navigation-container #{class}"
  end

  @spec build_nav_class(String.t(), String.t()) :: String.t()
  defp build_nav_class("pills", size) do
    "tab-pills flex space-x-1 #{build_size_base_class(size)}"
  end

  defp build_nav_class("underline", size) do
    "tab-underline flex space-x-6 border-b border-gray-200 #{build_size_base_class(size)}"
  end

  defp build_nav_class(_unmatched, size) do
    "tab-default flex space-x-1 #{build_size_base_class(size)}"
  end

  @spec build_size_base_class(String.t()) :: String.t()
  defp build_size_base_class("sm") do
    "text-sm"
  end

  defp build_size_base_class("lg") do
    "text-lg"
  end

  defp build_unmatchedsize_unmatchedbase_unmatchedclass(_unmatched) do
    "text-base"
  end

  @spec build_tab_class(boolean(), String.t(), String.t(), boolean()) :: String.t()
  defp build_tab_class(true, "pills", size, _disabled) do
    "tab-button tab-active tab-pills-active #{build_size_pill_class(size)}"
  end

  defp build_tab_class(false, "pills", size, false) do
    "tab-button tab-pills-inactive #{build_size_pill_class(size)}"
  end

  defp build_tab_class(true, "underline", size, _disabled) do
    "tab-button tab-active tab-underline-active #{build_size_text_class(size)}"
  end

  defp build_tab_class(false, "underline", size, false) do
    "tab-button tab-underline-inactive #{build_size_text_class(size)}"
  end

  defp build_tab_class(true, _unmatched, size, _disabled) do
    "tab-button tab-active #{build_size_default_class(size)}"
  end

  defp build_tab_class(false, _unmatched, size, false) do
    "tab-button #{build_size_default_class(size)}"
  end

  defp build_tab_class(_unmatched, _unmatched, size, true) do
    "tab-button tab-disabled #{build_size_default_class(size)}"
  end

  @spec build_size_pill_class(String.t()) :: String.t()
  defp build_size_pill_class("sm") do
    "px-3 py-1.5 text-sm"
  end

  defp build_size_pill_class("lg") do
    "px-4 py-2 text-lg"
  end

  defp build_unmatchedsize_unmatchedpill_unmatchedclass(_unmatched) do
    "px-3 py-2 text-base"
  end

  @spec build_size_text_class(String.t()) :: String.t()
  defp build_size_text_class("sm") do
    "py-2 px-1 text-sm font-medium"
  end

  defp build_size_text_class("lg") do
    "py-3 px-2 text-lg font-medium"
  end

  defp build_unmatchedsize_unmatchedtext_unmatchedclass(_unmatched) do
    "py-2.5 px-2 text-base font-medium"
  end

  @spec build_size_default_class(String.t()) :: String.t()
  defp build_size_default_class("sm") do
    "px-3 py-1.5 text-sm"
  end

  defp build_size_default_class("lg") do
    "px-4 py-2 text-lg"
  end

  defp build_unmatchedunmatchedsize_unmatchedunmatcheddefault_unmatchedunmatchedclass(_unmatchedunmatched) do
    "px-3 py-2 text-base"
  end
end
