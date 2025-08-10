defmodule RivaAsh.Components.UI.DropdownMenu do
  @moduledoc """
  Dropdown menu component with ARIA compliance and keyboard navigation.
  Composed from atoms following atomic design principles.
  """

  @variants [:default, :secondary, :destructive]
  @sizes [:sm, :md, :lg]
  @default_props %{
    variant: :default,
    size: :md,
    disabled: false,
    aria_label: "Dropdown menu",
    open: false,
    active_submenu: nil
  }

  def standard_assigns(assigns, defaults \\ @default_props) do
    assigns
    |> assign_new(:variant, fn -> validate_variant(defaults[:variant]) end)
    |> assign_new(:size, fn -> validate_size(defaults[:size]) end)
    |> assign_new(:disabled, fn -> !!defaults[:disabled] end)
    |> assign_new(:aria_label, fn -> defaults[:aria_label] end)
    |> assign_new(:open, fn -> false end)
    |> assign_new(:options, fn -> [] end)
    |> assign_new(:active_index, fn -> -1 end)
    |> assign_new(:search_query, fn -> "" end)
    |> assign_new(:loading, fn -> false end)
    |> assign_new(:active_submenu, fn -> nil end)
  end

  @doc """
  Handles submenu state management
  """
  def handle_submenu(socket, submenu_id) do
    socket
    |> assign(active_submenu: submenu_id)
  end

  @doc """
  Closes submenu
  """
  def close_submenu(socket) do
    socket
    |> assign(active_submenu: nil)
  end

  @doc """
  Handles keyboard navigation with submenu support
  """
  def handle_keyboard_navigation(socket) do
    socket
    |> on_event(:keydown, fn event, socket ->
      case event.key do
        "ArrowDown" ->
          socket
          |> update(:active_index, fn idx ->
            if idx < length(socket.assigns.options) - 1, do: idx + 1, else: 0
          end)
        "ArrowUp" ->
          socket
          |> update(:active_index, fn idx ->
            if idx > 0, do: idx - 1, else: length(socket.assigns.options) - 1
          end)
        "ArrowRight" ->
          current_option = Enum.at(socket.assigns.options, socket.assigns.active_index)
          if current_option && current_option[:submenu] do
            socket
            |> assign(active_submenu: current_option.id)
          else
            socket
          end
        "ArrowLeft" ->
          if socket.assigns.active_submenu do
            socket
            |> assign(active_submenu: nil)
          else
            socket
          end
        "Enter" ->
          if socket.assigns.open do
            selected = Enum.at(socket.assigns.options, socket.assigns.active_index)
            socket
            |> assign(open: false)
            |> announce_for_screen_reader("Selected: #{selected.label}")
          else
            socket
          end
        _ -> socket
      end
    end)
  end

  @doc """
  Renders submenu if active
  """
  def render_submenu(assigns, option) do
    ~H"""
    <%= if @active_submenu == option.id do %>
      <div
        class={"#{@variant_classes} #{@size_classes} #{@radii.radius-md} #{@shadows.shadow-lg} #{@motion.duration.normal} #{@motion.easing.ease-in-out} absolute z-50 mt-2 w-56 origin-top-right rounded-md bg-popover p-1 shadow-lg"}
        role="menu"
        :onkeydown={@keydown_handler}
      >
        <div class="py-1">
          <%= for {suboption, index} <- Enum.with_index(option.submenu) do %>
            <div
              class={"#{@option_classes} #{@focus_styles} #{@hover_styles} #{@active_index == index && "bg-accent text-accent-foreground" || ""}"}
              role="menuitem"
              tabindex={@active_index == index && 0 || -1}
              :onclick={@select_handler}
              data-option={suboption.id}
              aria-selected={@active_index == index}
            >
              <%= suboption.label %>
            </div>
          <% end %>
        </div>
      </div>
    <% end %>
    """
  end

  @doc """
  Renders the dropdown menu UI with ARIA roles and states
  """
  def render(assigns) do
    ~H"""
    <div class="inline-block relative text-left">
      <button
        class={"#{@variant_classes} #{@size_classes} #{@radii.radius-md} #{@shadows.shadow-md} #{@motion.duration.normal} #{@motion.easing.ease-in-out}"}
        aria-label={@aria_label}
        aria-expanded={@open}
        :onkeydown={@keydown_handler}
        :onclick={@toggle_handler}
      >
        <%= @trigger %>
      </button>

      <div
        :if={@open}
        class={"#{@variant_classes} #{@size_classes} #{@radii.radius-md} #{@shadows.shadow-lg} #{@motion.duration.normal} #{@motion.easing.ease-in-out} absolute z-50 mt-2 w-56 origin-top-right rounded-md bg-popover p-1 shadow-lg"}
        role="menu"
        id={@aria_owns}
        :onkeydown={@keydown_handler}
      >
        <div class="py-1">
          <%= if @loading do %>
            <div class="space-y-2 animate-pulse">
              <div class="bg-muted-foreground/10 rounded w-3/4 h-4"></div>
            </div>
          <% else %>
            <%= for {option, index} <- Enum.with_index(@options) do %>
              <div
                class={"#{@option_classes} #{@focus_styles} #{@hover_styles} #{@active_index == index && "bg-accent text-accent-foreground" || ""}"}
                role="menuitem"
                tabindex={@active_index == index && 0 || -1}
                :onclick={@select_handler}
                :onmouseover={JS.push("hover_submenu", value: %{id: option.id})}
                :onmouseout={JS.push("close_submenu")}
                data-option={option.id}
                aria-selected={@active_index == index}
                aria-haspopup={option.submenu && "menu" || nil}
                aria-expanded={@active_submenu == option.id}
              >
                <%= option.label %>
                <%= if option.submenu do %>
                  <span class="ml-2">▶</span>
                <% end %>
              </div>
              <%= render_submenu(assigns, option) %>
            <% end %>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  defp validate_variant(variant) when variant in @variants, do: variant
  defp validate_variant(_), do: :default

  defp validate_size(size) when size in @sizes, do: size
  defp validate_size(_), do: :md

  @doc """
  Applies design tokens from tokens.json for consistent styling
  """
  defp apply_design_tokens(socket) do
    tokens = read_tokens()
    socket
    |> assign(
      variant_classes: "bg-#{tokens.surface.popover} text-#{tokens.semantic.primary}",
      size_classes: "p-#{tokens.spacing.space-3} m-#{tokens.spacing.space-4}",
      radii: %{radius_md: tokens.radii.radius_md},
      shadows: %{shadow_md: tokens.shadows.shadow_lg},
      motion: %{duration: tokens.motion.duration.normal, easing: tokens.motion.easing.ease_in_out}
    )
  end

  defp read_tokens do
    # Implementation to read tokens from JSON file
    %{
      colors: %{
        core: %{blue: "oklch(0.52 0.18 275)"},
        semantic: %{primary: "var(--primary)"},
        surface: %{popover: "var(--popover)", card: "var(--card)"}
      },
      spacing: %{
        space-3: "0.75rem",
        space-4: "1rem"
      },
      radii: %{radius_md: "var(--radius-md)"},
      shadows: %{shadow_lg: "var(--shadow-lg)"},
      motion: %{
        duration: %{normal: "300ms"},
        easing: %{ease_in_out: "cubic-bezier(0.4, 0, 0.2, 1)"}
      }
    }
  end
end
