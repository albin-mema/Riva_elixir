defmodule RivaAsh.Components.UI.CommandPalette do
  @moduledoc """
  Command palette component with global shortcut and ARIA-compliant search functionality.
  Composed from atoms only, following atomic design principles.
  """

  @variants [:default, :search, :recent]
  @sizes [:sm, :md, :lg]
  @default_props %{
    variant: :default,
    size: :md,
    disabled: false,
    aria_label: "Command palette"
  }

  def standard_assigns(assigns, defaults \\ @default_props) do
    assigns
    |> assign_new(:variant, fn -> validate_variant(defaults[:variant]) end)
    |> assign_new(:size, fn -> validate_size(defaults[:size]) end)
    |> assign_new(:disabled, fn -> !!defaults[:disabled] end)
    |> assign_new(:aria_label, fn -> defaults[:aria_label] end)
    |> assign_new(:commands, fn -> [] end)
    |> assign_new(:recent_commands, fn -> [] end)
    |> assign_new(:is_open, fn -> false end)
  end

  @doc """
  Generates variant-specific classes for command palette
  """
  def variant_classes(:default) do
    "bg-popover text-popover-foreground border border-border"
  end

  def variant_classes(:search) do
    "bg-input text-foreground border border-input"
  end

  def variant_classes(:recent) do
    "bg-card text-muted-foreground border border-border"
  end

  @doc """
  Generates size-specific classes for command palette
  """
  def size_classes(:sm) do
  @doc """
  Applies design tokens from tokens.json for consistent styling
  """
  defp apply_design_tokens(socket) do
    tokens = read_tokens()
    socket
    |> assign(
      variant_classes: "bg-#{tokens.surface.card} text-#{tokens.semantic.primary}",
      size_classes: "p-#{tokens.spacing.space-3} m-#{tokens.spacing.space-4}",
      radii: %{radius-md: tokens.radii.radius_md},
      shadows: %{shadow-md: tokens.shadows.shadow_lg},
      motion: %{duration: tokens.motion.duration.normal}
    )
  end

  defp read_tokens do
    # Implementation to read tokens from JSON file
    # This would typically use a file reading function
    %{
      colors: %{
        core: %{blue: "oklch(0.52 0.18 275)"},
        semantic: %{primary: "var(--primary)"},
        surface: %{card: "var(--card)"}
      },
      spacing: %{
        space-3: "0.75rem",
        space-4: "1rem"
      },
      radii: %{radius_md: "var(--radius-md)"},
      shadows: %{shadow_lg: "var(--shadow-lg)"},
      motion: %{duration: %{normal: "300ms"}}
    }
  end
  @doc """
  Renders featured commands section with priority display
  """
  def render_featured_commands(assigns) do
    ~H"""
    <div :if={@featured_commands} class="p-2 border-b border-border">
      <h3 class="px-2 py-1 text-muted-foreground text-xs">Featured</h3>
      <ul>
        <%= for command <- @featured_commands do %>
          <li
            class="hover:bg-accent px-2 py-1 rounded-md hover:text-accent-foreground cursor-pointer"
            role="option"
            aria-selected={command.id == @active_command_id}
            :onclick={@command_handler}
            data-command={command.id}
          >
            <%= command.label %>
          </li>
        <% end %>
      </ul>
    </div>
    """
  end
  @doc """
  Handles debounced asynchronous search with loading and empty states
  """
  def handle_search(socket) do
    socket
    |> on_event(:search_query, fn query, socket ->
      if String.length(query) >= 2 do
        socket
        |> assign(loading: true)
        |> debounce(300, fn ->
          results = search_commands(query)
          assign(socket, commands: results, loading: false)
        end)
      else
        assign(socket, commands: [], loading: false)
      end
    end)
  end

  @doc """
  Manages recent commands history with ARIA state updates
  """
  def update_recent_commands(socket, command) do
    recent = get_recent_commands(socket) ++ [command]
    assign(socket, recent_commands: recent)
    |> announce_for_screen_reader("Command added to recent history: #{command.label}")
  end

  defp get_recent_commands(socket) do
    socket.assigns.recent_commands || []
  end

  defp search_commands(query) do
    # Placeholder for actual search implementation
    []
  end
  @doc """
  Renders the command palette UI with ARIA roles and states
  """
  def render(assigns) do
    ~H"""
    <div :if={@is_open} class={"#{@variant_classes} #{@size_classes} rounded-#{@radii.radius-md} shadow-#{@shadows.shadow-lg} transition-opacity duration-#{@motion.duration.normal} ease-in-out"}>
      <div class="flex items-center p-2 border-b border-border">
        <input
          type="text"
          class="w-full #{focus_styles()} #{focus_styles()}"
          placeholder="Type a command or search..."
          aria-label={@aria_label}
          aria-expanded={@is_open}
          aria-owns={@aria_owns}
          aria-activedescendant={@aria_activedescendant}
          :onkeydown={@keydown_handler}
          :value={@search_query}
        />
        <kbd class="text-muted-foreground text-xs">Cmd+K</kbd>
      </div>

      <div :if={@recent_commands} class="p-2">
        <h3 class="px-2 py-1 text-muted-foreground text-xs">Recent Commands</h3>
        <ul class="max-h-32 overflow-auto">
          <%= for command <- @recent_commands do %>
            <li
              class="hover:bg-accent px-2 py-1 rounded-md hover:text-accent-foreground cursor-pointer"
              :onclick={@command_handler}
              data-command={command.id}
            >
              <%= command.label %>
            </li>
          <% end %>
        </ul>
      </div>

      <div class="p-2">
        <%= if @loading do %>
          <div class="space-y-2 animate-pulse">
            <div class="bg-muted-foreground/10 rounded w-3/4 h-4"></div>
            <div class="bg-muted-foreground/10 rounded w-1/2 h-4"></div>
          </div>
        <% else %>
          <%= if Enum.empty?(@commands) do %>
            <div class="py-4 text-muted-foreground text-center">
              No commands found
            </div>
          <% else %>
            <ul class="max-h-64 overflow-auto" role="listbox" id={@aria_owns}>
              <%= for {command, index} <- Enum.with_index(@filtered_commands) do %>
                <li
                  class={"hover:bg-accent px-2 py-1 rounded-md hover:text-accent-foreground cursor-pointer #{if @active_index == index, do: "bg-accent text-accent-foreground"}"}
                  role="option"
                  aria-selected={@active_index == index}
                  :onclick={@command_handler}
                  data-command={command.id}
                  aria-current={@active_index == index}
                >
                  <%= command.label %>
                </li>
              <% end %>
            </ul>
          </div>
        </div>

        <div :if={@recent_commands and Enum.empty?(@filtered_commands) and String.trim(@search_query) == ""} class="p-2 border-t border-border">
          <h3 class="px-2 py-1 text-muted-foreground text-xs">Recent Commands</h3>
          <ul>
            <%= for {command, index} <- Enum.with_index(@recent_commands) do %>
              <li
                class={"hover:bg-accent px-2 py-1 rounded-md hover:text-accent-foreground cursor-pointer #{if @active_index == index, do: "bg-accent text-accent-foreground"}"}
                role="option"
                aria-selected={@active_index == index}
                :onclick={@command_handler}
                data-command={command.id}
                aria-current={@active_index == index}
              >
                <%= command.label %>
              </li>
            <% end %>
          </ul>
        </div>
                <li
                  class="hover:bg-accent px-2 py-1 rounded-md hover:text-accent-foreground cursor-pointer"
                  role="option"
                  aria-selected={command.id == @active_command_id}
                  :onclick={@command_handler}
                  data-command={command.id}
                >
                  <%= command.label %>
                </li>
              <% end %>
            </ul>
          <% end %>
        <% end %>
      </div>
    </div>
    """
  end
  @doc """
  Handles global keyboard shortcut (Cmd/Ctrl+K) to open command palette
  """
  def handle_global_shortcut(socket) do
    if connected?(socket) do
      socket
      |> on_keydown("k", :meta, fn _event, socket ->
        if should_open?() do
          socket
          |> assign(is_open: true)
          |> announce_for_screen_reader("Command palette opened. Type to search commands.")
        end
      end)
      |> on_keydown("Escape", fn _event, socket ->
        assign(socket, is_open: false)
      end)
    else
      socket
    end
  end

  @doc """
  Announces messages to screen readers using aria-live regions
  """
  def announce_for_screen_reader(socket, message) do
    send_update(socket, RivaAsh.Components.UI.LiveRegion, message: message)
  end
    "w-64 max-h-64 #{input_size_classes(:sm)}"
  end

  def size_classes(:md) do
    "w-80 max-h-96 #{input_size_classes(:md)}"
  end

  def size_classes(:lg) do
    "w-96 max-h-112 #{input_size_classes(:lg)}"
  end

  defp input_size_classes(:sm) do
    "p-2 text-xs"
  end

  defp input_size_classes(:md) do
    "p-3 text-sm"
  end

  defp input_size_classes(:lg) do
    "p-4 text-base"
  end

  @doc """
  Generates ARIA attributes for accessibility compliance
  """
  def aria_attributes(assigns) do
    %{
      "aria-expanded" => assigns.is_open,
      "aria-haspopup" => "listbox",
      "aria-owns" => assigns.id && "command-palette-#{assigns.id}",
      "aria-activedescendant" => assigns.active_command_id
    }
    |> Enum.filter(fn {_, v} -> v end)
    |> Enum.into(%{})
  end

  defp validate_variant(variant) when variant in @variants, do: variant
  defp validate_variant(_), do: :default

  defp validate_size(size) when size in @sizes, do: size
  defp validate_size(_), do: :md
end
