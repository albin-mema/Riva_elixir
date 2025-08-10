defmodule RivaAsh.Components.UI.Combobox do
  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms
  import RivaAsh.Components.UI.Form

  alias RivaAsh.Components.UI.{CommandPalette, DropdownMenu}

  attr :id, :string, default: "combobox-#{System.unique_integer()}"
  attr :name, :string
  attr :value, :any
  attr :placeholder, :string, default: "Search..."
  attr :options, :list, required: true
  attr :option_label, :any, default: & &1.label
  attr :option_key, :any, default: & &1.id
  attr :multiple, :boolean, default: false
  attr :disabled, :boolean, default: false
  attr :read_only, :boolean, default: false
  attr :loading, :boolean, default: false
  attr :error, :string
  attr :debounce, :integer, default: 300
  attr :min_chars, :integer, default: 1
  attr :allow_custom, :boolean, default: false
  attr :validate_selection, :any, default: & &1
  attr :async_load, :any
  attr :class, :string, default: ""
  attr :rest, :global, include: ~w(autofocus form name required)

  slot :header, doc: "Header content for the dropdown"
  slot :footer, doc: "Footer content for the dropdown"
  slot :empty, doc: "Content to show when no options match"
  slot :option, required: true do
    attr :option, :any
    attr :selected, :boolean
    attr :active, :boolean
  end

  def combobox(assigns) do
    assigns = assign_new(assigns, :value, fn -> if(assigns.multiple, do: [], else: nil) end)
    assigns = assign_new(assigns, :error, fn -> nil end)
    assigns = assign_new(assigns, :filter, fn -> "" end)
    assigns = assign_new(assigns, :open, fn -> false end)
    assigns = assign_new(assigns, :loading, fn -> false end)
    assigns = assign_new(assigns, :active_index, fn -> -1 end)
    assigns = assign_new(assigns, :filtered_options, fn ->
      if assigns.async_load, do: [], else: assigns.options
    end)

    ~H"""
    <div class={"relative #{assigns.class}"} phx-hook="Combobox">
      <div
        role="combobox"
        aria-haspopup="listbox"
        aria-owns={@id <> "-listbox"}
        aria-expanded={@open}
        aria-disabled={@disabled}
        aria-activedescendant={@active_index >= 0 && @id <> "-option-" <> to_string(@active_index) || nil}
        class="relative"
      >
        <.input
          id={@id}
          name={@name}
          type="text"
          value={@filter}
          placeholder={@placeholder}
          disabled={@disabled}
          readonly={@read_only}
          phx-change={:filter}
          phx-debounce={@debounce}
          phx-keydown={:keydown}
          aria-autocomplete="list"
          aria-controls={@id <> "-listbox"}
          {@rest}
        />
        <div class="right-0 absolute inset-y-0 flex items-center pr-2 pointer-events-none">
          {#if @loading}
            <.spinner size="sm" class="text-muted-foreground" />
          {#else}
            <.icon name="chevron-down" class="w-4 h-4 text-muted-foreground" />
          {/if}
        </div>
      </div>

      {#if @open}
        <div
          id={@id <> "-listbox"}
          role="listbox"
          aria-multiselectable={@multiple}
          class="z-50 absolute bg-popover ring-opacity-5 shadow-lg mt-1 py-1 rounded-md focus:outline-none ring-1 ring-black w-full max-h-60 overflow-auto text-popover-foreground sm:text-sm text-base"
          style={"transition: all var(--duration-normal) var(--easing-ease-in-out)"}
        >
          {#if @header}
            <div class="px-2 py-1.5 border-b border-border font-semibold text-sm">
              {#slot header /}
            </div>
          {/if}

          {#if @loading}
            <div class="px-4 py-2 text-muted-foreground text-center">
              Loading...
            </div>
          {#elseif @filtered_options == []}
            {#if @empty}
              {#slot empty /}
            {#else}
              <div class="px-4 py-2 text-muted-foreground text-center">
                No results found
              </div>
            {/if}
          {#else}
            {#each @filtered_options as option, i}
              <div
                id={@id <> "-option-" <> to_string(i)}
                role="option"
                aria-selected={@selected == option}
                class={[
                  "px-4 py-2 cursor-default select-none relative",
                  if(@active_index == i, do: "bg-accent text-accent-foreground", else: ""),
                  if(@selected == option, do: "font-medium", else: "text-foreground")
                ]}
                phx-click={:select}
                phx-value-index={i}
                phx-target={@myself}
              >
                {#slot option | option: option, selected: @selected == option, active: @active_index == i}
                  {apply(@option_label, [option])}
                {/slot}
              </div>
            {/each}
          {/if}

          {#if @footer}
            <div class="px-2 py-1.5 border-t border-border text-sm">
              {#slot footer /}
            </div>
          {/if}
        </div>
      {/if}
    </div>
    """
  end

  # Server-side implementation for handling events
  def handle_event("filter", %{"value" => filter}, socket) when byte_size(filter) >= @min_chars do
    filtered = Enum.filter(socket.assigns.options, fn option ->
      String.contains?(String.downcase(apply(socket.assigns.option_label, [option])), String.downcase(filter))
    end)

    {:noreply, assign(socket, filtered_options: filtered, filter: filter, open: true)}
  end

  def handle_event("filter", %{"value" => filter}, socket) do
    {:noreply, assign(socket, filter: filter, open: false)}
  end

  def handle_event("select", %{"index" => index}, socket) do
    option = Enum.at(socket.assigns.filtered_options, String.to_integer(index))
    selected = if socket.assigns.multiple, do: [option | socket.assigns.value], else: option

    {:noreply, assign(socket,
      value: selected,
      filter: apply(socket.assigns.option_label, [option]),
      open: not socket.assigns.multiple,
      active_index: 0
    )}
  end

  def handle_event("toggle", _, socket) do
    {:noreply, assign(socket, open: not socket.assigns.open)}
  end

  def handle_event("close", _, socket) do
    {:noreply, assign(socket, open: false)}
  end

  def handle_event("keydown", %{"key" => "ArrowDown"}, socket) do
    new_index = min(socket.assigns.active_index + 1, length(socket.assigns.filtered_options) - 1)
    {:noreply, assign(socket, active_index: new_index)}
  end

  def handle_event("keydown", %{"key" => "ArrowUp"}, socket) do
    new_index = max(socket.assigns.active_index - 1, 0)
    {:noreply, assign(socket, active_index: new_index)}
  end

  def handle_event("keydown", %{"key" => "Enter"}, socket) do
    if socket.assigns.active_index >= 0 do
      option = Enum.at(socket.assigns.filtered_options, socket.assigns.active_index)
      selected = if socket.assigns.multiple, do: [option | socket.assigns.value], else: option

      {:noreply, assign(socket,
        value: selected,
        filter: apply(socket.assigns.option_label, [option]),
        open: not socket.assigns.multiple,
        active_index: 0
      )}
    else
      {:noreply, socket}
    end
  end

  def handle_event("keydown", %{"key" => "Escape"}, socket) do
    {:noreply, assign(socket, open: false)}
  end
end

  @doc """
  Context provider for option state management
  """
  def option_context(socket) do
    %{
      selected: socket.assigns.value,
      active_index: socket.assigns.active_index,
      filter: socket.assigns.filter,
      open: socket.assigns.open
    }
  end

  @doc """
  Updates option selection state
  """
  def update_selection(socket, option) do
    if socket.assigns.multiple do
      current = socket.assigns.value || []
      if Enum.any?(current, &(&1 == option)) do
        assign(socket, value: List.delete(current, option))
      else
        assign(socket, value: [option | current])
      end
    else
      assign(socket, value: option, open: false)
    end
  end

  @doc """
  Clears current selection
  """
  def clear_selection(socket) do
    assign(socket, value: if(socket.assigns.multiple, do: [], else: nil))
  end

  @doc """
  Updates filter state with debounce
  """
  def update_filter(socket, filter) do
    socket
    |> assign(filter: filter)
    |> debounce(socket.assigns.debounce, fn socket ->
      filtered =
        if byte_size(filter) >= socket.assigns.min_chars do
          Enum.filter(socket.assigns.options, fn option ->
            String.contains?(String.downcase(apply(socket.assigns.option_label, [option])), String.downcase(filter))
          end)
        else
          []
        end

      assign(socket, filtered_options: filtered, open: filtered != [])
    end)
  end
end
