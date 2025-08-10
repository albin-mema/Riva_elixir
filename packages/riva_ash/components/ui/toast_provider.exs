defmodule RivaAshWeb.Components.Ui.ToastProvider do
  use Surface.Component
  alias RivaAshWeb.Components.Ui.Atoms
  alias RivaAshWeb.Components.Ui.IconButton

  data toasts, :list, default: []
  data live_region_id, :string, default: "toast-live-region"

  @toast_types [:success, :error, :warning, :info]
  @politeness_levels [:polite, :assertive]
  @default_timeout 5000
  @animation_duration 300  # Matches --duration-normal from tokens

  @doc """
  Adds a new toast to the queue with the specified type and message
  """
  def add_toast(socket, type, message, opts \\ []) when type in @toast_types do
    id = Ecto.UUID.generate()
    timeout = opts[:timeout] || @default_timeout
    dismissible = Keyword.get(opts, :dismissible, true)
    politeness = if opts[:polite], do: :polite, else: :assertive

    toast = %{
      id: id,
      type: type,
      message: message,
      timeout: timeout,
      dismissible: dismissible,
      politeness: politeness,
      removing: false
    }

    new_toasts = [toast | socket.assigns.toasts]
    socket = assign(socket, toasts: new_toasts)

    # Announce to screen readers with correct politeness
    send(self(), {:announce_toast, message, politeness})

    # Set up auto-dismissal
    if dismissible do
      Process.send_after(self(), {:dismiss_toast, id}, timeout)
    end

    socket
  end

  @doc """
  Dismisses a toast by ID with animation
  """
  def dismiss_toast(socket, id) do
    toasts = Enum.map(socket.assigns.toasts, fn
      %{id: ^id, removing: false} = toast -> %{toast | removing: true}
      toast -> toast
    end)

    # Start exit animation and schedule removal
    Process.send_after(self(), {:remove_toast, id}, @animation_duration)

    assign(socket, toasts: toasts)
  end

  def handle_info({:announce_toast, message, politeness}, socket) do
    # Update live region content for screen readers with correct politeness
    {:noreply, assign(socket, announcement: message, politeness: politeness)}
  end

  def handle_info({:dismiss_toast, id}, socket) do
    {:noreply, dismiss_toast(socket, id)}
  end

  def handle_info({:remove_toast, id}, socket) do
    toasts = Enum.reject(socket.assigns.toasts, &(&1.id == id))
    {:noreply, assign(socket, toasts: toasts)}
  end

  def handle_event("keydown", %{"key" => "Escape"}, socket) do
    # Dismiss focused toast when escape is pressed
    focused_toast_id = get_focused_toast_id(socket)
    if focused_toast_id, do: dismiss_toast(socket, focused_toast_id)
    {:noreply, socket}
  end

  defp get_focused_toast_id(socket) do
    # Logic to determine which toast has focus would go here
    # For simplicity, we'll assume the first toast is focused
    case socket.assigns.toasts do
      [%{id: id} | _] -> id
      _ -> nil
    end
  end

  def render(assigns) do
    ~H"""
    <div>
      <!-- ARIA live region for screen reader announcements -->
      <div
        id={@live_region_id}
        role="status"
        aria-live={@politeness || "polite"}
        aria-atomic="true"
        class="sr-only"
      >
        {@announcement}
      </div>

      <!-- Toast container -->
      <div class="top-4 right-4 z-[100] fixed space-y-2 w-full md:w-auto max-w-md">
        {#for toast <- @toasts}
          <div
            id={"toast-#{toast.id}"}
            class={[
              "transform transition-all duration-[var(--duration-normal)] ease-[var(--easing-ease-in-out)]",
              "rounded-[var(--radius-md)] shadow-[var(--shadow-lg)] p-4",
              "flex items-start gap-3",
              "focus-within:outline-none focus-within:ring-2 focus-within:ring-ring focus-within:ring-offset-2",
              if(toast.removing, do: "motion-safe:animate-fade-out", else: "motion-safe:animate-fade-in")
            ]}
            phx-hook="Toast"
            data-type={toast.type}
            tabindex="0"
            phx-keydown={__MODULE__}
          >
            <!-- Toast icon based on type -->
            <span class="mt-0.5 shrink-0">
              {toast_icon(toast.type)}
            </span>

            <!-- Toast content -->
            <div class="flex-1 min-w-0">
              <p class="text-sm font-medium {toast_text_color(toast.type)}">
                {toast.message}
              </p>
            </div>

            <!-- Dismiss button -->
            {#if toast.dismissible}
              <IconButton
                variant={:ghost}
                size={:sm}
                aria_label="Dismiss"
                on_click={fn _ -> dismiss_toast(@socket, toast.id) end}
                class="!p-1 shrink-0"
              >
                <span class="w-5 h-5 i-heroicons-x-mark-20-solid" />
              </IconButton>
            {/if}
          </div>
        {/for}
      </div>
    </div>
    """
  end

  defp toast_icon(type) do
    icon_class = "w-5 h-5"

    case type do
      :success -> ~H(<span class={"i-heroicons-check-circle-20-solid #{icon_class} text-success"} />)
      :error -> ~H(<span class={"i-heroicons-exclamation-circle-20-solid #{icon_class} text-destructive"} />)
      :warning -> ~H(<span class={"i-heroicons-exclamation-triangle-20-solid #{icon_class} text-warning"} />)
      :info -> ~H(<span class={"i-heroicons-information-circle-20-solid #{icon_class} text-primary"} />)
    end
  end

  defp toast_text_color(type) do
    case type do
      :success -> "text-success"
      :error -> "text-destructive"
      :warning -> "text-warning"
      :info -> "text-primary"
    end
  end
end
