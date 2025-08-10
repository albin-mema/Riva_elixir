defmodule ToastProviderStories do
  use Surface.LiveView

  alias RivaAshWeb.Components.Ui.ToastProvider
  alias RivaAshWeb.Components.Ui.Button

  data toast_count, :integer, default: 0

  def mount(_params, _session, socket) do
    {:ok, assign(socket, toast_count: 0)}
  end

  def render(assigns) do
    ~H"""
    <div class="mx-auto p-6 max-w-4xl">
      <ToastProvider id="toast-provider" />

      <div class="space-y-6">
        <div class="space-y-4">
          <h2 class="font-semibold text-xl">Basic Toast Queue Behavior</h2>
          <div class="flex flex-wrap gap-2">
            <Button variant={:primary} on_click={show_toast(:success)}>Show Success Toast</Button>
            <Button variant={:destructive} on_click={show_toast(:error)}>Show Error Toast</Button>
            <Button variant={:warning} on_click={show_toast(:warning)}>Show Warning Toast</Button>
            <Button variant={:secondary} on_click={show_toast(:info)}>Show Info Toast</Button>
          </div>
        </div>

        <div class="space-y-4">
          <h2 class="font-semibold text-xl">Different Toast Types and States</h2>
          <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
            <div class="p-4 border rounded-lg">
              <h3 class="mb-2 font-medium">Success Toast</h3>
              <div class="flex items-start gap-2 bg-success/10 p-3 border border-success/20 rounded">
                <span class="mt-0.5 w-5 h-5 text-success i-heroicons-check-circle-20-solid" />
                <p class="text-sm">Operation completed successfully!</p>
              </div>
            </div>
            <div class="p-4 border rounded-lg">
              <h3 class="mb-2 font-medium">Error Toast</h3>
              <div class="flex items-start gap-2 bg-destructive/10 p-3 border border-destructive/20 rounded">
                <span class="mt-0.5 w-5 h-5 text-destructive i-heroicons-exclamation-circle-20-solid" />
                <p class="text-sm">An error occurred while processing your request.</p>
              </div>
            </div>
            <div class="p-4 border rounded-lg">
              <h3 class="mb-2 font-medium">Warning Toast</h3>
              <div class="flex items-start gap-2 bg-warning/10 p-3 border border-warning/20 rounded">
                <span class="mt-0.5 w-5 h-5 text-warning i-heroicons-exclamation-triangle-20-solid" />
                <p class="text-sm">Please review your submission before proceeding.</p>
              </div>
            </div>
            <div class="p-4 border rounded-lg">
              <h3 class="mb-2 font-medium">Info Toast</h3>
              <div class="flex items-start gap-2 bg-primary/10 p-3 border border-primary/20 rounded">
                <span class="mt-0.5 w-5 h-5 text-primary i-heroicons-information-circle-20-solid" />
                <p class="text-sm">New feature available for your account.</p>
              </div>
            </div>
          </div>
        </div>

        <div class="space-y-4">
          <h2 class="font-semibold text-xl">Keyboard Navigation and Dismissal</h2>
          <div class="bg-muted/50 p-4 border rounded-lg">
            <p class="mb-2">Focus a toast with Tab/Shift+Tab and press Escape to dismiss it.</p>
            <p class="text-muted-foreground text-sm">Toasts are automatically focused when they appear for screen reader users.</p>
          </div>
        </div>

        <div class="space-y-4">
          <h2 class="font-semibold text-xl">Integration with AppShell</h2>
          <div class="border rounded-lg overflow-hidden">
            <div class="flex justify-between items-center bg-muted p-3 border-b">
              <div class="flex items-center gap-2">
                <span class="w-5 h-5 i-heroicons-bars-3-20-solid" />
                <span class="font-medium">AppShell Header</span>
              </div>
              <Button variant={:ghost} size={:sm}>Menu</Button>
            </div>
            <div class="flex justify-center items-center bg-background/50 p-6 min-h-[200px]">
              <div class="text-center">
                <span class="mx-auto mb-2 w-12 h-12 text-muted-foreground i-heroicons-inbox-20-solid" />
                <p class="mb-1 font-medium text-lg">AppShell Content Area</p>
                <p class="text-muted-foreground">Toasts appear above all content in the AppShell</p>
              </div>
            </div>
          </div>
        </div>

        <div class="space-y-4">
          <h2 class="font-semibold text-xl">Mobile Viewport Behavior</h2>
          <div class="mx-auto border rounded-lg max-w-[375px] overflow-hidden">
            <div class="bg-muted p-2 text-sm text-center">iPhone 12 Viewport (375×812)</div>
            <div class="relative bg-background p-2 min-h-[600px]">
              <div class="top-4 right-4 z-[100] absolute space-y-2 w-[calc(100%-2rem)] max-w-md">
                <div class="flex items-start gap-3 bg-card shadow-[var(--shadow-lg)] p-4 rounded-[var(--radius-md)] text-card-foreground transition-all duration-[var(--duration-normal)] ease-[var(--easing-ease-in-out)] transform">
                  <span class="mt-0.5 w-5 h-5 text-success i-heroicons-check-circle-20-solid" />
                  <div class="flex-1 min-w-0">
                    <p class="font-medium text-success text-sm">Operation completed successfully!</p>
                  </div>
                  <button class="hover:bg-muted !p-1 rounded shrink-0">
                    <span class="w-5 h-5 i-heroicons-x-mark-20-solid" />
                  </button>
                </div>
                <div class="flex items-start gap-3 bg-card shadow-[var(--shadow-lg)] p-4 rounded-[var(--radius-md)] text-card-foreground transition-all duration-[var(--duration-normal)] ease-[var(--easing-ease-in-out)] transform">
                  <span class="mt-0.5 w-5 h-5 text-destructive i-heroicons-exclamation-circle-20-solid" />
                  <div class="flex-1 min-w-0">
                    <p class="font-medium text-destructive text-sm">An error occurred while processing your request.</p>
                  </div>
                  <button class="hover:bg-muted !p-1 rounded shrink-0">
                    <span class="w-5 h-5 i-heroicons-x-mark-20-solid" />
                  </button>
                </div>
              </div>
              <div class="p-4 border-t">
                <p class="text-muted-foreground text-sm text-center">Stacked toasts on mobile viewport</p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def handle_event("show_toast", %{"type" => type}, socket) do
    type = String.to_existing_atom(type)
    toast_count = socket.assigns.toast_count + 1
    message = "Toast ##{toast_count} - #{String.capitalize(to_string(type))} message"

    socket =
      socket
      |> ToastProvider.add_toast(type, message, timeout: 5000)
      |> assign(toast_count: toast_count)

    {:noreply, socket}
  end

  defp show_toast(type) do
    %{"type" => Atom.to_string(type)}
  end
end
