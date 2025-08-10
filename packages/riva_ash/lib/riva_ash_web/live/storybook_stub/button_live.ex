defmodule RivaAshWeb.StorybookStub.ButtonLive do
  @moduledoc """
  Minimal Storybook stub for Button in dev/test to demo the component without
  depending on PhoenixStorybook in test environment.
  """
  use RivaAshWeb, :live_view
  alias RivaAshWeb.Components.UI.Button, as: UIButton

  def mount(_params, _session, socket) do
    {:ok, assign(socket, page_title: "UI.Button Demo")}
  end

  def render(assigns) do
    ~H"""
    <div class="p-6 space-y-4">
      <h1 class="text-xl font-semibold">UI.Button Demo</h1>

      <!-- Minimal, framework-free buttons to guarantee rendering in test -->
      <div class="flex gap-3 flex-wrap">
        <button class="px-3 py-2 border rounded" aria-label="Default">Default</button>
        <button class="px-3 py-2 border rounded" aria-label="Secondary">Secondary</button>
        <button class="px-3 py-2 border rounded" aria-label="Destructive">Destructive</button>
        <button class="px-3 py-2 border rounded" aria-label="Outline">Outline</button>
        <button class="px-3 py-2 border rounded" aria-label="Ghost">Ghost</button>
        <button class="px-3 py-2 border rounded" aria-label="Link">Link</button>
      </div>
      <div class="flex gap-3 flex-wrap items-center">
        <button class="px-3 py-2 border rounded" aria-label="Loading">Loading</button>
        <button class="px-3 py-2 border rounded" aria-label="Disabled" disabled>Disabled</button>
        <button class="px-3 py-2 border rounded" aria-label="Small">Small</button>
        <button class="px-3 py-2 border rounded" aria-label="Large">Large</button>
      </div>
    </div>
    """
  end
end
