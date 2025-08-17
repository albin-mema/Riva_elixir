defmodule RivaAshWeb.ReactButtonLive do
  use RivaAshWeb, :live_view

  # Renders a bare page with a single React button mounted via LiveReact hook.
  # No menus/sidebars/layouts – we use the :browser_no_layout pipeline in router.

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_event("simple_clicked", %{"count" => count}, socket) do
    {:noreply, put_flash(socket, :info, "Clicked #{count} times")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6">
      <h2 class="text-xl font-semibold mb-4">LiveReact demo</h2>
      <div id="simple-react-demo"
           phx-hook="LiveReact"
           data-live-react-class="SimpleReact"
           data-live-react-props={Jason.encode!(%{label: "Click me", count: 0})}>
      </div>

      <div class="mt-8">
        <h3 class="font-medium mb-2">React Awesome Query Builder</h3>
        <div id="qb-demo"
             class="border rounded p-3 min-h-[200px]"
             phx-hook="LiveReact"
             data-live-react-class="QueryBuilder"
             data-live-react-props={Jason.encode!(%{
               config: %{
                 fields: %{
                   "name" => %{"label" => "Name", "type" => "text"},
                   "age" => %{"label" => "Age", "type" => "number"}
                 }
               },
               value: nil,
               useTokens: true
             })}>
        </div>
      </div>

      <p class="mt-4 text-gray-600">Check the flash for click count increments.</p>
    </div>
    """
  end
end
