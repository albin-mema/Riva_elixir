defmodule RivaAshWeb.QueryBuilderLive do
  use RivaAshWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_event("qb_changed", %{"tree" => tree}, socket) do
    {:noreply, assign(socket, :qb_tree, tree)}
  end

  @impl true
  def render(assigns) do
    assigns = assign_new(assigns, :qb_tree, fn -> %{} end)

    ~H"""
    <div class="p-6 space-y-4">
      <h2 class="text-xl font-semibold">React Awesome Query Builder</h2>
      <div id="qb-demo"
           phx-hook="LiveReact"
           data-live-react-class="QueryBuilder"
           data-live-react-props={Jason.encode!(%{config: %{}, value: nil, useTokens: true})}>
      </div>

      <div class="mt-4 text-sm text-gray-600">
        <p>LiveView received tree:</p>
        <pre class="bg-gray-100 p-2 rounded overflow-x-auto"><%= inspect(@qb_tree, pretty: true, limit: :infinity) %></pre>
      </div>
    </div>
    """
  end
end

