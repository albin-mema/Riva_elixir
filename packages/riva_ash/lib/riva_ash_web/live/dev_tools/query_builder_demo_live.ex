defmodule RivaAshWeb.DevTools.QueryBuilderDemoLive do
  use RivaAshWeb, :live_view
  alias RivaAshWeb.Components.Interactive.QueryBuilder

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:qb_tree, nil)
     |> assign(:qb_json, %{})
     |> assign(:qb_config, demo_config())}
  end

  @impl true
  def handle_event("qb_changed", %{"tree" => tree}, socket) do
    {:noreply, assign(socket, :qb_json, tree)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-4 space-y-4">
      <h1 class="text-xl font-semibold">Query Builder Demo</h1>
      <div class="border rounded p-2">
        <QueryBuilder.query_builder id="qb-demo" config={@qb_config} value={nil} use_tokens={true} />
      </div>
      <div class="border rounded p-2 bg-muted">
        <h2 class="font-medium mb-2">Latest JSON</h2>
        <pre class="text-xs overflow-auto max-h-80"><%= Jason.Formatter.pretty_print(Jason.encode!(@qb_json)) %></pre>
      </div>
    </div>
    """
  end

  defp demo_config do
    %{
      "fields" => %{
        "name" => %{label: "Name", type: "text"},
        "age" => %{label: "Age", type: "number"},
        "is_active" => %{label: "Active?", type: "boolean", operators: ["equal"]},
        "color" => %{
          label: "Color",
          type: "select",
          fieldSettings: %{
            listValues: [
              %{value: "yellow", title: "Yellow"},
              %{value: "green", title: "Green"},
              %{value: "orange", title: "Orange"}
            ]
          }
        }
      }
    }
  end
end
