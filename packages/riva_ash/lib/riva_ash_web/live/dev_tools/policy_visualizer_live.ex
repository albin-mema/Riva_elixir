defmodule RivaAshWeb.DevTools.PolicyVisualizerLive do
  @moduledoc """
  Advanced policy decision tree visualizer.

  Features:
  - Interactive policy decision trees
  - Real-time policy evaluation
  - Policy simulation with different actors
  - Authorization flow visualization
  - Policy performance analysis
  """
  use RivaAshWeb, :live_view

  if Mix.env() != :dev do
    def mount(_params, _session, socket) do
      {:ok, redirect(socket, to: "/")}
    end
  else
    alias RivaAsh.DevTools.PolicyService

    @impl true
    def mount(_params, _session, socket) do
      socket =
        socket
        |> assign(:page_title, get_page_title())
        |> assign(:selected_resource, nil)
        |> assign(:selected_action, nil)
        |> assign(:test_actor, nil)
        |> assign(:policy_tree, nil)
        |> assign(:evaluation_result, nil)
        |> assign(:policy_trace, [])
        |> assign(:simulation_mode, false)
        |> PolicyService.load_initial_data()

      {:ok, socket}
    end

    @impl true
    def handle_event("select_resource", %{"resource" => resource}, socket) do
      PolicyService.select_resource(socket, resource)
    end

    def handle_event("select_action", %{"action" => action}, socket) do
      PolicyService.select_action(socket, action)
    end

    def handle_event("select_actor", %{"actor_id" => actor_id}, socket) do
      PolicyService.select_actor(socket, actor_id)
    end

    def handle_event("simulate_policy", _params, socket) do
      PolicyService.simulate_policy(socket)
    end

    def handle_event("reset_simulation", _params, socket) do
      PolicyService.reset_simulation(socket)
    end

    def handle_event("generate_mermaid", _params, socket) do
      PolicyService.generate_mermaid(socket)
    end

    @impl true
    def handle_info({:show_mermaid, diagram}, socket) do
      {:noreply, assign(socket, :mermaid_diagram, diagram)}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div class="min-h-screen bg-gray-50 p-6">
        <div class="max-w-7xl mx-auto">
          <div class="bg-white shadow rounded-lg">
            <div class="border-b border-gray-200 px-6 py-4">
              <h1 class="text-2xl font-bold text-gray-900">Policy Decision Tree Visualizer</h1>
              <p class="text-gray-600 mt-1">Visualize and simulate authorization policy decisions</p>
            </div>

            <div class="p-6">
              <!-- Resource and Action Selection -->
              <div class="grid grid-cols-1 md:grid-cols-3 gap-6 mb-6">
                <div>
                  <label class="block text-sm font-medium text-gray-700 mb-2">Resource</label>
                  <select phx-change="select_resource" class="w-full rounded border-gray-300">
                    <option value="">Select Resource</option>
                    <%= for resource <- @available_resources do %>
                      <option value={resource.name} selected={@selected_resource == resource.module}>
                        <%= resource.name %>
                      </option>
                    <% end %>
                  </select>
                </div>

                <%= if @selected_resource do %>
                  <div>
                    <label class="block text-sm font-medium text-gray-700 mb-2">Action</label>
                    <select phx-change="select_action" class="w-full rounded border-gray-300">
                      <option value="">Select Action</option>
                      <%= for action <- @available_actions do %>
                        <option value={action} selected={@selected_action == action}>
                          <%= action %>
                        </option>
                      <% end %>
                    </select>
                  </div>
                <% end %>

                <%= if @selected_action do %>
                  <div>
                    <label class="block text-sm font-medium text-gray-700 mb-2">Test Actor</label>
                    <select phx-change="select_actor" class="w-full rounded border-gray-300">
                      <option value="">Select Actor</option>
                      <%= for actor <- @test_actors do %>
                        <option value={actor.id} selected={@test_actor && @test_actor.id == actor.id}>
                          <%= actor.email %> (<%= actor.role %>)
                        </option>
                      <% end %>
                    </select>
                  </div>
                <% end %>
              </div>

              <%= if @selected_resource && @selected_action do %>
                <!-- Policy Controls -->
                <div class="mb-6 flex items-center justify-between">
                  <div class="flex items-center space-x-4">
                    <button
                      phx-click="simulate_policy"
                      disabled={!@test_actor}
                      class="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
                    >
                      Simulate Policy Evaluation
                    </button>

                    <button
                      phx-click="reset_simulation"
                      class="bg-gray-600 text-white px-4 py-2 rounded hover:bg-gray-700"
                    >
                      Reset
                    </button>

                    <button
                      phx-click="generate_mermaid"
                      class="bg-purple-600 text-white px-4 py-2 rounded hover:bg-purple-700"
                    >
                      Show Decision Tree
                    </button>
                  </div>

                  <%= if @evaluation_result do %>
                    <div class="flex items-center">
                      <span class="text-sm text-gray-600 mr-2">Result:</span>
                      <span class={[
                        "px-3 py-1 rounded-full text-sm font-medium",
                        case @evaluation_result.decision do
                          :authorized -> "bg-green-100 text-green-800"
                          :forbidden -> "bg-red-100 text-red-800"
                          _ -> "bg-yellow-100 text-yellow-800"
                        end
                      ]}>
                        <%= @evaluation_result.decision |> to_string() |> String.capitalize() %>
                      </span>
                    </div>
                  <% end %>
                </div>

                <!-- Policy Tree Structure -->
                <%= if @policy_tree do %>
                  <div class="mb-6">
                    <h3 class="text-lg font-semibold text-gray-900 mb-3">Policy Structure</h3>
                    <div class="bg-gray-50 rounded-lg p-4">
                      <.render_policy_tree tree={@policy_tree} trace={@policy_trace} />
                    </div>
                  </div>
                <% end %>

                <!-- Mermaid Decision Tree -->
                <%= if assigns[:mermaid_diagram] do %>
                  <div class="mb-6">
                    <h3 class="text-lg font-semibold text-gray-900 mb-3">Decision Tree Diagram</h3>
                    <div class="border border-gray-300 rounded-lg p-4 bg-white">
                      <div id="policy-mermaid-diagram" phx-hook="MermaidDiagram" data-diagram={@mermaid_diagram}>
                        <pre class="text-sm text-gray-600"><%= @mermaid_diagram %></pre>
                      </div>
                    </div>
                  </div>
                <% end %>

                <!-- Policy Evaluation Trace -->
                <%= if @policy_trace != [] do %>
                  <div class="mb-6">
                    <h3 class="text-lg font-semibold text-gray-900 mb-3">Evaluation Trace</h3>
                    <div class="space-y-3">
                      <%= for {step, index} <- Enum.with_index(@policy_trace) do %>
                        <div class={[
                          "border rounded-lg p-4",
                          case step.result do
                            :authorized -> "border-green-300 bg-green-50"
                            :forbidden -> "border-red-300 bg-red-50"
                            :unknown -> "border-yellow-300 bg-yellow-50"
                            _ -> "border-gray-300 bg-gray-50"
                          end
                        ]}>
                          <div class="flex items-center justify-between mb-2">
                            <div class="flex items-center">
                              <span class={[
                                "w-6 h-6 rounded-full flex items-center justify-center text-xs font-medium mr-3",
                                case step.result do
                                  :authorized -> "bg-green-600 text-white"
                                  :forbidden -> "bg-red-600 text-white"
                                  :unknown -> "bg-yellow-600 text-white"
                                  _ -> "bg-gray-600 text-white"
                                end
                              ]}>
                                <%= index + 1 %>
                              </span>
                              <h4 class="font-semibold text-gray-900"><%= step.policy_name %></h4>
                            </div>

                            <div class="flex items-center space-x-2">
                              <span class={[
                                "px-2 py-1 rounded text-xs font-medium",
                                case step.result do
                                  :authorized -> "bg-green-100 text-green-800"
                                  :forbidden -> "bg-red-100 text-red-800"
                                  :unknown -> "bg-yellow-100 text-yellow-800"
                                  _ -> "bg-gray-100 text-gray-800"
                                end
                              ]}>
                                <%= step.result %>
                              </span>

                              <%= if step.duration do %>
                                <span class="text-xs text-gray-500"><%= step.duration %>ms</span>
                              <% end %>
                            </div>
                          </div>

                          <%= if step.condition do %>
                            <div class="mt-2">
                              <span class="text-sm font-medium text-gray-700">Condition:</span>
                              <pre class="text-xs bg-white p-2 rounded mt-1 overflow-x-auto"><%= step.condition %></pre>
                            </div>
                          <% end %>

                          <%= if step.reason do %>
                            <div class="mt-2">
                              <span class="text-sm font-medium text-gray-700">Reason:</span>
                              <p class="text-sm text-gray-600 mt-1"><%= step.reason %></p>
                            </div>
                          <% end %>
                        </div>
                      <% end %>
                    </div>
                  </div>
                <% end %>

                <!-- Policy Performance Analysis -->
                <%= if @evaluation_result && @evaluation_result.performance do %>
                  <div class="mb-6">
                    <h3 class="text-lg font-semibold text-gray-900 mb-3">Performance Analysis</h3>
                    <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
                      <div class="bg-blue-50 rounded-lg p-4">
                        <div class="text-2xl font-bold text-blue-900"><%= @evaluation_result.performance.total_time %>ms</div>
                        <div class="text-sm text-blue-600">Total Evaluation Time</div>
                      </div>

                      <div class="bg-green-50 rounded-lg p-4">
                        <div class="text-2xl font-bold text-green-900"><%= @evaluation_result.performance.policies_evaluated %></div>
                        <div class="text-sm text-green-600">Policies Evaluated</div>
                      </div>

                      <div class="bg-purple-50 rounded-lg p-4">
                        <div class="text-2xl font-bold text-purple-900"><%= @evaluation_result.performance.cache_hits %></div>
                        <div class="text-sm text-purple-600">Cache Hits</div>
                      </div>
                    </div>
                  </div>
                <% end %>
              <% end %>
            </div>
          </div>
        </div>
      </div>
      """
    end

    defp render_policy_tree(assigns) do
      ~H"""
      <div class="policy-tree">
        <%= render_policy_node(@tree, @trace, 0) %>
      </div>
      """
    end

    defp render_policy_node(node, trace, depth) do
      trace_result = get_trace_result(node.name, trace)

      assigns = %{node: node, trace_result: trace_result, depth: depth, trace: trace}

      ~H"""
      <div class={["policy-node", "ml-#{@depth * 4}", "mb-2"]}>
        <div class={[
          "flex items-center p-3 rounded border",
          case @trace_result do
            :authorized -> "border-green-300 bg-green-50"
            :forbidden -> "border-red-300 bg-red-50"
            :unknown -> "border-yellow-300 bg-yellow-50"
            _ -> "border-gray-300 bg-gray-50"
          end
        ]}>
          <div class={[
            "w-3 h-3 rounded-full mr-3",
            case @trace_result do
              :authorized -> "bg-green-500"
              :forbidden -> "bg-red-500"
              :unknown -> "bg-yellow-500"
              _ -> "bg-gray-400"
            end
          ]}></div>

          <div class="flex-1">
            <div class="font-medium text-gray-900"><%= @node.name %></div>
            <%= if @node.description do %>
              <div class="text-sm text-gray-600"><%= @node.description %></div>
            <% end %>
          </div>

          <%= if @trace_result do %>
            <div class={[
              "px-2 py-1 rounded text-xs font-medium",
              case @trace_result do
                :authorized -> "bg-green-100 text-green-800"
                :forbidden -> "bg-red-100 text-red-800"
                :unknown -> "bg-yellow-100 text-yellow-800"
                _ -> "bg-gray-100 text-gray-800"
              end
            ]}>
              <%= @trace_result %>
            </div>
          <% end %>
        </div>

        <%= if @node.children && @node.children != [] do %>
          <div class="mt-2">
            <%= for child <- @node.children do %>
              <%= render_policy_node(child, @trace, @depth + 1) %>
            <% end %>
          </div>
        <% end %>
      </div>
      """
    end

    # Helper functions
    defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, [])[:page_title] || "Policy Decision Tree Visualizer"

    # Policy trace functions
    defp get_trace_result(_node_name, _trace) do
      %{result: :allowed, reason: "Mock trace result"}
    end
  end
end
