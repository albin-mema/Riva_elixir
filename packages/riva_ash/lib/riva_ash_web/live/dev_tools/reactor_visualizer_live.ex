defmodule RivaAshWeb.DevTools.ReactorVisualizerLive do
  @moduledoc """
  Visualizes and executes Reactor flows step-by-step.

  Features:
  - Interactive reactor flow diagrams
  - Step-by-step execution
  - Input/output inspection
  - Error handling visualization
  - Performance metrics
  """
  use RivaAshWeb, :live_view

  if Mix.env() != :dev do
    def mount(_params, _session, socket) do
      {:ok, redirect(socket, to: "/")}
    end
  else
    alias RivaAsh.DevTools.ReactorService

    @impl true
    def mount(_params, _session, socket) do
      socket =
        socket
        |> assign(:page_title, get_page_title())
        |> assign(:selected_reactor, nil)
        |> assign(:reactor_state, :idle)
        |> assign(:execution_steps, [])
        |> assign(:current_step, 0)
        |> assign(:inputs, %{})
        |> assign(:outputs, %{})
        |> assign(:errors, [])
        |> assign(:performance_metrics, %{})
        |> ReactorService.load_initial_data()

      {:ok, socket}
    end

    @impl true
    def handle_event("select_reactor", %{"reactor" => reactor_name}, socket) do
      ReactorService.select_reactor(socket, reactor_name)
    end

    def handle_event("update_input", %{"field" => field, "value" => value}, socket) do
      ReactorService.update_input(socket, field, value)
    end

    def handle_event("execute_reactor", _params, socket) do
      ReactorService.execute_reactor(socket)
    end

    def handle_event("step_execution", _params, socket) do
      ReactorService.step_execution(socket)
    end

    def handle_event("reset_execution", _params, socket) do
      ReactorService.reset_execution(socket)
    end

    def handle_event("generate_mermaid", _params, socket) do
      ReactorService.generate_mermaid(socket)
    end

    def handle_event("toggle_diagram_view", _params, socket) do
      ReactorService.toggle_diagram_view(socket)
    end

    def handle_event("copy_diagram_code", _params, socket) do
      ReactorService.copy_diagram_code(socket)
    end

    @impl true
    def handle_info(:start_execution, socket) do
      reactor = socket.assigns.selected_reactor
      inputs = socket.assigns.inputs

      # Start reactor execution with step-by-step tracking
      task = Task.async(fn -> execute_reactor_with_tracking(reactor, inputs) end)

      socket =
        socket
        |> assign(:execution_task, task)
        |> assign(:reactor_state, :executing)

      {:noreply, socket}
    end

    def handle_info(:continue_execution, socket) do
      # Continue to next step
      current_step = socket.assigns.current_step + 1

      socket =
        socket
        |> assign(:current_step, current_step)
        |> assign(:reactor_state, :executing)

      {:noreply, socket}
    end

    def handle_info({:execution_step, step_data}, socket) do
      steps = socket.assigns.execution_steps ++ [step_data]

      socket =
        socket
        |> assign(:execution_steps, steps)
        |> assign(:reactor_state, :paused)

      {:noreply, socket}
    end

    def handle_info({:execution_complete, result}, socket) do
      socket =
        socket
        |> assign(:reactor_state, :completed)
        |> assign(:outputs, result)

      {:noreply, socket}
    end

    def handle_info({:execution_error, error}, socket) do
      errors = socket.assigns.errors ++ [error]

      socket =
        socket
        |> assign(:reactor_state, :error)
        |> assign(:errors, errors)

      {:noreply, socket}
    end

    def handle_info({:show_mermaid, diagram}, socket) do
      # This would integrate with a mermaid rendering component
      {:noreply, assign(socket, :mermaid_diagram, diagram)}
    end

    def handle_info({ref, result}, socket) when is_reference(ref) do
      # Handle task completion
      Process.demonitor(ref, [:flush])

      case result do
        {:ok, outputs} ->
          handle_info({:execution_complete, outputs}, socket)
        {:error, error} ->
          handle_info({:execution_error, error}, socket)
      end
    end

    def handle_info({:DOWN, _ref, :process, _pid, _reason}, socket) do
      # Task crashed
      {:noreply, assign(socket, :reactor_state, :error)}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div class="min-h-screen bg-gray-50 p-6">
        <div class="max-w-7xl mx-auto">
          <div class="bg-white shadow rounded-lg">
            <div class="border-b border-gray-200 px-6 py-4">
              <h1 class="text-2xl font-bold text-gray-900">Reactor Flow Visualizer</h1>
              <p class="text-gray-600 mt-1">Visualize and execute reactor workflows step-by-step</p>
            </div>

            <div class="p-6">
              <!-- Reactor Selection -->
              <div class="mb-6">
                <label class="block text-sm font-medium text-gray-700 mb-2">Select Reactor</label>
                <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <%= for reactor <- @available_reactors do %>
                    <button
                      phx-click="select_reactor"
                      phx-value-reactor={reactor.name}
                      class={[
                        "p-4 border rounded-lg text-left transition-colors",
                        if(@selected_reactor == reactor.module,
                          do: "border-blue-500 bg-blue-50",
                          else: "border-gray-300 hover:border-gray-400")
                      ]}
                    >
                      <h3 class="font-semibold text-gray-900"><%= reactor.name %></h3>
                      <p class="text-sm text-gray-600 mt-1"><%= reactor.description %></p>
                      <div class="mt-2">
                        <span class="text-xs bg-gray-100 text-gray-700 px-2 py-1 rounded">
                          <%= reactor.step_count %> steps
                        </span>
                      </div>
                    </button>
                  <% end %>
                </div>
              </div>

              <%= if @selected_reactor do %>
                <!-- Reactor Controls -->
                <div class="mb-6 flex items-center justify-between">
                  <div class="flex items-center space-x-4">
                    <button
                      phx-click="execute_reactor"
                      disabled={@reactor_state in [:executing]}
                      class="bg-green-600 text-white px-4 py-2 rounded hover:bg-green-700 disabled:opacity-50 disabled:cursor-not-allowed"
                    >
                      <%= if @reactor_state == :executing, do: "Executing...", else: "Execute Reactor" %>
                    </button>

                    <%= if @reactor_state == :paused do %>
                      <button
                        phx-click="step_execution"
                        class="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700"
                      >
                        Next Step
                      </button>
                    <% end %>

                    <button
                      phx-click="reset_execution"
                      class="bg-gray-600 text-white px-4 py-2 rounded hover:bg-gray-700"
                    >
                      Reset
                    </button>

                    <button
                      phx-click="generate_mermaid"
                      class="bg-purple-600 text-white px-4 py-2 rounded hover:bg-purple-700"
                    >
                      Show Diagram
                    </button>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm text-gray-600 mr-2">Status:</span>
                    <span class={[
                      "px-3 py-1 rounded-full text-sm font-medium",
                      case @reactor_state do
                        :idle -> "bg-gray-100 text-gray-800"
                        :ready -> "bg-blue-100 text-blue-800"
                        :executing -> "bg-yellow-100 text-yellow-800"
                        :paused -> "bg-orange-100 text-orange-800"
                        :completed -> "bg-green-100 text-green-800"
                        :error -> "bg-red-100 text-red-800"
                      end
                    ]}>
                      <%= @reactor_state |> to_string() |> String.capitalize() %>
                    </span>
                  </div>
                </div>

                <!-- Input Configuration -->
                <div class="mb-6">
                  <h3 class="text-lg font-semibold text-gray-900 mb-3">Reactor Inputs</h3>
                  <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <%= for {key, value} <- @inputs do %>
                      <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">
                          <%= key |> to_string() |> String.replace("_", " ") |> String.capitalize() %>
                        </label>
                        <textarea
                          phx-change="update_input"
                          phx-value-field={key}
                          class="w-full border border-gray-300 rounded px-3 py-2 text-sm"
                          rows="3"
                          placeholder="Enter JSON or Elixir term"
                        ><%= inspect(value, pretty: true) %></textarea>
                      </div>
                    <% end %>
                  </div>
                </div>

                <!-- Execution Steps Visualization -->
                <%= if @execution_steps != [] do %>
                  <div class="mb-6">
                    <h3 class="text-lg font-semibold text-gray-900 mb-3">Execution Flow</h3>
                    <div class="space-y-4">
                      <%= for {step, index} <- Enum.with_index(@execution_steps) do %>
                        <div class={[
                          "border rounded-lg p-4",
                          if(index <= @current_step, do: "border-green-300 bg-green-50", else: "border-gray-300 bg-gray-50")
                        ]}>
                          <div class="flex items-center justify-between mb-2">
                            <div class="flex items-center">
                              <span class={[
                                "w-8 h-8 rounded-full flex items-center justify-center text-sm font-medium mr-3",
                                if(index <= @current_step, do: "bg-green-600 text-white", else: "bg-gray-400 text-white")
                              ]}>
                                <%= index + 1 %>
                              </span>
                              <h4 class="font-semibold text-gray-900"><%= step.name %></h4>
                            </div>

                            <%= if step.duration do %>
                              <span class="text-sm text-gray-600"><%= step.duration %>ms</span>
                            <% end %>
                          </div>

                          <%= if step.input do %>
                            <div class="mt-2">
                              <span class="text-sm font-medium text-gray-700">Input:</span>
                              <pre class="text-xs bg-white p-2 rounded mt-1 overflow-x-auto"><%= inspect(step.input, pretty: true, limit: 3) %></pre>
                            </div>
                          <% end %>

                          <%= if step.output do %>
                            <div class="mt-2">
                              <span class="text-sm font-medium text-gray-700">Output:</span>
                              <pre class="text-xs bg-white p-2 rounded mt-1 overflow-x-auto"><%= inspect(step.output, pretty: true, limit: 3) %></pre>
                            </div>
                          <% end %>

                          <%= if step.error do %>
                            <div class="mt-2 p-2 bg-red-100 border border-red-300 rounded">
                              <span class="text-sm font-medium text-red-700">Error:</span>
                              <pre class="text-xs text-red-600 mt-1"><%= step.error %></pre>
                            </div>
                          <% end %>
                        </div>
                      <% end %>
                    </div>
                  </div>
                <% end %>

                <!-- Mermaid Diagram -->
                <%= if assigns[:mermaid_diagram] do %>
                  <div class="mb-6">
                    <div class="flex justify-between items-center mb-3">
                      <h3 class="text-lg font-semibold text-gray-900">Reactor Flow Diagram</h3>
                      <button
                        phx-click="toggle_diagram_view"
                        class="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700 transition-colors"
                      >
                        <%= if assigns[:show_diagram_code], do: "Hide Code", else: "Show Code" %>
                      </button>
                    </div>

                    <%= if assigns[:show_diagram_code] do %>
                      <!-- Code View -->
                      <div class="border border-gray-300 rounded-lg bg-gray-900 text-green-400">
                        <div class="flex justify-between items-center p-3 border-b border-gray-700">
                          <span class="text-sm font-medium">Mermaid Code</span>
                          <button
                            phx-click="copy_diagram_code"
                            class="bg-gray-700 hover:bg-gray-600 text-white px-3 py-1 rounded text-sm transition-colors"
                          >
                            Copy Code
                          </button>
                        </div>
                        <div class="p-4 overflow-x-auto">
                          <pre class="text-sm font-mono whitespace-pre-wrap"><%= @mermaid_diagram %></pre>
                        </div>
                        <div class="p-3 border-t border-gray-700 text-xs text-gray-400">
                          Copy this code and paste it into any Mermaid editor like:
                          <a href="https://mermaid.live" target="_blank" class="text-blue-400 hover:text-blue-300 underline ml-1">
                            mermaid.live
                          </a>
                        </div>
                      </div>
                    <% else %>
                      <!-- Diagram Preview -->
                      <div class="border border-gray-300 rounded-lg p-6 bg-white">
                        <div class="text-center text-gray-500 mb-6">
                          <svg class="w-16 h-16 mx-auto mb-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2-2V7a2 2 0 012-2h2a2 2 0 002 2v2a2 2 0 002 2h2a2 2 0 002-2V7a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 00-2 2h-2a2 2 0 00-2 2v6a2 2 0 01-2 2H9z"></path>
                          </svg>
                          <p class="text-lg font-medium">Reactor Flow Diagram</p>
                          <p class="text-sm mb-4">Click "Show Code" to view the Mermaid diagram code that you can copy and paste into any Mermaid editor</p>

                          <div class="flex justify-center space-x-4">
                            <a
                              href="https://mermaid.live"
                              target="_blank"
                              class="inline-flex items-center px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 transition-colors"
                            >
                              <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"></path>
                              </svg>
                              Open Mermaid Live Editor
                            </a>

                            <a
                              href="https://mermaid.js.org/intro/"
                              target="_blank"
                              class="inline-flex items-center px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-700 transition-colors"
                            >
                              <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>
                              </svg>
                              Mermaid Docs
                            </a>
                          </div>
                        </div>

                        <!-- Reactor Steps Preview -->
                        <div class="bg-gray-50 rounded-lg p-4">
                          <h4 class="font-medium text-gray-900 mb-3">Reactor Steps:</h4>
                          <.render_text_flow reactor={@selected_reactor} />
                        </div>
                      </div>
                    <% end %>
                  </div>
                <% end %>

                <!-- Final Outputs -->
                <%= if @outputs != %{} do %>
                  <div class="mb-6">
                    <h3 class="text-lg font-semibold text-gray-900 mb-3">Final Outputs</h3>
                    <div class="bg-green-50 border border-green-200 rounded-lg p-4">
                      <pre class="text-sm text-gray-800 overflow-x-auto"><%= inspect(@outputs, pretty: true, limit: :infinity) %></pre>
                    </div>
                  </div>
                <% end %>

                <!-- Errors -->
                <%= if @errors != [] do %>
                  <div class="mb-6">
                    <h3 class="text-lg font-semibold text-red-900 mb-3">Errors</h3>
                    <div class="space-y-2">
                      <%= for error <- @errors do %>
                        <div class="bg-red-50 border border-red-200 rounded-lg p-4">
                          <pre class="text-sm text-red-800"><%= inspect(error, pretty: true) %></pre>
                        </div>
                      <% end %>
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

    # Helper functions
    defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, [])[:page_title] || "Reactor Flow Visualizer"
  end
end
