defmodule RivaAshWeb.BusinessSetupLive do
  @moduledoc """
  Business Setup Wizard - Guided multi-step setup flow.
  Combines Business, Plot, Section, Layout, and Item configuration into a unified workflow.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  alias RivaAsh.Resources.{Business, Plot, Section, Layout, Item, ItemType}
  alias RivaAsh.ErrorHelpers

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button

  import RivaAshWeb.Components.Interactive.PlotLayoutDesigner
  import RivaAshWeb.Live.AuthHelpers

  @setup_steps [
    %{
      id: "business",
      title: "Business Information",
      description: "Basic business details and settings"
    },
    %{
      id: "layout",
      title: "Physical Layout",
      description: "Design your space with plots and sections"
    },
    %{
      id: "items",
      title: "Item Configuration",
      description: "Set up your bookable items and types"
    },
    %{
      id: "schedule",
      title: "Operating Hours",
      description: "Configure availability and schedules"
    },
    %{id: "pricing", title: "Pricing Setup", description: "Set up pricing rules and rates"},
    %{id: "review", title: "Review & Launch", description: "Review settings and go live"}
  ]

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, "Business Setup Wizard")
          |> assign(:current_step, "business")
          |> assign(:steps, @setup_steps)
          |> assign(:setup_data, %{})
          |> assign(:loading, false)
          |> assign(:errors, [])

        {:ok, socket}

      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="max-w-4xl mx-auto space-y-6">
      <!-- Page Header -->
      <.page_header title="🏗️ Business Setup Wizard" description="Let's get your business set up step by step">
        <:action>
          <.button phx-click="save_and_exit" variant="secondary">
            Save & Exit
          </.button>
        </:action>
      </.page_header>

      <!-- Progress Steps -->
      <div class="bg-white shadow rounded-lg">
        <div class="px-6 py-4">
          <nav aria-label="Progress">
            <ol class="flex items-center">
              <%= for {step, index} <- Enum.with_index(@steps) do %>
                <li class={["relative", if(index < length(@steps) - 1, do: "pr-8 sm:pr-20", else: "")]} >
                  <!-- Step connector line -->
                  <%= if index < length(@steps) - 1 do %>
                    <div class="absolute inset-0 flex items-center" aria-hidden="true">
                      <div class={[
                        "h-0.5 w-full",
                        if(step_completed?(step.id, @current_step, @steps), do: "bg-blue-600", else: "bg-gray-200")
                      ]}></div>
                    </div>
                  <% end %>

                  <!-- Step circle and content -->
                  <div class="relative flex items-center justify-center">
                    <div class={[
                      "h-8 w-8 rounded-full flex items-center justify-center text-sm font-medium",
                      cond do
                        step_completed?(step.id, @current_step, @steps) -> "bg-blue-600 text-white"
                        step.id == @current_step -> "bg-blue-100 text-blue-600 border-2 border-blue-600"
                        true -> "bg-gray-100 text-gray-400"
                      end
                    ]}>
                      <%= if step_completed?(step.id, @current_step, @steps) do %>
                        <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                          <path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd" />
                        </svg>
                      <% else %>
                        <%= index + 1 %>
                      <% end %>
                    </div>
                  </div>

                  <!-- Step title (hidden on mobile) -->
                  <div class="hidden sm:block absolute top-10 left-1/2 transform -translate-x-1/2 w-32 text-center">
                    <p class={[
                      "text-xs font-medium",
                      if(step.id == @current_step, do: "text-blue-600", else: "text-gray-500")
                    ]}>
                      <%= step.title %>
                    </p>
                  </div>
                </li>
              <% end %>
            </ol>
          </nav>
        </div>
      </div>

      <!-- Step Content -->
      <div class="bg-white shadow rounded-lg">
        <div class="px-6 py-8">
          <%= case @current_step do %>
            <% "business" -> %>
              <%= render_business_step(assigns) %>
            <% "layout" -> %>
              <%= render_layout_step(assigns) %>
            <% "items" -> %>
              <%= render_items_step(assigns) %>
            <% "schedule" -> %>
              <%= render_schedule_step(assigns) %>
            <% "pricing" -> %>
              <%= render_pricing_step(assigns) %>
            <% "review" -> %>
              <%= render_review_step(assigns) %>
          <% end %>
        </div>

        <!-- Navigation Buttons -->
        <div class="px-6 py-4 bg-gray-50 flex justify-between rounded-b-lg">
          <div>
            <%= if @current_step != "business" do %>
              <.button phx-click="previous_step" variant="secondary">
                ← Previous
              </.button>
            <% end %>
          </div>
          <div class="flex space-x-3">
            <%= if @current_step == "review" do %>
              <.button phx-click="launch_business" variant="primary" loading={@loading}>
                🚀 Launch Business
              </.button>
            <% else %>
              <.button phx-click="next_step" variant="primary">
                Next →
              </.button>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  # Step rendering functions
  defp render_business_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="text-2xl font-bold text-gray-900">Business Information</h2>
        <p class="mt-2 text-gray-600">Let's start with the basics about your business.</p>
      </div>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div>
          <label class="block text-sm font-medium text-gray-700 mb-2">Business Name</label>
          <input
            type="text"
            class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            placeholder="Enter your business name"
            value={@setup_data[:business_name] || ""}
            phx-blur="update_field"
            phx-value-field="business_name"
          />
        </div>

        <div>
          <label class="block text-sm font-medium text-gray-700 mb-2">Business Type</label>
          <select
            class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            phx-change="update_field"
            phx-value-field="business_type"
          >
            <option value="">Select business type</option>
            <option value="restaurant">Restaurant</option>
            <option value="hotel">Hotel</option>
            <option value="spa">Spa/Wellness</option>
            <option value="event_venue">Event Venue</option>
            <option value="coworking">Coworking Space</option>
            <option value="other">Other</option>
          </select>
        </div>

        <div class="md:col-span-2">
          <label class="block text-sm font-medium text-gray-700 mb-2">Description</label>
          <textarea
            rows="3"
            class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            placeholder="Describe your business..."
            phx-blur="update_field"
            phx-value-field="business_description"
          ><%= @setup_data[:business_description] || "" %></textarea>
        </div>

        <div>
          <label class="block text-sm font-medium text-gray-700 mb-2">Address</label>
          <input
            type="text"
            class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            placeholder="Business address"
            value={@setup_data[:address] || ""}
            phx-blur="update_field"
            phx-value-field="address"
          />
        </div>

        <div>
          <label class="block text-sm font-medium text-gray-700 mb-2">Phone</label>
          <input
            type="tel"
            class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            placeholder="Business phone"
            value={@setup_data[:phone] || ""}
            phx-blur="update_field"
            phx-value-field="phone"
          />
        </div>
      </div>
    </div>
    """
  end

  defp render_layout_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="text-2xl font-bold text-gray-900">Physical Layout</h2>
        <p class="mt-2 text-gray-600">Design your space by creating plots and sections.</p>
      </div>

      <div class="bg-gray-50 p-6 rounded-lg">
        <h3 class="text-lg font-medium text-gray-900 mb-4">Layout Designer</h3>
        <p class="text-gray-600 mb-4">Use the visual designer below to create your space layout.</p>

        <!-- Placeholder for layout designer component -->
        <div class="bg-white border-2 border-dashed border-gray-300 rounded-lg h-96 flex items-center justify-center">
          <div class="text-center">
            <svg class="mx-auto h-12 w-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 5a1 1 0 011-1h14a1 1 0 011 1v2a1 1 0 01-1 1H5a1 1 0 01-1-1V5zM4 13a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H5a1 1 0 01-1-1v-6zM16 13a1 1 0 011-1h2a1 1 0 011 1v6a1 1 0 01-1 1h-2a1 1 0 01-1-1v-6z" />
            </svg>
            <p class="mt-2 text-gray-500">Interactive Layout Designer</p>
            <p class="text-sm text-gray-400">Drag and drop to create your space</p>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp render_items_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="text-2xl font-bold text-gray-900">Item Configuration</h2>
        <p class="mt-2 text-gray-600">Set up the items that customers can book.</p>
      </div>

      <div class="space-y-4">
        <div class="flex justify-between items-center">
          <h3 class="text-lg font-medium text-gray-900">Your Items</h3>
          <.button phx-click="add_item" variant="primary" size="sm">
            + Add Item
          </.button>
        </div>

        <!-- Items list placeholder -->
        <div class="bg-gray-50 rounded-lg p-6">
          <div class="text-center text-gray-500">
            <svg class="mx-auto h-12 w-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 7l-8-4-8 4m16 0l-8 4m8-4v10l-8 4m0-10L4 7m8 4v10M4 7v10l8 4" />
            </svg>
            <p class="mt-2">No items configured yet</p>
            <p class="text-sm text-gray-400">Add your first bookable item to get started</p>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp render_schedule_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="text-2xl font-bold text-gray-900">Operating Hours</h2>
        <p class="mt-2 text-gray-600">Configure when your business is open for bookings.</p>
      </div>

      <div class="bg-gray-50 rounded-lg p-6">
        <p class="text-center text-gray-500">Schedule configuration interface will be implemented here</p>
      </div>
    </div>
    """
  end

  defp render_pricing_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="text-2xl font-bold text-gray-900">Pricing Setup</h2>
        <p class="mt-2 text-gray-600">Set up your pricing rules and rates.</p>
      </div>

      <div class="bg-gray-50 rounded-lg p-6">
        <p class="text-center text-gray-500">Pricing configuration interface will be implemented here</p>
      </div>
    </div>
    """
  end

  defp render_review_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="text-2xl font-bold text-gray-900">Review & Launch</h2>
        <p class="mt-2 text-gray-600">Review your settings and launch your business.</p>
      </div>

      <div class="bg-green-50 border border-green-200 rounded-lg p-6">
        <div class="flex items-center">
          <svg class="w-6 h-6 text-green-600 mr-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
          <div>
            <h3 class="text-lg font-medium text-green-900">Ready to Launch!</h3>
            <p class="text-green-700">Your business setup is complete and ready to go live.</p>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("next_step", _params, socket) do
    current_step_index = Enum.find_index(@setup_steps, &(&1.id == socket.assigns.current_step))

    if current_step_index < length(@setup_steps) - 1 do
      next_step = Enum.at(@setup_steps, current_step_index + 1)
      {:noreply, assign(socket, :current_step, next_step.id)}
    else
      {:noreply, socket}
    end
  end

  def handle_event("previous_step", _params, socket) do
    current_step_index = Enum.find_index(@setup_steps, &(&1.id == socket.assigns.current_step))

    if current_step_index > 0 do
      previous_step = Enum.at(@setup_steps, current_step_index - 1)
      {:noreply, assign(socket, :current_step, previous_step.id)}
    else
      {:noreply, socket}
    end
  end

  def handle_event("update_field", %{"field" => field, "value" => value}, socket) do
    setup_data = Map.put(socket.assigns.setup_data, String.to_atom(field), value)
    {:noreply, assign(socket, :setup_data, setup_data)}
  end

  def handle_event("add_item", _params, socket) do
    # Placeholder for adding items
    {:noreply, socket}
  end

  def handle_event("launch_business", _params, socket) do
    socket = assign(socket, :loading, true)

    # Here you would create the business and related resources
    # For now, just redirect to dashboard
    Process.send_after(self(), :complete_setup, 2000)

    {:noreply, socket}
  end

  def handle_event("save_and_exit", _params, socket) do
    {:noreply, push_navigate(socket, to: "/dashboard")}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info(:complete_setup, socket) do
    socket =
      socket
      |> assign(:loading, false)
      |> put_flash(:info, "🎉 Business setup completed successfully!")

    {:noreply, push_navigate(socket, to: "/dashboard")}
  end

  # Helper functions
  defp step_completed?(step_id, current_step, steps) do
    current_index = Enum.find_index(steps, &(&1.id == current_step))
    step_index = Enum.find_index(steps, &(&1.id == step_id))

    step_index < current_index
  end
end
