
defmodule RivaAshWeb.BusinessSetupLive do
  @moduledoc """
  Business Setup Wizard - Guided multi-step setup flow.
  Combines Business, Plot, Section, Layout, and Item configuration into a unified workflow.

  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Uses AuthHelpers for authentication and business scoping
  - Delegates business logic to BusinessSetup context
  - Handles UI state and user interactions
  - Uses proper Ash error handling
  - Implements CRUD operations through Ash actions
  """

  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  alias RivaAsh.BusinessSetup
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
    case mount_business_scoped(
           socket,
           session,
           Business,
           [:owner_id],
           "Business Setup Wizard"
         ) do
      {:ok, socket} ->
        {:ok,
         socket
         |> assign(:current_step, "business")
         |> assign(:steps, @setup_steps)
         |> assign(:setup_data, %{})
         |> assign(:loading, false)
         |> assign(:errors, [])}

      {:error, _unmatched} = error ->
        {:ok, error}
    end
  end

  @impl true
  def handle_params(params, _url, socket) do
    # Handle step navigation and data loading
    case BusinessSetup.get_setup_progress(socket.assigns.current_user, params) do
      {:ok, progress_data} ->
        {:noreply,
         socket
         |> assign(:setup_data, progress_data.setup_data)
         |> assign(:current_step, progress_data.current_step)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load setup progress: #{reason}")
         |> assign(loading: false)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="space-y-6 mx-auto max-w-4xl">
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
                  <div class="relative flex justify-center items-center">
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
                  <div class="hidden sm:block top-10 left-1/2 absolute w-32 text-center -translate-x-1/2 transform">
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
        <div class="flex justify-between bg-gray-50 px-6 py-4 rounded-b-lg">
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
        <h2 class="font-bold text-gray-900 text-2xl">Business Information</h2>
        <p class="mt-2 text-gray-600">Let's start with the basics about your business.</p>
      </div>

      <%= if @errors[:business] do %>
        <div class="bg-red-50 p-4 border border-red-200 rounded-lg">
          <p class="text-red-800"><%= @errors[:business] %></p>
        </div>
      <% end %>

      <.form id="business-form" for={@changeset} phx-submit="save_business_step" class="space-y-6">
        <div class="gap-6 grid grid-cols-1 md:grid-cols-2">
          <div>
            <label class="block mb-2 font-medium text-gray-700 text-sm">Business Name</label>
            <input
              type="text"
              name="business[name]"
              class="px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
              placeholder="Enter your business name"
              value={@setup_data[:business_name] || ""}
              phx-blur="update_field"
              phx-value-field="business_name"
            />
          </div>

          <div>
            <label class="block mb-2 font-medium text-gray-700 text-sm">Business Type</label>
            <select
              name="business[business_type]"
              class="px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
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
            <label class="block mb-2 font-medium text-gray-700 text-sm">Description</label>
            <textarea
              name="business[description]"
              rows="3"
              class="px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
              placeholder="Describe your business..."
              phx-blur="update_field"
              phx-value-field="business_description"
            ><%= @setup_data[:business_description] || "" %></textarea>
          </div>

          <div>
            <label class="block mb-2 font-medium text-gray-700 text-sm">Address</label>
            <input
              type="text"
              name="business[address]"
              class="px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
              placeholder="Business address"
              value={@setup_data[:address] || ""}
              phx-blur="update_field"
              phx-value-field="address"
            />
          </div>

          <div>
            <label class="block mb-2 font-medium text-gray-700 text-sm">Phone</label>
            <input
              type="tel"
              name="business[phone]"
              class="px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
              placeholder="Business phone"
              value={@setup_data[:phone] || ""}
              phx-blur="update_field"
              phx-value-field="phone"
            />
          </div>
        </div>

        <div class="flex justify-end">
          <.button type="submit" variant="primary">
            Save Business Information
          </.button>
        </div>
      </.form>
    </div>
    """
  end

  defp render_layout_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="font-bold text-gray-900 text-2xl">Physical Layout</h2>
        <p class="mt-2 text-gray-600">Design your space by creating plots and sections.</p>
      </div>

      <div class="bg-gray-50 p-6 rounded-lg">
        <h3 class="mb-4 font-medium text-gray-900 text-lg">Layout Designer</h3>
        <p class="mb-4 text-gray-600">Use the visual designer below to create your space layout.</p>

        <.plot_layout_designer
          plots={@setup_data[:plots] || []}
          on_plot_change="update_layout"
          on_section_change="update_layout"
        />
      </div>

      <div class="flex justify-end">
        <.button phx-click="save_layout_step" variant="primary">
          Save Layout
        </.button>
      </div>
    </div>
    """
  end

  defp render_items_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="font-bold text-gray-900 text-2xl">Item Configuration</h2>
        <p class="mt-2 text-gray-600">Set up the items that customers can book.</p>
      </div>

      <div class="space-y-4">
        <div class="flex justify-between items-center">
          <h3 class="font-medium text-gray-900 text-lg">Your Items</h3>
          <.button phx-click="add_item" variant="primary" size="sm">
            + Add Item
          </.button>
        </div>

        <%= if @setup_data[:items] && length(@setup_data[:items]) > 0 do %>
          <div class="space-y-3">
            <%= for item <- @setup_data[:items] do %>
              <.card>
                <:title><%= item.name %></:title>
                <p class="text-gray-600"><%= item.description || "No description" %></p>
                <div class="flex justify-between items-center mt-4">
                  <span class="text-gray-500 text-sm">
                    <%= item.item_type.name %> • <%= item.duration %> min
                  </span>
                  <div class="space-x-2">
                    <.button phx-click="edit_item" phx-value-id={item.id} variant="outline" size="sm">
                      Edit
                    </.button>
                    <.button phx-click="delete_item" phx-value-id={item.id} variant="destructive" size="sm">
                      Delete
                    </.button>
                  </div>
                </div>
              </.card>
            <% end %>
          </div>
        <% else %>
          <div class="bg-gray-50 p-6 rounded-lg">
            <div class="text-gray-500 text-center">
              <svg class="mx-auto w-12 h-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 7l-8-4-8 4m16 0l-8 4m8-4v10l-8 4m0-10L4 7m8 4v10M4 7v10l8 4" />
              </svg>
              <p class="mt-2">No items configured yet</p>
              <p class="text-gray-400 text-sm">Add your first bookable item to get started</p>
            </div>
          </div>
        <% end %>
      </div>

      <div class="flex justify-end">
        <.button phx-click="save_items_step" variant="primary">
          Save Items
        </.button>
      </div>
    </div>
    """
  end

  defp render_schedule_step(assigns) do
    assigns =
      assigns
      |> Phoenix.Component.assign_new(:setup_data, fn -> %{} end)

    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="font-bold text-gray-900 text-2xl">Operating Hours</h2>
        <p class="mt-2 text-gray-600">Configure when your business is open for bookings.</p>
      </div>

      <.form id="schedule-form" for={@changeset} phx-submit="save_schedule_step" class="space-y-6">
        <div class="gap-6 grid grid-cols-1 md:grid-cols-2">
          <%= for day <- ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"] do %>
            <% day_key = String.downcase(day) %>
            <% day_map = Map.get(@setup_data[:schedule] || %{}, day_key, %{}) %>
            <div class="p-4 border border-gray-200 rounded-lg">
              <h4 class="mb-3 font-medium text-gray-900"><%= day %></h4>
              <div class="space-y-3">
                <div class="flex items-center space-x-2">
                  <input
                    type="checkbox"
                    name={"schedule[#{day_key}][enabled]"}
                    id={"schedule_#{day_key}_enabled"}
                    class="border-gray-300 rounded focus:ring-blue-500 text-blue-600"
                    checked={Map.get(day_map, "enabled", false)}
                  />
                  <label for={"schedule_#{day_key}_enabled"} class="text-gray-700 text-sm">
                    Open
                  </label>
                </div>
                <div class="gap-2 grid grid-cols-2">
                  <div>
                    <label class="block mb-1 text-gray-500 text-xs">Open Time</label>
                    <input
                      type="time"
                      name={"schedule[#{day_key}][open_time]"}
                      class="px-2 py-1 border border-gray-300 rounded w-full text-sm"
                      value={Map.get(day_map, "open_time", "")}
                    />
                  </div>
                  <div>
                    <label class="block mb-1 text-gray-500 text-xs">Close Time</label>
                    <input
                      type="time"
                      name={"schedule[#{day_key}][close_time]"}
                      class="px-2 py-1 border border-gray-300 rounded w-full text-sm"
                      value={Map.get(day_map, "close_time", "")}
                    />
                  </div>
                </div>
              </div>
            </div>
          <% end %>
        </div>

        <div class="flex justify-end">
          <.button type="submit" variant="primary">
            Save Schedule
          </.button>
        </div>
      </.form>
    </div>
    """
  end

  defp render_pricing_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="font-bold text-gray-900 text-2xl">Pricing Setup</h2>
        <p class="mt-2 text-gray-600">Set up your pricing rules and rates.</p>
      </div>

      <.form id="pricing-form" for={@changeset} phx-submit="save_pricing_step" class="space-y-6">
        <div class="gap-6 grid grid-cols-1 md:grid-cols-2">
          <div>
            <label class="block mb-2 font-medium text-gray-700 text-sm">Currency</label>
            <select
              name="pricing[currency]"
              class="px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
            >
              <option value="USD">USD - US Dollar</option>
              <option value="EUR">EUR - Euro</option>
              <option value="GBP">GBP - British Pound</option>
              <option value="JPY">JPY - Japanese Yen</option>
            </select>
          </div>

          <div>
            <label class="block mb-2 font-medium text-gray-700 text-sm">Tax Rate (%)</label>
            <input
              type="number"
              name="pricing[tax_rate]"
              step="0.01"
              min="0"
              max="100"
              class="px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
              value={@setup_data[:pricing]["tax_rate"] || 0}
            />
          </div>

          <div class="md:col-span-2">
            <label class="block mb-2 font-medium text-gray-700 text-sm">Pricing Rules</label>
            <div class="space-y-3">
              <div class="flex items-center space-x-2">
                <input
                  type="checkbox"
                  name="pricing[enable_dynamic_pricing]"
                  id="dynamic_pricing"
                  class="border-gray-300 rounded focus:ring-blue-500 text-blue-600"
                  checked={@setup_data[:pricing]["enable_dynamic_pricing"] || false}
                />
                <label for="dynamic_pricing" class="text-gray-700 text-sm">
                  Enable dynamic pricing based on demand
                </label>
              </div>
              <div class="flex items-center space-x-2">
                <input
                  type="checkbox"
                  name="pricing[enable_discounts]"
                  id="discounts"
                  class="border-gray-300 rounded focus:ring-blue-500 text-blue-600"
                  checked={@setup_data[:pricing]["enable_discounts"] || false}
                />
                <label for="discounts" class="text-gray-700 text-sm">
                  Enable discount codes
                </label>
              </div>
            </div>
          </div>
        </div>

        <div class="flex justify-end">
          <.button type="submit" variant="primary">
            Save Pricing
          </.button>
        </div>
      </.form>
    </div>
    """
  end

  defp render_review_step(assigns) do
    ~H"""
    <div class="space-y-6">
      <div>
        <h2 class="font-bold text-gray-900 text-2xl">Review & Launch</h2>
        <p class="mt-2 text-gray-600">Review your settings and launch your business.</p>
      </div>

      <div class="gap-6 grid grid-cols-1 md:grid-cols-2">
        <.card>
          <:header>
            <h3 class="font-medium text-lg">Business Information</h3>
          </:header>
          <:body>
            <dl class="space-y-2">
              <div>
                <dt class="font-medium text-gray-500 text-sm">Name</dt>
                <dd class="text-gray-900 text-sm"><%= @setup_data[:business_name] %></dd>
              </div>
              <div>
                <dt class="font-medium text-gray-500 text-sm">Type</dt>
                <dd class="text-gray-900 text-sm"><%= @setup_data[:business_type] %></dd>
              </div>
              <div>
                <dt class="font-medium text-gray-500 text-sm">Description</dt>
                <dd class="text-gray-900 text-sm"><%= @setup_data[:business_description] %></dd>
              </div>
            </dl>
          </:body>
        </.card>

        <.card>
          <:header>
            <h3 class="font-medium text-lg">Configuration Summary</h3>
          </:header>
          <:body>
            <dl class="space-y-2">
              <div>
                <dt class="font-medium text-gray-500 text-sm">Items Configured</dt>
                <dd class="text-gray-900 text-sm"><%= length(@setup_data[:items] || []) %> items</dd>
              </div>
              <div>
                <dt class="font-medium text-gray-500 text-sm">Plots Created</dt>
                <dd class="text-gray-900 text-sm"><%= length(@setup_data[:plots] || []) %> plots</dd>
              </div>
              <div>
                <dt class="font-medium text-gray-500 text-sm">Operating Hours</dt>
                <dd class="text-gray-900 text-sm"><%= Enum.count(@setup_data[:schedule] || [], fn {_unmatched, day} -> day["enabled"] end) %> days</dd>
              </div>
            </dl>
          </:body>
        </.card>
      </div>

      <div class="bg-green-50 p-6 border border-green-200 rounded-lg">
        <div class="flex items-center">
          <svg class="mr-3 w-6 h-6 text-green-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
          <div>
            <h3 class="font-medium text-green-900 text-lg">Ready to Launch!</h3>
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
    setup_data = Map.put(socket.assigns.setup_data, String.to_existing_atom(field), value)
    {:noreply, assign(socket, :setup_data, setup_data)}
  end

  def handle_event("save_business_step", %{"business" => business_params}, socket) do
    case BusinessSetup.save_business_step(socket.assigns.current_user, business_params) do
      {:ok, _business} ->
        {:noreply,
         socket
         |> put_flash(:info, "Business information saved successfully")
         |> push_patch(to: ~p"/business-setup?step=layout")}

      {:error, %Ash.Error.Invalid{errors: errors}} ->
        error_message = ErrorHelpers.format_error(%Ash.Error.Invalid{errors: errors})
        error_message = if is_map(error_message), do: error_message.message, else: to_string(error_message)

        {:noreply,
         socket
         |> put_flash(:error, "Failed to save business information: #{error_message}")
         |> assign(:errors, %{business: error_message})}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to save business information: #{reason}")
         |> assign(:errors, %{business: reason})}
    end
  end

  def handle_event("save_layout_step", _params, socket) do
    case BusinessSetup.save_layout_step(socket.assigns.current_user, socket.assigns.setup_data) do
      {:ok, _layout} ->
        {:noreply,
         socket
         |> put_flash(:info, "Layout saved successfully")
         |> push_patch(to: ~p"/business-setup?step=items")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to save layout: #{reason}")}
    end
  end

  def handle_event("save_items_step", _params, socket) do
    case BusinessSetup.save_items_step(socket.assigns.current_user, socket.assigns.setup_data) do
      {:ok, _items} ->
        {:noreply,
         socket
         |> put_flash(:info, "Items saved successfully")
         |> push_patch(to: ~p"/business-setup?step=schedule")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to save items: #{reason}")}
    end
  end

  def handle_event("save_schedule_step", %{"schedule" => schedule_params}, socket) do
    case BusinessSetup.save_schedule_step(socket.assigns.current_user, schedule_params) do
      {:ok, _schedule} ->
        {:noreply,
         socket
         |> put_flash(:info, "Schedule saved successfully")
         |> push_patch(to: ~p"/business-setup?step=pricing")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to save schedule: #{reason}")}
    end
  end

  def handle_event("save_pricing_step", %{"pricing" => pricing_params}, socket) do
    case BusinessSetup.save_pricing_step(socket.assigns.current_user, pricing_params) do
      {:ok, _pricing} ->
        {:noreply,
         socket
         |> put_flash(:info, "Pricing saved successfully")
         |> push_patch(to: ~p"/business-setup?step=review")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to save pricing: #{reason}")}
    end
  end

  def handle_event("add_item", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/business-setup/items/new")}
  end

  def handle_event("edit_item", %{"id" => id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/business-setup/items/#{id}/edit")}
  end

  def handle_event("delete_item", %{"id" => id}, socket) do
    case BusinessSetup.delete_item(socket.assigns.current_user, id) do
      {:ok, _item} ->
        {:noreply,
         socket
         |> put_flash(:info, "Item deleted successfully")
         |> push_patch(to: ~p"/business-setup?step=items")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete item: #{reason}")}
    end
  end

  def handle_event("update_layout", %{"layout" => layout_data}, socket) do
    setup_data = Map.put(socket.assigns.setup_data, :layout, layout_data)
    {:noreply, assign(socket, :setup_data, setup_data)}
  end

  def handle_event("launch_business", _params, socket) do
    socket = assign(socket, :loading, true)

    case BusinessSetup.complete_setup(socket.assigns.current_user, socket.assigns.setup_data) do
      {:ok, business} ->
        {:noreply,
         socket
         |> assign(:loading, false)
         |> put_flash(:info, "🎉 Business setup completed successfully!")
         |> push_navigate(to: ~p"/businesses/#{business.id}")}

      {:error, %Ash.Error.Invalid{errors: errors}} ->
        error_message = ErrorHelpers.format_error(%Ash.Error.Invalid{errors: errors})
        error_message = if is_map(error_message), do: error_message.message, else: to_string(error_message)

        {:noreply,
         socket
         |> assign(:loading, false)
         |> put_flash(:error, "Failed to launch business: #{error_message}")}

      {:error, reason} ->
        {:noreply,
         socket
         |> assign(:loading, false)
         |> put_flash(:error, "Failed to launch business: #{reason}")}
    end
  end

  def handle_event("save_and_exit", _params, socket) do
    case BusinessSetup.save_progress(socket.assigns.current_user, socket.assigns.setup_data) do
      {:ok, _progress} ->
        {:noreply,
         socket
         |> put_flash(:info, "Setup progress saved successfully")
         |> push_navigate(to: ~p"/dashboard")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to save progress: #{reason}")
         |> push_navigate(to: ~p"/dashboard")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp step_completed?(step_id, current_step, steps) do
    current_index = Enum.find_index(steps, &(&1.id == current_step))
    step_index = Enum.find_index(steps, &(&1.id == step_id))

    step_index < current_index
  end
end
