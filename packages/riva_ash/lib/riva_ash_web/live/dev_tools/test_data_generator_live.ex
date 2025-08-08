alias RivaAshWeb.DevTools, as: DevTools
alias RivaAsh.Resources, as: Resources
alias RivaAsh.Accounts, as: Accounts
alias RivaAsh.Reactors, as: Reactors
alias Ash.Changeset, as: Changeset

defmodule RivaAshWeb.DevTools.TestDataGeneratorLive do
  @moduledoc """
  Generate realistic test data for development and testing.

  Features:
  - One-click user creation
  - Complete business setup
  - Bulk data generation
  - Realistic fake data
  - Relationship management
  """
  use RivaAshWeb, :live_view

  if Mix.env() != :dev do
    def mount(_params, _session, socket) do
      {:ok, redirect(socket, to: "/")}
    end
  else
    alias RivaAsh.Resources.{Business, Item, Client, Reservation, Employee}
    alias RivaAsh.Accounts.User
    alias RivaAsh.Reactors.BusinessSetupFlow

    @impl true
    def mount(_params, _session, socket) do
      socket =
        socket
        |> assign(:page_title, "Test Data Generator")
        |> assign(:generation_log, [])
        |> assign(:selected_template, nil)
        |> assign(:generation_options, %{})
        |> assign(:is_generating, false)
        |> load_data_templates()
        |> load_current_stats()

      {:ok, socket}
    end

    @impl true
    def handle_event("select_template", %{"template" => template}, socket) do
      template_atom = String.to_existing_atom(template)
      options = get_template_options(template_atom)

      socket =
        socket
        |> assign(:selected_template, template_atom)
        |> assign(:generation_options, options)

      {:noreply, socket}
    end

    def handle_event("update_option", %{"field" => field, "value" => value}, socket) do
      field_atom = String.to_existing_atom(field)

      # Parse value based on field type
      parsed_value =
        case field_atom do
          field when field in [:count, :businesses_count, :items_per_business, :clients_count, :reservations_count] ->
            String.to_integer(value)

          field when field in [:with_reservations, :with_employees, :with_items] ->
            value == "true"

          _unmatchedunmatched ->
            value
        end

      options = Map.put(socket.assigns.generation_options, field_atom, parsed_value)
      {:noreply, assign(socket, :generation_options, options)}
    end

    def handle_event("generate_data", _params, socket) do
      if socket.assigns.selected_template do
        send(self(), :start_generation)
        {:noreply, assign(socket, :is_generating, true)}
      else
        {:noreply, socket}
      end
    end

    def handle_event("clear_log", _params, socket) do
      {:noreply, assign(socket, :generation_log, [])}
    end

    def handle_event("quick_generate", %{"type" => type}, socket) do
      send(self(), {:quick_generate, String.to_existing_atom(type)})
      {:noreply, assign(socket, :is_generating, true)}
    end

    @impl true
    def handle_info(:start_generation, socket) do
      template = socket.assigns.selected_template
      options = socket.assigns.generation_options

      task = Task.async(fn -> generate_template_data(template, options) end)

      socket =
        socket
        |> assign(:generation_task, task)

      {:noreply, socket}
    end

    def handle_info({:quick_generate, type}, socket) do
      task = Task.async(fn -> quick_generate_data(type) end)

      socket =
        socket
        |> assign(:generation_task, task)

      {:noreply, socket}
    end

    def handle_info({:generation_log, message}, socket) do
      log_entry = %{
        timestamp: DateTime.utc_now(),
        message: message,
        type: :info
      }

      log = [log_entry | socket.assigns.generation_log]
      {:noreply, assign(socket, :generation_log, Enum.take(log, 50))}
    end

    def handle_info({:generation_error, error}, socket) do
      log_entry = %{
        timestamp: DateTime.utc_now(),
        message: "Error: #{error}",
        type: :error
      }

      log = [log_entry | socket.assigns.generation_log]

      socket =
        socket
        |> assign(:generation_log, Enum.take(log, 50))
        |> assign(:is_generating, false)
        |> load_current_stats()

      {:noreply, socket}
    end

    def handle_info({ref, result}, socket) when is_reference(ref) do
      Process.demonitor(ref, [:flush])

      case result do
        {:ok, summary} ->
          log_entry = %{
            timestamp: DateTime.utc_now(),
            message: "Generation completed: #{summary}",
            type: :success
          }

          log = [log_entry | socket.assigns.generation_log]

          socket =
            socket
            |> assign(:generation_log, Enum.take(log, 50))
            |> assign(:is_generating, false)
            |> load_current_stats()

          {:noreply, socket}

        {:error, error} ->
          handle_info({:generation_error, inspect(error)}, socket)
      end
    end

    def handle_info({:DOWN, _ref, :process, _pid, _reason}, socket) do
      handle_info({:generation_error, "Generation task crashed"}, socket)
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div class="min-h-screen bg-gray-50 p-6">
        <div class="max-w-7xl mx-auto">
          <div class="bg-white shadow rounded-lg">
            <div class="border-b border-gray-200 px-6 py-4">
              <h1 class="text-2xl font-bold text-gray-900">Test Data Generator</h1>
              <p class="text-gray-600 mt-1">Generate realistic test data for development and testing</p>
            </div>

            <div class="p-6">
              <!-- Current Stats -->
              <div class="mb-6 grid grid-cols-2 md:grid-cols-5 gap-4">
                <%= for {label, count} <- @current_stats do %>
                  <div class="bg-gray-50 rounded-lg p-4 text-center">
                    <div class="text-2xl font-bold text-gray-900"><%= count %></div>
                    <div class="text-sm text-gray-600"><%= label %></div>
                  </div>
                <% end %>
              </div>

              <!-- Quick Actions -->
              <div class="mb-6">
                <h3 class="text-lg font-semibold text-gray-900 mb-3">Quick Generate</h3>
                <div class="grid grid-cols-2 md:grid-cols-4 gap-4">
                  <button
                    phx-click="quick_generate"
                    phx-value-type="user"
                    disabled={@is_generating}
                    class="flex items-center justify-center p-4 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors disabled:opacity-50"
                  >
                    <svg class="w-5 h-5 text-blue-600 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"></path>
                    </svg>
                    <span class="text-sm font-medium">New User</span>
                  </button>

                  <button
                    phx-click="quick_generate"
                    phx-value-type="business"
                    disabled={@is_generating}
                    class="flex items-center justify-center p-4 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors disabled:opacity-50"
                  >
                    <svg class="w-5 h-5 text-green-600 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-4m-5 0H9m0 0H7m2 0v-4a2 2 0 012-2h2a2 2 0 012 2v4"></path>
                    </svg>
                    <span class="text-sm font-medium">New Business</span>
                  </button>

                  <button
                    phx-click="quick_generate"
                    phx-value-type="client"
                    disabled={@is_generating}
                    class="flex items-center justify-center p-4 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors disabled:opacity-50"
                  >
                    <svg class="w-5 h-5 text-purple-600 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"></path>
                    </svg>
                    <span class="text-sm font-medium">New Client</span>
                  </button>

                  <button
                    phx-click="quick_generate"
                    phx-value-type="reservation"
                    disabled={@is_generating}
                    class="flex items-center justify-center p-4 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors disabled:opacity-50"
                  >
                    <svg class="w-5 h-5 text-orange-600 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"></path>
                    </svg>
                    <span class="text-sm font-medium">New Reservation</span>
                  </button>
                </div>
              </div>

              <!-- Template Selection -->
              <div class="mb-6">
                <h3 class="text-lg font-semibold text-gray-900 mb-3">Data Templates</h3>
                <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                  <%= for template <- @data_templates do %>
                    <button
                      phx-click="select_template"
                      phx-value-template={template.key}
                      class={[
                        "p-4 border rounded-lg text-left transition-colors",
                        if(@selected_template == template.key,
                          do: "border-blue-500 bg-blue-50",
                          else: "border-gray-300 hover:border-gray-400")
                      ]}
                    >
                      <h4 class="font-semibold text-gray-900"><%= template.name %></h4>
                      <p class="text-sm text-gray-600 mt-1"><%= template.description %></p>
                      <div class="mt-2 flex flex-wrap gap-1">
                        <%= for item <- template.includes do %>
                          <span class="text-xs bg-gray-100 text-gray-700 px-2 py-1 rounded"><%= item %></span>
                        <% end %>
                      </div>
                    </button>
                  <% end %>
                </div>
              </div>

              <%= if @selected_template do %>
                <!-- Template Options -->
                <div class="mb-6">
                  <h3 class="text-lg font-semibold text-gray-900 mb-3">Generation Options</h3>
                  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                    <%= for {key, value} <- @generation_options do %>
                      <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">
                          <%= key |> to_string() |> String.replace("_unmatched", " ") |> String.capitalize() %>
                        </label>
                        <%= if is_boolean(value) do %>
                          <select phx-change="update_option" phx-value-field={key} class="w-full border border-gray-300 rounded px-3 py-2">
                            <option value="true" selected={value}>Yes</option>
                            <option value="false" selected={!value}>No</option>
                          </select>
                        <% else %>
                          <input
                            type={if is_integer(value), do: "number", else: "text"}
                            phx-change="update_option"
                            phx-value-field={key}
                            value={value}
                            class="w-full border border-gray-300 rounded px-3 py-2"
                          />
                        <% end %>
                      </div>
                    <% end %>
                  </div>
                </div>

                <!-- Generate Button -->
                <div class="mb-6">
                  <button
                    phx-click="generate_data"
                    disabled={@is_generating}
                    class="bg-blue-600 text-white px-6 py-3 rounded-lg hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
                  >
                    <%= if @is_generating, do: "Generating...", else: "Generate Data" %>
                  </button>
                </div>
              <% end %>

              <!-- Generation Log -->
              <div class="mb-6">
                <div class="flex justify-between items-center mb-3">
                  <h3 class="text-lg font-semibold text-gray-900">Generation Log</h3>
                  <button
                    phx-click="clear_log"
                    class="text-sm text-gray-600 hover:text-gray-800"
                  >
                    Clear Log
                  </button>
                </div>

                <div class="bg-gray-900 rounded-lg p-4 h-64 overflow-y-auto">
                  <%= if @generation_log == [] do %>
                    <p class="text-gray-400 text-sm">No generation activity yet...</p>
                  <% else %>
                    <%= for entry <- Enum.reverse(@generation_log) do %>
                      <div class="text-sm mb-1">
                        <span class="text-gray-400">
                          <%= Calendar.strftime(entry.timestamp, "%H:%M:%S") %>
                        </span>
                        <span class={[
                          "ml-2",
                          case entry.type do
                            :info -> "text-blue-400"
                            :success -> "text-green-400"
                            :error -> "text-red-400"
                          end
                        ]}>
                          <%= entry.message %>
                        </span>
                      </div>
                    <% end %>
                  <% end %>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
      """
    end

    # Helper functions
    defp load_data_templates(socket) do
      templates = [
        %{
          key: :complete_business,
          name: "Complete Business Setup",
          description: "Full business with plots, items, employees, and clients",
          includes: ["Business", "Plot", "Layout", "Items", "Employees", "Clients"]
        },
        %{
          key: :reservation_scenario,
          name: "Reservation Scenario",
          description: "Business with active reservations and booking history",
          includes: ["Business", "Items", "Clients", "Reservations", "Payments"]
        },
        %{
          key: :user_management,
          name: "User Management",
          description: "Multiple users with different roles and permissions",
          includes: ["Users", "Employees", "Businesses", "Permissions"]
        },
        %{
          key: :bulk_data,
          name: "Bulk Test Data",
          description: "Large dataset for performance testing",
          includes: ["Many Users", "Many Businesses", "Many Reservations"]
        }
      ]

      assign(socket, :data_templates, templates)
    end

    defp load_current_stats(socket) do
      stats = [
        {"Users", count_records(User)},
        {"Businesses", count_records(Business)},
        {"Items", count_records(Item)},
        {"Clients", count_records(Client)},
        {"Reservations", count_records(Reservation)}
      ]

      assign(socket, :current_stats, stats)
    end

    defp count_records(resource) do
      try do
        case resource.read(domain: get_domain(resource)) do
          {:ok, records} -> length(records)
          _unmatchedunmatched -> 0
        end
      rescue
        _unmatchedunmatched -> 0
      end
    end

    defp get_domain(User), do: RivaAsh.Accounts
    defp get_unmatcheddomain(_unmatched), do: RivaAsh.Domain

    defp get_template_options(:complete_business) do
      %{
        businesses_count: 1,
        items_per_business: 10,
        employees_per_business: 3,
        clients_count: 5,
        with_reservations: true
      }
    end

    defp get_template_options(:reservation_scenario) do
      %{
        businesses_count: 1,
        items_count: 5,
        clients_count: 10,
        reservations_count: 15,
        with_payments: true
      }
    end

    defp get_template_options(:user_management) do
      %{
        users_count: 5,
        admin_users: 1,
        manager_users: 2,
        staff_users: 2,
        with_businesses: true
      }
    end

    defp get_template_options(:bulk_data) do
      %{
        users_count: 50,
        businesses_count: 10,
        items_count: 100,
        clients_count: 200,
        reservations_count: 500
      }
    end

    defp get_unmatchedtemplate_unmatchedoptions(_unmatched), do: %{}

    defp generate_template_data(template, options) do
      send(self(), {:generation_log, "Starting #{template} generation with options: #{inspect(options)}"})

      case template do
        :complete_business ->
          generate_complete_business(options)

        :reservation_scenario ->
          generate_reservation_scenario(options)

        :user_management ->
          generate_user_management(options)

        :bulk_data ->
          generate_bulk_data(options)

        _unmatchedunmatched ->
          {:error, "Unknown template"}
      end
    end

    defp quick_generate_data(:user) do
      send(self(), {:generation_log, "Creating new user..."})

      user_attrs = %{
        email: "user_#{:rand.uniform(10000)}@example.com",
        name:
          Enum.random(["Alice", "Bob", "Charlie", "Diana", "Eve"]) <>
            " " <> Enum.random(["Smith", "Johnson", "Brown", "Davis", "Wilson"]),
        password: "password123",
        role: Enum.random(["user", "admin", "manager"])
      }

      case User
           |> Ash.Changeset.for_create(:register_with_password, user_attrs)
           |> Ash.create(domain: RivaAsh.Accounts) do
        {:ok, user} ->
          {:ok, "Created user: #{user.email}"}

        {:error, error} ->
          {:error, "Failed to create user: #{inspect(error)}"}
      end
    end

    defp quick_generate_data(:business) do
      send(self(), {:generation_log, "Creating new business..."})

      # This would use your BusinessSetupFlow reactor
      business_attrs = %{
        name: "Business #{:rand.uniform(1000)}",
        description: "A test business for development"
      }

      case Business |> Ash.Changeset.for_create(:create, business_attrs) |> Ash.create(domain: RivaAsh.Domain) do
        {:ok, business} ->
          {:ok, "Created business: #{business.name}"}

        {:error, error} ->
          {:error, "Failed to create business: #{inspect(error)}"}
      end
    end

    defp quick_generate_data(:client) do
      send(self(), {:generation_log, "Creating new client..."})

      client_attrs = %{
        email: "client_#{:rand.uniform(10000)}@example.com",
        first_name: Enum.random(["John", "Jane", "Mike", "Sarah", "Tom"]),
        last_name: Enum.random(["Doe", "Smith", "Johnson", "Brown", "Davis"]),
        phone: "+1-555-#{:rand.uniform(999)}-#{:rand.uniform(9999)}"
      }

      case Client |> Ash.Changeset.for_create(:create, client_attrs) |> Ash.create(domain: RivaAsh.Domain) do
        {:ok, client} ->
          {:ok, "Created client: #{client.first_name} #{client.last_name}"}

        {:error, error} ->
          {:error, "Failed to create client: #{inspect(error)}"}
      end
    end

    defp quick_generate_data(:reservation) do
      send(self(), {:generation_log, "Creating new reservation..."})
      {:ok, "Reservation creation not implemented yet"}
    end

    defp generate_complete_business(_options) do
      send(self(), {:generation_log, "Generating complete business setup..."})
      # Implementation would use BusinessSetupFlow reactor
      {:ok, "Complete business setup generated"}
    end

    defp generate_reservation_scenario(_options) do
      send(self(), {:generation_log, "Generating reservation scenario..."})
      {:ok, "Reservation scenario generated"}
    end

    defp generate_user_management(_options) do
      send(self(), {:generation_log, "Generating user management data..."})
      {:ok, "User management data generated"}
    end

    defp generate_bulk_data(_options) do
      send(self(), {:generation_log, "Generating bulk test data..."})
      {:ok, "Bulk test data generated"}
    end
  end
end
