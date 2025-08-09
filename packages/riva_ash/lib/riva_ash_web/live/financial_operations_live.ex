
defmodule RivaAshWeb.FinancialOperationsLive do
  @moduledoc """
  Financial Operations - Unified financial interface.
  Combines Payments, Pricing, and financial reporting into a single interface.

  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Uses AuthHelpers for authentication and business scoping
  - Delegates business logic to Financial context
  - Handles UI state and user interactions
  - Uses proper Ash error handling
  - Implements CRUD operations through Ash actions
  """

  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout

  alias RivaAsh.Financial

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Badge
  import RivaAshWeb.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case mount_business_scoped(
           socket,
           session,
           nil,
           [],
           "Financial Operations"
         ) do
      {:ok, socket} ->
        {:ok, assign(socket, loading: true)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def handle_params(params, _url, socket) do
    # Load financial data through business logic
    case Financial.load_financial_data(socket.assigns.current_user, params) do
      {:ok, data} ->
        {:noreply,
         socket
         |> assign(:payments, data.payments)
         |> assign(:pricings, data.pricings)
         |> assign(:stats, data.stats)
         |> assign(:loading, false)}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to view financial data")
         |> push_navigate(to: ~p"/access-denied")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load financial data: #{reason}")
         |> assign(loading: false)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <!-- Page Header with Quick Actions -->
      <.page_header title="💰 Financial Operations" description="Revenue tracking, payments, and pricing management">
        <:action>
          <.button phx-click="record_payment" variant="primary" class="mr-2">
            + Record Payment
          </.button>
          <.button phx-click="create_pricing" variant="secondary" class="mr-2">
            + Pricing Rule
          </.button>
          <.button phx-click="export_report" variant="secondary">
            Export Report
          </.button>
        </:action>
      </.page_header>

      <!-- Loading State -->
      <%= if @loading do %>
        <div class="flex justify-center items-center py-12">
          <div class="border-b-2 border-blue-600 rounded-full w-8 h-8 animate-spin"></div>
        </div>
      <% else %>
        <!-- Financial Dashboard -->
        <div class="gap-6 grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4">
          <!-- Revenue Card -->
          <.card>
            <:body>
            <div class="p-6">
              <div class="flex items-center">
                <div class="flex-shrink-0">
                  <div class="flex justify-center items-center bg-green-500 rounded-md w-8 h-8">
                    <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                    </svg>
                  </div>
                </div>
                <div class="flex-1 ml-5 w-0">
                  <dl>
                    <dt class="font-medium text-gray-500 text-sm truncate">Total Revenue</dt>
                    <dd class="font-medium text-gray-900 text-lg">
                      <%= format_currency(@stats.total_revenue) %>
                    </dd>
                  </dl>
                </div>
              </div>
            </div>
            </:body>
          </.card>

          <!-- Monthly Revenue -->
          <.card>
            <:body>
            <div class="p-6">
              <div class="flex items-center">
                <div class="flex-shrink-0">
                  <div class="flex justify-center items-center bg-blue-500 rounded-md w-8 h-8">
                    <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
                    </svg>
                  </div>
                </div>
                <div class="flex-1 ml-5 w-0">
                  <dl>
                    <dt class="font-medium text-gray-500 text-sm truncate">This Month</dt>
                    <dd class="font-medium text-gray-900 text-lg">
                      <%= format_currency(@stats.monthly_revenue) %>
                    </dd>
                  </dl>
                </div>
              </div>
            </div>
            </:body>
          </.card>

          <!-- Pending Payments -->
          <.card>
            <:body>
            <div class="p-6">
              <div class="flex items-center">
                <div class="flex-shrink-0">
                  <div class="flex justify-center items-center bg-yellow-500 rounded-md w-8 h-8">
                    <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
                    </svg>
                  </div>
                </div>
                <div class="flex-1 ml-5 w-0">
                  <dl>
                    <dt class="font-medium text-gray-500 text-sm truncate">Pending</dt>
                    <dd class="font-medium text-gray-900 text-lg">
                      <%= format_currency(@stats.pending_payments) %>
                    </dd>
                  </dl>
                </div>
              </div>
            </div>
            </:body>
          </.card>

          <!-- Active Pricing Rules -->
          <.card>
            <:body>
            <div class="p-6">
              <div class="flex items-center">
                <div class="flex-shrink-0">
                  <div class="flex justify-center items-center bg-purple-500 rounded-md w-8 h-8">
                    <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z" />
                    </svg>
                  </div>
                </div>
                <div class="flex-1 ml-5 w-0">
                  <dl>
                    <dt class="font-medium text-gray-500 text-sm truncate">Pricing Rules</dt>
                    <dd class="font-medium text-gray-900 text-lg"><%= length(@pricings) %></dd>
                  </dl>
                </div>
              </div>
            </div>
            </:body>
          </.card>
        </div>

        <!-- View Tabs -->
        <div class="bg-white shadow rounded-lg">
          <div class="px-6 py-4">
            <div class="flex space-x-1 bg-gray-100 p-1 rounded-lg">
              <button
                phx-click="change_view"
                phx-value-mode="dashboard"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "dashboard", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Dashboard
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="payments"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "payments", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Payments
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="pricing"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "pricing", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Pricing
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="reports"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "reports", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Reports
              </button>
            </div>
          </div>
        </div>

        <!-- Main Content -->
        <%= case @view_mode do %>
          <% "dashboard" -> %>
            <%= render_dashboard_view(assigns) %>
          <% "payments" -> %>
            <%= render_payments_view(assigns) %>
          <% "pricing" -> %>
            <%= render_pricing_view(assigns) %>
          <% "reports" -> %>
            <%= render_reports_view(assigns) %>
        <% end %>
      <% end %>
    </div>
    """
  end

  # View rendering functions
  defp render_dashboard_view(assigns) do
    ~H"""
    <div class="gap-6 grid grid-cols-1 lg:grid-cols-2">
      <!-- Revenue Chart -->
      <.card>
        <:body>
        <div class="p-6">
          <h3 class="mb-4 font-medium text-gray-900 text-lg">Revenue Trend</h3>
          <div class="flex justify-center items-center bg-gray-50 p-6 rounded-lg h-64">
            <div class="text-center">
              <svg class="mx-auto w-12 h-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
              </svg>
              <p class="mt-2 text-gray-500">Revenue Chart</p>
              <p class="text-gray-400 text-sm">Interactive chart will be implemented here</p>
            </div>
          </div>
        </div>
        </:body>
      </.card>

      <!-- Recent Transactions -->
      <.card>
        <:body>
        <div class="p-6">
          <h3 class="mb-4 font-medium text-gray-900 text-lg">Recent Transactions</h3>
          <div class="space-y-3">
            <%= if length(@payments) == 0 do %>
              <p class="py-8 text-gray-500 text-center">No transactions yet</p>
            <% else %>
              <%= for payment <- Enum.take(@payments, 5) do %>
                <div class="flex justify-between items-center bg-gray-50 p-3 rounded-lg">
                  <div>
                    <p class="font-medium text-gray-900 text-sm">
                      <%= payment.client.first_name %> <%= payment.client.last_name %>
                    </p>
                    <p class="text-gray-500 text-sm">
                      <%= Calendar.strftime(payment.inserted_at, "%b %d, %Y") %>
                    </p>
                  </div>
                  <div class="text-right">
                    <p class="font-medium text-green-600 text-sm">
                      <%= format_currency(payment.amount) %>
                    </p>
                    <.badge variant={status_variant(payment.status)}>
                      <%= String.capitalize(to_string(payment.status)) %>
                    </.badge>
                  </div>
                </div>
              <% end %>
            <% end %>
          </div>
        </div>
        </:body>
      </.card>
    </div>
    """
  end

  defp render_payments_view(assigns) do
    ~H"""
    <.card>
      <:body>
      <div class="p-6">
        <div class="flex justify-between items-center mb-4">
          <h3 class="font-medium text-gray-900 text-lg">Payment Management</h3>
          <.button phx-click="record_payment" variant="primary">
            + Record Payment
          </.button>
        </div>

        <div class="overflow-x-auto">
          <table class="divide-y divide-gray-200 min-w-full">
            <thead class="bg-gray-50">
              <tr>
                <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Client</th>
                <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Amount</th>
                <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Date</th>
                <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Status</th>
                <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Actions</th>
              </tr>
            </thead>
            <tbody class="bg-white divide-y divide-gray-200">
              <%= if length(@payments) == 0 do %>
                <tr>
                  <td colspan="5" class="px-6 py-4 text-gray-500 text-center">
                    No payments found
                  </td>
                </tr>
              <% else %>
                <%= for payment <- @payments do %>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="font-medium text-gray-900 text-sm">
                        <%= payment.client.first_name %> <%= payment.client.last_name %>
                      </div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="text-gray-900 text-sm">
                        <%= format_currency(payment.amount) %>
                      </div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="text-gray-500 text-sm">
                        <%= Calendar.strftime(payment.inserted_at, "%b %d, %Y") %>
                      </div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <.badge variant={status_variant(payment.status)}>
                        <%= String.capitalize(to_string(payment.status)) %>
                      </.badge>
                    </td>
                    <td class="px-6 py-4 font-medium text-sm whitespace-nowrap">
                      <.button phx-click="view_payment" phx-value-id={payment.id} variant="outline" size="sm">
                        View
                      </.button>
                    </td>
                  </tr>
                <% end %>
              <% end %>
            </tbody>
          </table>
        </div>
      </div>
      </:body>
    </.card>
    """
  end

  defp render_pricing_view(assigns) do
    ~H"""
    <.card>
      <:body>
      <div class="p-6">
        <div class="flex justify-between items-center mb-4">
          <h3 class="font-medium text-gray-900 text-lg">Pricing Rules</h3>
          <.button phx-click="create_pricing" variant="primary">
            + Create Pricing Rule
          </.button>
        </div>

        <div class="gap-4 grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3">
          <%= if length(@pricings) == 0 do %>
            <div class="col-span-full py-8 text-gray-500 text-center">
              No pricing rules found
            </div>
          <% else %>
            <%= for pricing <- @pricings do %>
              <.card>
                <:body>
                <div class="p-4">
                  <h4 class="mb-2 font-medium text-gray-900"><%= pricing.name %></h4>
                  <p class="mb-3 text-gray-600 text-sm"><%= pricing.description %></p>
                  <div class="flex justify-between items-center">
                    <span class="font-semibold text-green-600 text-lg">
                      <%= format_currency(pricing.amount) %>
                    </span>
                    <.badge variant={status_variant(pricing.status)}>
                      <%= String.capitalize(to_string(pricing.status)) %>
                    </.badge>
                  </div>
                </div>
                </:body>
              </.card>
            <% end %>
          <% end %>
        </div>
      </div>
      </:body>
    </.card>
    """
  end

  defp render_reports_view(assigns) do
    ~H"""
    <.card>
      <:body>
      <div class="p-6">
        <h3 class="mb-4 font-medium text-gray-900 text-lg">Financial Reports</h3>

        <div class="gap-6 grid grid-cols-1 md:grid-cols-2">
          <!-- Revenue Report -->
          <div class="bg-gray-50 p-6 rounded-lg">
            <h4 class="mb-2 font-medium text-gray-900">Revenue Report</h4>
            <p class="mb-4 text-gray-600 text-sm">Detailed revenue analysis and trends</p>
            <.button phx-click="generate_report" phx-value-type="revenue" variant="outline">
              Generate Report
            </.button>
          </div>

          <!-- Tax Report -->
          <div class="bg-gray-50 p-6 rounded-lg">
            <h4 class="mb-2 font-medium text-gray-900">Tax Report</h4>
            <p class="mb-4 text-gray-600 text-sm">Tax calculations and summaries</p>
            <.button phx-click="generate_report" phx-value-type="tax" variant="outline">
              Generate Report
            </.button>
          </div>
        </div>

        <!-- Export Options -->
        <div class="mt-6 pt-6 border-gray-200 border-t">
          <h4 class="mb-4 font-medium text-gray-900">Export Options</h4>
          <div class="flex flex-wrap gap-3">
            <.button phx-click="export_report" phx-value-format="pdf" variant="secondary">
              Export as PDF
            </.button>
            <.button phx-click="export_report" phx-value-format="csv" variant="secondary">
              Export as CSV
            </.button>
            <.button phx-click="export_report" phx-value-format="excel" variant="secondary">
              Export as Excel
            </.button>
          </div>
        </div>
      </div>
      </:body>
    </.card>
    """
  end

  @impl true
  def handle_event("change_view", %{"mode" => mode}, socket) do
    {:noreply, assign(socket, :view_mode, mode)}
  end

  def handle_event("record_payment", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/payments/new")}
  end

  def handle_event("create_pricing", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/pricing/new")}
  end

  def handle_event("view_payment", %{"id" => id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/payments/#{id}")}
  end

  def handle_event("generate_report", %{"type" => type}, socket) do
    case Financial.generate_report(socket.assigns.current_user, type) do
      {:ok, report_url} ->
        {:noreply,
         socket
         |> put_flash(:info, "Report generated successfully")
         |> push_navigate(to: report_url)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to generate report: #{reason}")}
    end
  end

  def handle_event("export_report", %{"format" => format}, socket) do
    case Financial.export_report(socket.assigns.current_user, format) do
      {:ok, download_url} ->
        {:noreply,
         socket
         |> put_flash(:info, "Report exported as #{format} successfully")
         |> push_navigate(to: download_url)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to export report: #{reason}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions

  @doc """
  Formats currency values for display.
  """
  defp format_currency(amount) when is_number(amount) do
    "$#{:erlang.float_to_binary(amount, decimals: 2)}"
  end

  defp format_currency(%Decimal{} = decimal) do
    "$#{Decimal.to_string(decimal, :normal)}"
  end


  @doc """
  Determines the badge variant based on status.
  """
  defp status_variant(:active), do: "default"
  defp status_variant(:pending), do: "secondary"
  defp status_variant(:completed), do: "default"
  defp status_variant(:cancelled), do: "destructive"
end
