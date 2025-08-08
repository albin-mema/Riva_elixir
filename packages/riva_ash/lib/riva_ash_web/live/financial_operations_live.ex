alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Live, as: Live
alias Ash.Error, as: Error

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
  @layout {RivaAshWeb.Layouts, :authenticated}

  alias RivaAsh.Financial
  alias RivaAsh.ErrorHelpers

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

      {:error, _unmatched} = error ->
        {:ok, error}
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
          <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
        </div>
      <% else %>
        <!-- Financial Dashboard -->
        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          <!-- Revenue Card -->
          <.card>
            <:body>
            <div class="p-6">
              <div class="flex items-center">
                <div class="flex-shrink-0">
                  <div class="w-8 h-8 bg-green-500 rounded-md flex items-center justify-center">
                    <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                    </svg>
                  </div>
                </div>
                <div class="ml-5 w-0 flex-1">
                  <dl>
                    <dt class="text-sm font-medium text-gray-500 truncate">Total Revenue</dt>
                    <dd class="text-lg font-medium text-gray-900">
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
                  <div class="w-8 h-8 bg-blue-500 rounded-md flex items-center justify-center">
                    <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
                    </svg>
                  </div>
                </div>
                <div class="ml-5 w-0 flex-1">
                  <dl>
                    <dt class="text-sm font-medium text-gray-500 truncate">This Month</dt>
                    <dd class="text-lg font-medium text-gray-900">
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
                  <div class="w-8 h-8 bg-yellow-500 rounded-md flex items-center justify-center">
                    <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
                    </svg>
                  </div>
                </div>
                <div class="ml-5 w-0 flex-1">
                  <dl>
                    <dt class="text-sm font-medium text-gray-500 truncate">Pending</dt>
                    <dd class="text-lg font-medium text-gray-900">
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
                  <div class="w-8 h-8 bg-purple-500 rounded-md flex items-center justify-center">
                    <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z" />
                    </svg>
                  </div>
                </div>
                <div class="ml-5 w-0 flex-1">
                  <dl>
                    <dt class="text-sm font-medium text-gray-500 truncate">Pricing Rules</dt>
                    <dd class="text-lg font-medium text-gray-900"><%= length(@pricings) %></dd>
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
            <div class="flex space-x-1 bg-gray-100 rounded-lg p-1">
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
    <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
      <!-- Revenue Chart -->
      <.card>
        <:body>
        <div class="p-6">
          <h3 class="text-lg font-medium text-gray-900 mb-4">Revenue Trend</h3>
          <div class="bg-gray-50 p-6 rounded-lg h-64 flex items-center justify-center">
            <div class="text-center">
              <svg class="mx-auto h-12 w-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
              </svg>
              <p class="mt-2 text-gray-500">Revenue Chart</p>
              <p class="text-sm text-gray-400">Interactive chart will be implemented here</p>
            </div>
          </div>
        </div>
        </:body>
      </.card>

      <!-- Recent Transactions -->
      <.card>
        <:body>
        <div class="p-6">
          <h3 class="text-lg font-medium text-gray-900 mb-4">Recent Transactions</h3>
          <div class="space-y-3">
            <%= if length(@payments) == 0 do %>
              <p class="text-center text-gray-500 py-8">No transactions yet</p>
            <% else %>
              <%= for payment <- Enum.take(@payments, 5) do %>
                <div class="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                  <div>
                    <p class="text-sm font-medium text-gray-900">
                      <%= payment.client.first_name %> <%= payment.client.last_name %>
                    </p>
                    <p class="text-sm text-gray-500">
                      <%= Calendar.strftime(payment.inserted_at, "%b %d, %Y") %>
                    </p>
                  </div>
                  <div class="text-right">
                    <p class="text-sm font-medium text-green-600">
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
          <h3 class="text-lg font-medium text-gray-900">Payment Management</h3>
          <.button phx-click="record_payment" variant="primary">
            + Record Payment
          </.button>
        </div>

        <div class="overflow-x-auto">
          <table class="min-w-full divide-y divide-gray-200">
            <thead class="bg-gray-50">
              <tr>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Client</th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Amount</th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Date</th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Status</th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Actions</th>
              </tr>
            </thead>
            <tbody class="bg-white divide-y divide-gray-200">
              <%= if length(@payments) == 0 do %>
                <tr>
                  <td colspan="5" class="px-6 py-4 text-center text-gray-500">
                    No payments found
                  </td>
                </tr>
              <% else %>
                <%= for payment <- @payments do %>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="text-sm font-medium text-gray-900">
                        <%= payment.client.first_name %> <%= payment.client.last_name %>
                      </div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="text-sm text-gray-900">
                        <%= format_currency(payment.amount) %>
                      </div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="text-sm text-gray-500">
                        <%= Calendar.strftime(payment.inserted_at, "%b %d, %Y") %>
                      </div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <.badge variant={status_variant(payment.status)}>
                        <%= String.capitalize(to_string(payment.status)) %>
                      </.badge>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-medium">
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
          <h3 class="text-lg font-medium text-gray-900">Pricing Rules</h3>
          <.button phx-click="create_pricing" variant="primary">
            + Create Pricing Rule
          </.button>
        </div>

        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          <%= if length(@pricings) == 0 do %>
            <div class="col-span-full text-center text-gray-500 py-8">
              No pricing rules found
            </div>
          <% else %>
            <%= for pricing <- @pricings do %>
              <.card>
                <:body>
                <div class="p-4">
                  <h4 class="font-medium text-gray-900 mb-2"><%= pricing.name %></h4>
                  <p class="text-sm text-gray-600 mb-3"><%= pricing.description %></p>
                  <div class="flex justify-between items-center">
                    <span class="text-lg font-semibold text-green-600">
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
        <h3 class="text-lg font-medium text-gray-900 mb-4">Financial Reports</h3>

        <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
          <!-- Revenue Report -->
          <div class="bg-gray-50 p-6 rounded-lg">
            <h4 class="font-medium text-gray-900 mb-2">Revenue Report</h4>
            <p class="text-sm text-gray-600 mb-4">Detailed revenue analysis and trends</p>
            <.button phx-click="generate_report" phx-value-type="revenue" variant="outline">
              Generate Report
            </.button>
          </div>

          <!-- Tax Report -->
          <div class="bg-gray-50 p-6 rounded-lg">
            <h4 class="font-medium text-gray-900 mb-2">Tax Report</h4>
            <p class="text-sm text-gray-600 mb-4">Tax calculations and summaries</p>
            <.button phx-click="generate_report" phx-value-type="tax" variant="outline">
              Generate Report
            </.button>
          </div>
        </div>

        <!-- Export Options -->
        <div class="mt-6 pt-6 border-t border-gray-200">
          <h4 class="font-medium text-gray-900 mb-4">Export Options</h4>
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

  defp format_unmatchedcurrency(_unmatched), do: "$0.00"

  @doc """
  Determines the badge variant based on status.
  """
  defp status_variant(:active), do: "default"
  defp status_variant(:pending), do: "secondary"
  defp status_variant(:completed), do: "default"
  defp status_variant(:cancelled), do: "destructive"
  defp status_unmatchedvariant(_unmatched), do: "secondary"
end
