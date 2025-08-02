defmodule RivaAshWeb.FinancialOperationsLive do
  @moduledoc """
  Financial Operations - Unified financial interface.
  Combines Payments, Pricing, and financial reporting into a single interface.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout

  alias RivaAsh.Resources.{Business, Payment, Pricing}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        try do
          # Load user's businesses
          businesses = Business.read!(actor: user)
          business_ids = Enum.map(businesses, & &1.id)

          # Load financial data
          payments =
            Payment.read!(
              actor: user,
              filter: [business_id: [in: business_ids]]
            )

          pricings =
            Pricing.read!(
              actor: user,
              filter: [business_id: [in: business_ids]]
            )

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, "Financial Operations")
            |> assign(:businesses, businesses)
            |> assign(:payments, payments)
            |> assign(:pricings, pricings)
            |> assign(:view_mode, "dashboard")
            |> assign(:selected_period, "month")
            |> assign(:loading, false)

          {:ok, socket}
        rescue
          _error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
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
                  <dd class="text-lg font-medium text-gray-900">$<%= calculate_total_revenue(@payments) %></dd>
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
                  <dd class="text-lg font-medium text-gray-900">$<%= calculate_monthly_revenue(@payments) %></dd>
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
                  <dd class="text-lg font-medium text-gray-900">$<%= calculate_pending_payments(@payments) %></dd>
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
                    <p class="text-sm font-medium text-gray-900">Payment #<%= payment.id %></p>
                    <p class="text-sm text-gray-500">
                      <%= Calendar.strftime(payment.inserted_at, "%b %d, %Y") %>
                    </p>
                  </div>
                  <div class="text-right">
                    <p class="text-sm font-medium text-green-600">
                      $<%= format_amount(payment.amount) %>
                    </p>
                    <p class="text-xs text-gray-500">
                      <%= String.capitalize(to_string(payment.status || :completed)) %>
                    </p>
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
        <h3 class="text-lg font-medium text-gray-900 mb-4">Payment Management</h3>
        <div class="bg-gray-50 p-6 rounded-lg">
          <p class="text-center text-gray-500">Payment processing interface will be implemented here</p>
          <p class="text-center text-sm text-gray-400 mt-2">Payment history, processing, and refunds</p>
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
        <h3 class="text-lg font-medium text-gray-900 mb-4">Pricing Rules</h3>
        <div class="bg-gray-50 p-6 rounded-lg">
          <p class="text-center text-gray-500">Pricing rule management interface will be implemented here</p>
          <p class="text-center text-sm text-gray-400 mt-2">Dynamic pricing, discounts, and rate management</p>
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
        <div class="bg-gray-50 p-6 rounded-lg">
          <p class="text-center text-gray-500">Financial reporting interface will be implemented here</p>
          <p class="text-center text-sm text-gray-400 mt-2">Revenue reports, analytics, and insights</p>
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

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp calculate_total_revenue(payments) do
    payments
    |> Enum.map(&(&1.amount || 0))
    |> Enum.sum()
    |> format_amount()
  end

  defp calculate_monthly_revenue(payments) do
    current_month = Date.utc_today() |> Date.beginning_of_month()

    payments
    |> Enum.filter(fn payment ->
      payment_date = DateTime.to_date(payment.inserted_at)
      Date.compare(payment_date, current_month) != :lt
    end)
    |> Enum.map(&(&1.amount || 0))
    |> Enum.sum()
    |> format_amount()
  end

  defp calculate_pending_payments(_payments) do
    # Placeholder - would filter by status
    "0.00"
  end

  defp format_amount(amount) when is_number(amount),
    do: :erlang.float_to_binary(amount / 1.0, decimals: 2)

  defp format_amount(%Decimal{} = amount), do: Decimal.to_string(amount)
  defp format_amount(_), do: "0.00"
end
