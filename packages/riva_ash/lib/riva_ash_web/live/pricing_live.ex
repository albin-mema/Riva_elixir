alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Live, as: Live
alias RivaAsh.Resources, as: Resources
alias RivaAsh.Pricing, as: Pricing
alias Ash.Error, as: Error

defmodule RivaAshWeb.PricingLive do
  @moduledoc """
  LiveView for managing Pricing plans.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.Pricing
  alias RivaAsh.Pricing.PricingService

  @impl true
  def mount(_params, session, socket) do
    mount_business_scoped(socket, session, Pricing, :business_id, get_page_title())
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Pricing" description="Define and manage pricing rules and plans">
        <:action>
          <.button phx-click="new_pricing" class="bg-blue-600 hover:bg-blue-700">New Pricing</.button>
        </:action>
      </.page_header>

      <.data_table
        id="pricings-table"
        items={@pricings}
        meta={@meta}
        path="/pricings"
      >
        <:col :let={pricing} label="Name" field={:name} sortable>
          <%= pricing.name %>
        </:col>
        <:col :let={pricing} label="Amount">
          <%= format_currency(pricing.amount) %>
        </:col>
        <:col :let={pricing} label="Duration">
          <%= pricing.duration %>
        </:col>
        <:col :let={pricing} label="Status">
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case pricing.status do
              :active -> "bg-green-100 text-green-800"
              :inactive -> "bg-gray-100 text-gray-800"
              _unmatchedunmatched -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(pricing.status)) %>
          </span>
        </:col>
        <:col :let={pricing} label="Actions">
          <.button phx-click="edit_pricing" phx-value-id={pricing.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_pricing" phx-value-id={pricing.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_pricing", _params, socket) do
    {:noreply, push_patch(socket, to: "/pricings/new")}
  end

  def handle_event("edit_pricing", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/pricings/#{id}/edit")}
  end

  def handle_event("delete_pricing", %{"id" => id}, socket) do
    case PricingService.delete_pricing(id, socket.assigns.current_user) do
      {:ok, _pricing} ->
        {:noreply,
         socket
         |> put_flash(:info, "Pricing deleted successfully")
         |> reload_pricing_data()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete pricing: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions
  defp reload_pricing_data(socket) do
    case PricingService.get_user_pricings(socket.assigns.current_user) do
      {:ok, {pricings, meta}} ->
        socket
        |> assign(:pricings, pricings)
        |> assign(:meta, meta)

      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Pricing"
  end

  defp format_currency(amount) when is_number(amount) do
    case Application.get_env(:riva_ash, :currency, "USD") do
      "USD" -> "$#{:erlang.float_to_binary(amount, decimals: 2)}"
      "EUR" -> "€#{:erlang.float_to_binary(amount, decimals: 2)}"
      "GBP" -> "£#{:erlang.float_to_binary(amount, decimals: 2)}"
      currency -> "#{currency}#{:erlang.float_to_binary(amount, decimals: 2)}"
    end
  end

  defp format_currency(amount), do: amount

  defp format_error(reason) do
    case RivaAsh.ErrorHelpers.format_error(reason) do
      %{message: message} -> message
      message when is_binary(message) -> message
      _ -> "An unexpected error occurred"
    end
  end

  defp format_validation_error(error) do
    case error do
      {message, _unmatched} -> message
      message when is_binary(message) -> message
      _unmatchedunmatched -> "Invalid input"
    end
  end
end
