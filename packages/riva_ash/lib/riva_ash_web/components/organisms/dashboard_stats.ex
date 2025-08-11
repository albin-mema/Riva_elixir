defmodule RivaAshWeb.Components.Organisms.DashboardStats do
  @moduledoc """
  DashboardStats organism that combines multiple CardWithActions molecules.
  
  Provides a comprehensive dashboard statistics display with multiple stat cards.
  """
  use Phoenix.Component

  alias RivaAshWeb.Components.Molecules.CardWithActions
  alias RivaAshWeb.Components.UIWrapped.Text

  @doc """
  Renders a dashboard statistics section with multiple stat cards.
  """
  attr :stats, :list,
    required: true,
    doc: "List of stat cards with :title, :value, :change, :icon, and :variant keys"

  attr :class, :string, default: ""
  attr :rest, :global

  @spec dashboard_stats(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def dashboard_stats(assigns) do
    assigns
    |> build_dashboard_stats_attrs()
    |> validate_dashboard_stats_attrs()
    |> render_dashboard_stats()
  end

  @spec build_dashboard_stats_attrs(assigns :: map()) :: map()
  defp build_dashboard_stats_attrs(assigns), do: assigns

  @spec validate_dashboard_stats_attrs(assigns :: map()) :: map()
  defp validate_dashboard_stats_attrs(assigns) do
    with :ok <- validate_stats(assigns[:stats]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid dashboard stats attributes: #{reason}"
    end
  end

  @spec validate_stats(list(map())) :: :ok | {:error, String.t()}
  defp validate_stats(stats) when is_list(stats) do
    case Enum.all?(stats, &valid_stat?/1) do
      true -> :ok
      false -> {:error, "All stats must be maps with :title, :value, and :icon keys"}
    end
  end

  defp validate_unmatchedstats(_unmatched), do: {:error, "stats must be a list"}

  @spec valid_stat?(map()) :: boolean()
  defp valid_stat?(stat) do
    is_map(stat) and
      is_binary(stat[:title]) and
      (is_binary(stat[:value]) or is_integer(stat[:value])) and
      (is_atom(stat[:icon]) or is_nil(stat[:icon])) and
      (is_binary(stat[:change]) or is_nil(stat[:change])) and
      (is_binary(stat[:variant]) or is_nil(stat[:variant]))
  end

  @spec render_dashboard_stats(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_dashboard_stats(assigns) do
    ~H"""
    <div class={["dashboard-stats", @class]} {@rest}>
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
        <%= for stat <- @stats do %>
          <CardWithActions.card_with_actions
            title={stat.title}
            icon={stat.icon}
            variant={stat.variant || "default"}
            class="stat-card"
          >
            <div class="flex items-baseline space-x-2">
              <Text.text variant="lead" class="text-2xl font-bold">
                <%= stat.value %>
              </Text.text>
              <%= if stat.change do %>
                <Text.text variant="small" class={[
                  case String.starts_with?(stat.change, "+") do
                    true -> "text-green-600"
                    false -> "text-red-600"
                  end
                ]}>
                  <%= stat.change %>
                </Text.text>
              <% end %>
            </div>
          </CardWithActions.card_with_actions>
        <% end %>
      </div>
    </div>
    """
  end
end