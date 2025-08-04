defmodule RivaAshWeb.Components.Molecules.Pagination do
  @moduledoc """
  Pagination component for table navigation.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon
  alias RivaAshWeb.Components.UI.Select, as: UISelect
  alias RivaAshWeb.Components.UI.Text, as: UIText

  @doc """
  Renders pagination controls.
  """
  attr(:meta, :map, required: true)
  attr(:path, :string, required: true)
  attr(:show_page_size, :boolean, default: true)
  attr(:page_sizes, :list, default: [10, 20, 50, 100])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def pagination(assigns) do
    assigns = assign(assigns, :current_page, assigns.meta.current_page || 1)
    assigns = assign(assigns, :total_pages, assigns.meta.total_pages || 1)
    assigns = assign(assigns, :total_count, assigns.meta.total_count || 0)
    assigns = assign(assigns, :page_size, assigns.meta.page_size || 20)

    ~H"""
    <div class={["flex items-center justify-between", @class]} {@rest}>
      <div class="flex items-center gap-2">
        <UIText.text variant="small" color="muted">
          Showing <%= (@current_page - 1) * @page_size + 1 %> to <%= min(@current_page * @page_size, @total_count) %> of <%= @total_count %> results
        </UIText.text>
      </div>

      <div class="flex items-center gap-2">
        <%= if @show_page_size do %>
          <div class="flex items-center gap-2">
            <UIText.text variant="small">Show</UIText.text>
            <UISelect.select
              options={Enum.map(@page_sizes, &{to_string(&1), &1})}
              value={@page_size}
              size="sm"
              phx-change="change-page-size"
            />
            <UIText.text variant="small">per page</UIText.text>
          </div>
        <% end %>

        <div class="flex items-center gap-1">
          <UIButton.button
            variant="outline"
            size="sm"
            disabled={@current_page <= 1}
            phx-click="goto-page"
            phx-value-page={@current_page - 1}
          >
            <UIIcon.icon name={:chevron_left} size="sm" />
            Previous
          </UIButton.button>

          <%= for page <- page_range(@current_page, @total_pages) do %>
            <%= if page == :ellipsis do %>
              <span class="px-2 py-1 text-sm text-muted-foreground">…</span>
            <% else %>
              <UIButton.button
                variant={if page == @current_page, do: "default", else: "outline"}
                size="sm"
                phx-click="goto-page"
                phx-value-page={page}
              >
                <%= page %>
              </UIButton.button>
            <% end %>
          <% end %>

          <UIButton.button
            variant="outline"
            size="sm"
            disabled={@current_page >= @total_pages}
            phx-click="goto-page"
            phx-value-page={@current_page + 1}
          >
            Next
            <UIIcon.icon name={:chevron_right} size="sm" />
          </UIButton.button>
        </div>
      </div>
    </div>
    """
  end

  defp page_range(current, total) when total <= 7 do
    1..total |> Enum.to_list()
  end

  defp page_range(current, total) do
    cond do
      current <= 4 ->
        [1, 2, 3, 4, 5, :ellipsis, total]

      current >= total - 3 ->
        [1, :ellipsis, total - 4, total - 3, total - 2, total - 1, total]

      true ->
        [1, :ellipsis, current - 1, current, current + 1, :ellipsis, total]
    end
  end
end
