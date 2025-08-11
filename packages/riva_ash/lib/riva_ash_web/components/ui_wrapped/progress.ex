defmodule RivaAshWeb.Components.UIWrapped.Progress do
  @moduledoc """
  App-level Progress wrapper around SaladUI.Progress.
  
  Provides progress indicators for loading states and completion tracking.
  """
  use Phoenix.Component

  @doc """
  Renders a progress bar with value and label.
  """
  attr :value, :integer,
    required: true,
    doc: "Progress value between 0 and 100"

  attr :max, :integer,
    default: 100,
    doc: "Maximum value for the progress bar"

  attr :size, :string,
    default: "default",
    values: ~w(default sm),
    doc: "Progress bar size"

  attr :class, :string, default: ""
  attr :rest, :global

  slot :label, doc: "Optional label to display with the progress"

  def progress(assigns) do
    assigns =
      assigns
      |> assign_new(:percentage, fn ->
        if assigns.max > 0, do: round((assigns.value / assigns.max) * 100), else: 0
      end)

    ~H"""
    <div class="w-full">
      <SaladUI.Progress.progress
        value={@value}
        max={@max}
        class={[
          case @size do
            "sm" -> "h-2"
            _ -> "h-4"
          end,
          @class
        ]}
        {@rest}
      />
      
      <%= if @percentage >= 0 and @percentage <= 100 do %>
        <div class="mt-2 flex justify-between text-sm text-muted-foreground">
          <%= render_slot(@label) %>
          <span><%= @percentage %>%</span>
        </div>
      <% end %>
    </div>
    """
  end
end