defmodule RivaAshWeb.Components.Ui.Stepper do
  use Phoenix.Component

  import RivaAshWeb.Components.Atoms

  attr :steps, :list, required: true
  attr :current_step, :integer, required: true
  attr :orientation, :string, default: "horizontal", values: ["horizontal", "vertical"]
  attr :density, :string, default: "md", values: ["sm", "md", "lg"]
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div
      class="stepper #{@class}"
      data-orientation={@orientation}
      role="tablist"
      aria-orientation={@orientation}
      phx-keydown={@on_keydown}
      aria-label="Wizard navigation"
    >
      <div aria-live="polite" class="sr-only">
        Step <%= @current_step %> of <%= length(@steps) %>: <%= Enum.at(@steps, @current_step - 1).label %>
      </div>
      <div class="stepper-header">
        <%= for {step, index} <- Enum.with_index(@steps) do %>
          <div class="step-container" data-active={index + 1 == @current_step} data-completed={index + 1 < @current_step}>
            <button
              id={"step-#{index + 1}-tab"}
              class={["step-indicator", Atoms.variant_classes(:button, :primary), Atoms.size_classes(:button, :md), Atoms.focus_styles(), "transition-all #{motion_token(:smooth)}"]}
              type="button"
              role="tab"
              aria-selected={index + 1 == @current_step}
              aria-controls={"step-#{index + 1}-panel"}
              aria-disabled={step.disabled}
              tabindex={if index + 1 == @current_step, do: "0", else: "-1"}
              phx-click={JS.push("select_step", value: %{step: index + 1})}
            >
              <%= if step.icon do %>
                <span class="step-icon"><%= step.icon %></span>
              <% else %>
                <span class="step-number"><%= index + 1 %></span>
              <% end %>
            </button>
            <span class="step-label"><%= step.label %></span>
          </div>
        <% end %>
      </div>
      <div
        id={"step-#{@current_step}-panel"}
        class="stepper-content"
        role="tabpanel"
        aria-labelledby={"step-#{@current_step}-tab"}
        aria-hidden={@current_step != @active_step}
        phx-hook="FocusOnStepChange"
      >
        <%= render_slot(@inner_block) %>
      </div>
    </div>

    <div class="flex justify-between mt-4 stepper-navigation">
      <%= if @current_step > 1 do %>
        <button
          class={["stepper-prev", Atoms.variant_classes(:button, :secondary), Atoms.size_classes(:button, @density), Atoms.focus_styles()]}
          phx-click={@on_prev}
          aria-label="Previous step"
          disabled={@current_step == 1}
        >
          Previous
        </button>
      <% end %>

      <%= if @current_step < length(@steps) do %>
        <button
          class={["stepper-next", Atoms.variant_classes(:button, :primary), Atoms.size_classes(:button, @density), Atoms.focus_styles()]}
          phx-click={@on_next}
          aria-label="Next step"
          disabled={@step_validations && !@step_validations[@current_step]}
        >
          Next
        </button>
      <% end %>
    </div>
    """
  end

  @doc """
  Handles keyboard navigation for stepper component
  """
  def handle_event("keydown", %{"key" => "ArrowLeft"}, socket) do
    new_step = max(socket.assigns.current_step - 1, 1)
    {:noreply, assign(socket, current_step: new_step)}
  end

  def handle_event("keydown", %{"key" => "ArrowRight"}, socket) do
    new_step = min(socket.assigns.current_step + 1, length(socket.assigns.steps))
    {:noreply, assign(socket, current_step: new_step)}
100|   end
101|
102|   def handle_event("keydown", %{"key" => "Home"}, socket) do
103|     {:noreply, assign(socket, current_step: 1)}
104|   end
105|
106|   def handle_event("keydown", %{"key" => "End"}, socket) do
107|     {:noreply, assign(socket, current_step: length(socket.assigns.steps))}
108|   end
109|
110|   def handle_event("keydown", %{"key" => "Enter"}, socket) do
111|     # Activate current step (handled by existing click handler)
112|     {:noreply, socket}
113|   end
114| end
