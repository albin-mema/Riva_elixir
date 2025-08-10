defmodule RivaAshWeb.WizardTemplate do
  use Phoenix.Component

  import RivaAshWeb.Components.Atoms
  import RivaAshWeb.Components.Molecules

  def wizard_template(assigns) do
    ~H"""
    <div
      role="dialog"
      aria-labelledby="wizard-title"
      class="bg-card shadow-md mx-auto rounded-lg w-full max-w-4xl overflow-hidden"
    >
      <div class="p-6 border-b border-border">
        <h2 id="wizard-title" class="font-semibold text-foreground text-xl">
          <%= @title %>
        </h2>
        <p class="mt-1 text-gray-500 text-sm" id="wizard-description">
          <%= @description %>
        </p>
      </div>

      <div class="p-6">
        <div class="md:hidden mb-6">
          <.stepper
            steps={@steps}
            current_step={@current_step}
            orientation="vertical"
            density="lg"
            on_step_change={@on_step_change}
            on_step_click={@on_step_click}
            on_prev={@on_back}
            on_next={@on_next}
            step_validations={@step_validations}
            aria-describedby="wizard-description"
          />
        </div>
        <div class="hidden md:block mb-6">
          <.stepper
            steps={@steps}
            current_step={@current_step}
            orientation="horizontal"
            density="md"
            on_step_change={@on_step_change}
            on_step_click={@on_step_click}
            on_prev={@on_back}
            on_next={@on_next}
            step_validations={@step_validations}
            aria-describedby="wizard-description"
          />
        </div>

        <div class="mb-8">
          <div class="relative min-h-[200px]">
            <%= if @loading_step do %>
              <div class="z-10 absolute inset-0 flex justify-center items-center bg-white bg-opacity-75">
                <.spinner size="lg" />
              </div>
            <% end %>
            <%= render_slot(@content) %>
          </div>
        </div>

        <div class="flex justify-between">
          <.button
            label="Back"
            variant="secondary"
            disabled={@current_step <= 1}
            on_click={@on_back}
            aria-label="Previous step"
          />

          <.button
            label={@current_step == length(@steps) ? "Complete" : "Next"}
            on_click={@on_next}
            aria-label={@current_step == length(@steps) ? "Complete wizard" : "Next step"}
            variant={@current_step == length(@steps) ? "primary" : "secondary"}
          />
        </div>
      </div>
    </div>
    """
  end
end
