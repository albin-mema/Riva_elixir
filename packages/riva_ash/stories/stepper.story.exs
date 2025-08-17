defmodule StepperStories do
  use RivaAshWeb.Storybook

  alias RivaAshWeb.Components.Ui.Stepper

  def basic_wizard do
    ~H"""
    <Stepper
      steps={[
        %{label: "Account", icon: "user"},
        %{label: "Profile", icon: "profile"},
        %{label: "Preferences", icon: "settings"}
      ]}
      current_step={1}
      on_next={JS.push("next_step")}
      on_prev={JS.push("prev_step")}
    >
      <div>Step 1 content</div>
    </Stepper>
    """
  end

  def validation_states do
    ~H"""
    <Stepper
      steps={[
        %{label: "Basic Info", valid: true},
        %{label: "Contact", valid: false},
        %{label: "Payment", valid: nil}
      ]}
      current_step={2}
      step_validations={%{1 => true, 2 => false, 3 => nil}}
      on_next={JS.push("next_step")}
      on_prev={JS.push("prev_step")}
    >
      <div>Step 2 content (invalid)</div>
    </Stepper>
    """
  end

  def keyboard_navigation do
    ~H"""
    <Stepper
      steps={[
        %{label: "Step 1"},
        %{label: "Step 2"},
        %{label: "Step 3"}
      ]}
      current_step={2}
      on_keydown={JS.push("handle_keydown")}
      on_next={JS.push("next_step")}
      on_prev={JS.push("prev_step")}
    >
      <div>Focusable step content for keyboard testing</div>
    </Stepper>
    """
  end

  def wizard_template_integration do
    ~H"""
    <div class="mx-auto max-w-2xl">
      <Stepper
        steps={[
          %{label: "Business Details"},
          %{label: "Location Setup"},
          %{label: "Staff Management"},
          %{label: "Payment Configuration"}
        ]}
        current_step={3}
        orientation="vertical"
        density="sm"
        on_next={JS.push("next_step")}
        on_prev={JS.push("prev_step")}
      >
        <div class="bg-card p-6 rounded-lg">
          <h3 class="mb-4 font-medium text-lg">Staff Management</h3>
          <p>Configure your team members and permissions</p>
        </div>
      </Stepper>
    </div>
    """
  end

  def mobile_behavior do
    ~H"""
    <div class="bg-background p-4 w-screen h-screen">
      <Stepper
        steps={[
          %{label: "Step 1"},
          %{label: "Step 2"},
          %{label: "Step 3"},
          %{label: "Step 4"},
          %{label: "Step 5"}
        ]}
        current_step={3}
        orientation="vertical"
        class="md:flex-row"
      >
        <div>Mobile-optimized step content</div>
      </Stepper>
    </div>
    """
  end
end
