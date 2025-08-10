defmodule WizardTemplateStories do
  use ExStory.Story

  import RivaAshWeb.WizardTemplate
  import RivaAshWeb.Components.Organisms, only: [page_header: 1]

  def desktop_layout do
    ~H"""
    <.wizard_template
      title="Account Setup"
      description="Complete all steps to finish your account setup"
      steps={[
        %{label: "Account", icon: "user"},
        %{label: "Profile", icon: "profile"},
        %{label: "Preferences", icon: "settings"}
      ]}
      current_step={1}
      on_step_change={fn _ -> end}
      on_back={fn -> end}
      on_next={fn -> end}
      step_validations={%{1 => true, 2 => false, 3 => true}}
    >
      <div class="p-4">
        <h3 class="mb-2 font-medium text-lg">Account Information</h3>
        <p>Enter your account details to get started.</p>
      </div>
    </.wizard_template>
    """
  end

  def mobile_layout do
    ~H"""
    <.wizard_template
      title="Account Setup"
      description="Complete all steps to finish your account setup"
      steps={[
        %{label: "Account", icon: "user"},
        %{label: "Profile", icon: "profile"},
        %{label: "Preferences", icon: "settings"}
      ]}
      current_step={2}
      on_step_change={fn _ -> end}
      on_back={fn -> end}
      on_next={fn -> end}
      step_validations={%{1 => true, 2 => false, 3 => true}}
    >
      <div class="p-4">
        <h3 class="mb-2 font-medium text-lg">Profile Information</h3>
        <p>Set up your profile details.</p>
      </div>
    </.wizard_template>
    """
  end

  def validation_flow do
    ~H"""
    <.wizard_template
      title="Validation Flow"
      description="Demonstrates step validation"
      steps={[
        %{label: "Account", icon: "user"},
        %{label: "Profile", icon: "profile"},
        %{label: "Preferences", icon: "settings"}
      ]}
      current_step={2}
      on_step_change={fn _ -> end}
      on_back={fn -> end}
      on_next={fn -> end}
      step_validations={%{1 => true, 2 => false, 3 => true}}
    >
      <div class="p-4">
        <h3 class="mb-2 font-medium text-lg">Profile Information</h3>
        <p class="mb-2 text-red-500">Please complete all required fields</p>
        <div class="space-y-4">
          <div>
            <label class="block font-medium text-gray-700 text-sm">Name</label>
            <input type="text" class="block shadow-sm mt-1 p-2 border border-gray-300 rounded-md w-full" />
          </div>
        </div>
      </div>
    </.wizard_template>
    """
  end

  def loading_state do
    ~H"""
    <.wizard_template
      title="Loading State"
      description="Demonstrates loading state during step transition"
      steps={[
        %{label: "Account", icon: "user"},
        %{label: "Profile", icon: "profile"},
        %{label: "Preferences", icon: "settings"}
      ]}
      current_step={2}
      on_step_change={fn _ -> end}
      on_back={fn -> end}
      on_next={fn -> end}
      step_validations={%{1 => true, 2 => true, 3 => true}}
      loading_step={true}
    >
      <div class="p-4">
        <h3 class="mb-2 font-medium text-lg">Profile Information</h3>
        <p>Processing your information...</p>
      </div>
    </.wizard_template>
    """
  end

  def with_page_header do
    ~H"""
    <.page_header
      title="Wizard Setup"
      description="Multi-step configuration process"
      actions={
        [
          %{label: "Cancel", variant: "secondary", on_click: fn -> end},
          %{label: "Save Draft", variant: "secondary", on_click: fn -> end}
        ]
      }
    />
    <.wizard_template
      title="Account Setup"
      description="Complete all steps to finish your account setup"
      steps={[
        %{label: "Account", icon: "user"},
        %{label: "Profile", icon: "profile"},
        %{label: "Preferences", icon: "settings"}
      ]}
      current_step={1}
      on_step_change={fn _ -> end}
      on_back={fn -> end}
      on_next={fn -> end}
      step_validations={%{1 => true, 2 => false, 3 => true}}
    >
      <div class="p-4">
        <h3 class="mb-2 font-medium text-lg">Account Information</h3>
        <p>Enter your account details to get started.</p>
      </div>
    </.wizard_template>
    """
  end
end
