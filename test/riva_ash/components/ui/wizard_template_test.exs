defmodule RivaAshWeb.WizardTemplateTest do
  use ExUnit.Case, async: true
  import Phoenix.LiveViewTest
  import RivaAshWeb.WizardTemplate

  describe "ARIA attributes" do
    test "main wizard container has proper dialog role and labels" do
      assigns = %{
        title: "Account Setup",
        description: "Complete all steps to finish your account setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 1,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end
      }

      html = render_component(&wizard_template/1, assigns)

      assert html =~ ~r/role="dialog"/
      assert html =~ ~r/aria-labelledby="wizard-title"/
      assert html =~ ~r/aria-describedby="wizard-description"/
      assert html =~ ~r/id="wizard-title"/
      assert html =~ ~r/id="wizard-description"/
    end

    test "stepper has proper tablist semantics" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 1,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end
      }

      html = render_component(&wizard_template/1, assigns)

      assert html =~ ~r/role="tablist"/
      assert html =~ ~r/aria-orientation="horizontal"/
      assert html =~ ~r/aria-label="Wizard navigation"/
    end

    test "active step has aria-current attribute" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 2,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end
      }

      html = render_component(&wizard_template/1, assigns)

      assert html =~ ~r/aria-current="step"/
      assert html =~ ~r/data-active="true"/
    end
  end

  describe "step navigation" do
    test "back button is disabled on first step" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 1,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end
      }

      html = render_component(&wizard_template/1, assigns)
      assert html =~ ~r/disabled/
      assert html =~ ~r/Previous step/
    end

    test "next button shows correct label on final step" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 2,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end
      }

      html = render_component(&wizard_template/1, assigns)
      assert html =~ ~r/Complete/
      assert html =~ ~r/Complete wizard/
    end

    test "validation prevents progression" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 1,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end,
        step_validations: %{1 => false}
      }

      html = render_component(&wizard_template/1, assigns)
      assert html =~ ~r/disabled/
      assert html =~ ~r/Next step/
    end
  end

  describe "responsive layout" do
    test "mobile stepper has vertical orientation classes" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 1,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end
      }

      html = render_component(&wizard_template/1, assigns)
      assert html =~ ~r/class="md:hidden mb-6"/
      assert html =~ ~r/orientation="vertical"/
      assert html =~ ~r/density="lg"/
    end

    test "desktop stepper has horizontal orientation classes" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 1,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end
      }

      html = render_component(&wizard_template/1, assigns)
      assert html =~ ~r/class="hidden md:block mb-6"/
      assert html =~ ~r/orientation="horizontal"/
      assert html =~ ~r/density="md"/
    end
  end

  describe "loading state" do
    test "spinner is visible when loading_step is true" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 1,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end,
        loading_step: true
      }

      html = render_component(&wizard_template/1, assigns)
      assert html =~ ~r/class="z-10 absolute inset-0"/
      assert html =~ ~r/spinner/
    end
  end

  describe "touch target requirements" do
    test "mobile stepper has adequate touch targets" do
      assigns = %{
        title: "Account Setup",
        steps: [%{label: "Account"}, %{label: "Profile"}],
        current_step: 1,
        on_step_change: fn _ -> end,
        on_back: fn -> end,
        on_next: fn -> end
      }

      html = render_component(&wizard_template/1, assigns)
      assert html =~ ~r/class="step-indicator.*w-\[44px\] h-\[44px\]"/
    end
  end
end
