defmodule RivaAshWeb.Components.Ui.StepperTest do
  use RivaAshWeb.ConnCase
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import RivaAshWeb.Components.Ui.Stepper

  describe "stepper component" do
    test "renders steps correctly" do
      steps = [%{label: "Step 1"}, %{label: "Step 2"}]
      current_step = 1

      html = render_component(fn ->
        ~H"""
        <Stepper steps={steps} current_step={current_step}>
          <div>Content</div>
        </Stepper>
        """
      end)

      assert html =~ ~r/data-orientation="horizontal"/
      assert html =~ ~r/step-container.*data-active="true"/
      assert html =~ ~r/step-label.*Step 1/
      assert html =~ ~r/step-label.*Step 2/
    end

    test "handles vertical orientation" do
      steps = [%{label: "Step 1"}, %{label: "Step 2"}]
      current_step = 1

      html = render_component(fn ->
        ~H"""
        <Stepper steps={steps} current_step={current_step} orientation="vertical">
          <div>Content</div>
        </Stepper>
        """
      end)

      assert html =~ ~r/data-orientation="vertical"/
      assert html =~ ~r/flex-col/
    end

    test "navigates between steps" do
      steps = [%{label: "Step 1"}, %{label: "Step 2"}]

      html = render_component(fn ->
        ~H"""
        <Stepper
          steps={steps}
          current_step={1}
          on_next={JS.push("next_step")}
          on_prev={JS.push("prev_step")}
        >
          <div>Step 1 content</div>
        </Stepper>
        """
      end)

      assert html =~ ~r/phx-click="next_step"/
      assert html =~ ~r/phx-click="prev_step"/
    end

    test "applies density variants" do
      steps = [%{label: "Step 1"}, %{label: "Step 2"}]

      html = render_component(fn ->
        ~H"""
        <Stepper steps={steps} current_step={1} density="sm">
          <div>Content</div>
        </Stepper>
        """
      end)

      assert html =~ ~r/step-indicator.*size-classes.*:button, :sm)/
    end

    test "shows validation states" do
      steps = [%{label: "Valid", valid: true}, %{label: "Invalid", valid: false}]

      html = render_component(fn ->
        ~H"""
        <Stepper
          steps={steps}
          current_step={2}
          step_validations={%{1 => true, 2 => false}}
        >
          <div>Step 2 content</div>
        </Stepper>
        """
      end)

      assert html =~ ~r/step-container.*data-valid="true"/
      assert html =~ ~r/step-container.*data-valid="false"/
    end

    test "keyboard navigation works" do
      steps = [%{label: "Step 1"}, %{label: "Step 2"}]

      html = render_component(fn ->
        ~H"""
        <Stepper
          steps={steps}
          current_step={1}
          on_keydown={JS.push("handle_keydown")}
        >
          <div>Step 1 content</div>
        </Stepper>
        """
      end)

      assert html =~ ~r/phx-keydown="handle_keydown"/
    end
  end
end
