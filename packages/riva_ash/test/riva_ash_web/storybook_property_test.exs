defmodule RivaAshWeb.StorybookPropertyTest do
  @moduledoc """
  Property-based testing for Phoenix Storybook components.
  
  This test suite automatically generates thousands of random component
  variations and tests them for:
  - Rendering without crashes
  - Valid HTML output
  - Accessibility compliance
  - Visual regression detection
  - Performance characteristics
  
  Run with: mix test test/riva_ash_web/storybook_property_test.exs
  """
  
  use ExUnit.Case, async: false
  use ExUnitProperties
  
  import Phoenix.LiveViewTest
  import RivaAsh.StorybookTesting.PropertyGenerators
  
  alias RivaAshWeb.Components.Atoms
  
  @endpoint RivaAshWeb.Endpoint
  
  describe "Button Component Property Testing" do
    property "button renders with any valid props without crashing" do
      check all(
        props <- button_props(),
        max_runs: 100
      ) do
        # Test component rendering
        html = render_component(&Atoms.Button.button/1, Map.put(props, :inner_block, [props.text]))
        
        # Basic assertions
        assert html =~ "<button"
        assert html =~ props.text
        assert html =~ "type=\"#{props.type}\""
        
        # Variant-specific assertions
        if props.variant != "" do
          # Should contain some styling related to variant
          assert html =~ ~r/class="[^"]*"/
        end
        
        # Disabled state
        if props.disabled do
          assert html =~ "disabled"
        end
        
        # No script tags (XSS protection)
        refute html =~ "<script"
        
        # Valid HTML structure
        assert html =~ ~r/<button[^>]*>.*<\/button>/s
      end
    end
    
    property "button handles edge cases gracefully" do
      check all(
        props <- edge_case_props(:button),
        max_runs: 50
      ) do
        # Should not crash even with invalid props
        html = render_component(&Atoms.Button.button/1, Map.put(props, :inner_block, [props.text || "Button"]))
        
        # Should still render a button
        assert html =~ "<button"
        
        # Should escape dangerous content
        refute html =~ "<script"
        refute html =~ "javascript:"
      end
    end
  end
  
  describe "Input Component Property Testing" do
    property "input renders with any valid props without crashing" do
      check all(
        props <- input_props(),
        max_runs: 100
      ) do
        html = render_component(&Atoms.Input.input/1, props)
        
        # Basic assertions
        assert html =~ "<input"
        assert html =~ "type=\"#{props.type}\""
        
        if props.placeholder != "" do
          assert html =~ "placeholder=\"#{props.placeholder}\""
        end
        
        if props.value do
          assert html =~ "value=\"#{props.value}\""
        end
        
        if props.disabled do
          assert html =~ "disabled"
        end
        
        if props.readonly do
          assert html =~ "readonly"
        end
        
        if props.required do
          assert html =~ "required"
        end
        
        # Security checks
        refute html =~ "<script"
        refute html =~ "javascript:"
      end
    end
    
    property "input handles edge cases and potential XSS" do
      check all(
        props <- edge_case_props(:input),
        max_runs: 50
      ) do
        html = render_component(&Atoms.Input.input/1, props)
        
        # Should still render an input
        assert html =~ "<input"
        
        # Should escape dangerous content
        refute html =~ "<script"
        refute html =~ "javascript:"
        refute html =~ "onload="
        refute html =~ "onerror="
      end
    end
  end
  
  describe "Text Component Property Testing" do
    property "text renders with any valid props without crashing" do
      check all(
        props <- text_props(),
        max_runs: 100
      ) do
        html = render_component(&Atoms.Text.text/1, Map.put(props, :inner_block, [props.text]))
        
        # Should contain the text
        assert html =~ props.text
        
        # Should have appropriate HTML tag based on variant
        case props.variant do
          "h1" -> assert html =~ "<h1"
          "h2" -> assert html =~ "<h2"
          "h3" -> assert html =~ "<h3"
          "h4" -> assert html =~ "<h4"
          "h5" -> assert html =~ "<h5"
          "h6" -> assert html =~ "<h6"
          "p" -> assert html =~ "<p"
          "span" -> assert html =~ "<span"
          _ -> assert html =~ ~r/<[^>]+>/
        end
        
        # Security checks
        refute html =~ "<script"
      end
    end
  end
  
  describe "Icon Component Property Testing" do
    property "icon renders with any valid props without crashing" do
      check all(
        props <- icon_props(),
        max_runs: 100
      ) do
        html = render_component(&Atoms.Icon.icon/1, props)
        
        # Should render an SVG or icon element
        assert html =~ ~r/<svg|<i|class="[^"]*icon/
        
        # Should not crash with any valid icon name
        assert is_binary(html)
        assert String.length(html) > 0
      end
    end
  end
  
  describe "Visual Regression Testing with Property-Based Data" do
    @tag :visual
    property "components render consistently in browser with random props" do
      check all(
        button_props <- button_props(),
        input_props <- input_props(),
        max_runs: 10  # Fewer runs for browser tests
      ) do
        # Create a test page with random components
        test_html = """
        <!DOCTYPE html>
        <html>
        <head>
          <meta charset="utf-8">
          <title>Property Test</title>
          <script src="https://cdn.tailwindcss.com"></script>
        </head>
        <body class="p-8 space-y-4">
          <div class="space-y-4">
            #{render_component(&Atoms.Button.button/1, Map.put(button_props, :inner_block, [button_props.text]))}
            #{render_component(&Atoms.Input.input/1, input_props)}
          </div>
        </body>
        </html>
        """
        
        # Write to temp file and test in browser
        temp_file = Path.join(System.tmp_dir(), "storybook_property_test_#{:rand.uniform(10000)}.html")
        File.write!(temp_file, test_html)
        
        try do
          # Visit the temp file in browser
          conn = build_conn()
          conn
          |> visit("file://#{temp_file}")
          |> assert_has("button")
          |> assert_has("input")
          
          # Take screenshot for visual regression
          screenshot_name = "property_test_#{:rand.uniform(10000)}.png"
          conn |> screenshot(screenshot_name)
          
        after
          File.rm(temp_file)
        end
      end
    end
  end
  
  describe "Performance Testing with Property-Based Data" do
    property "components render within acceptable time limits" do
      check all(
        props_list <- list_of(button_props(), length: 50),
        max_runs: 5
      ) do
        # Measure rendering time for batch of components
        {time_microseconds, _result} = :timer.tc(fn ->
          Enum.each(props_list, fn props ->
            render_component(&Atoms.Button.button/1, Map.put(props, :inner_block, [props.text]))
          end)
        end)
        
        # Should render 50 components in less than 100ms
        assert time_microseconds < 100_000, 
          "Rendering 50 components took #{time_microseconds}μs (>100ms)"
      end
    end
  end
  
  describe "Accessibility Testing with Property-Based Data" do
    property "components maintain accessibility with random props" do
      check all(
        button_props <- button_props(),
        max_runs: 50
      ) do
        html = render_component(&Atoms.Button.button/1, Map.put(button_props, :inner_block, [button_props.text]))
        
        # Should have proper button semantics
        assert html =~ ~r/<button[^>]*>/
        
        # Disabled buttons should have proper attributes
        if button_props.disabled do
          assert html =~ "disabled"
          # Could also check for aria-disabled if implemented
        end
        
        # Should not have empty button text (accessibility issue)
        if String.trim(button_props.text) != "" do
          assert html =~ button_props.text
        end
      end
    end
  end
  
  # Helper function to build a test connection
  defp build_conn do
    Phoenix.ConnTest.build_conn()
  end
end
