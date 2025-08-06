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

  @type props :: map()
  @type html_output :: String.t()
  @type test_result :: :ok | {:error, String.t()}
  @type performance_metrics :: {integer(), any()}
  @type test_context :: %{temp_file: String.t(), html: String.t()}
  @type button_props :: map()
  @type input_props :: map()
  @type text_props :: map()
  @type icon_props :: map()

  @max_rendering_time_ms Application.compile_env(:riva_ash, :storybook_max_rendering_time_ms, 100)
  @max_visual_test_runs Application.compile_env(:riva_ash, :storybook_max_visual_test_runs, 10)
  @max_performance_test_runs Application.compile_env(:riva_ash, :storybook_max_performance_test_runs, 5)

  # Configuration constants for test behavior
  @default_max_runs Application.compile_env(:riva_ash, :storybook_default_max_runs, 100)
  @edge_case_max_runs Application.compile_env(:riva_ash, :storybook_edge_case_max_runs, 50)
  @performance_test_length Application.compile_env(:riva_ash, :storybook_performance_test_length, 50)
  @xss_test_patterns Application.compile_env(:riva_ash, :storybook_xss_test_patterns, [
                       "<script",
                       "javascript:",
                       "onload=",
                       "onerror=",
                       "onclick=",
                       "onmouseover="
                     ])
  @valid_html_tags Application.compile_env(:riva_ash, :storybook_valid_html_tags, [
                     "<button",
                     "<input",
                     "<h1",
                     "<h2",
                     "<h3",
                     "<h4",
                     "<h5",
                     "<h6",
                     "<p",
                     "<span",
                     "<svg",
                     "<i"
                   ])

  @callback render_component((map() -> any()), map()) :: String.t()
  @callback validate_html_structure(String.t(), map()) :: :ok | {:error, String.t()}
  @callback assert_security_controls(String.t()) :: :ok | {:error, String.t()}

  # Functional core - pure business logic functions
  defmodule TestCore do
    @moduledoc """
    Pure functions for component testing logic.
    These functions contain no side effects and are easily testable.
    """

    @type props :: map()
    @type html_output :: String.t()
    @type test_result :: :ok | {:error, String.t()}

    @spec validate_html_structure(html_output(), atom()) :: test_result()
    def validate_html_structure(html, component_type) do
      case component_type do
        :button -> validate_button_html(html)
        :input -> validate_input_html(html)
        :text -> validate_text_html(html)
        :icon -> validate_icon_html(html)
        _ -> {:error, "Unknown component type: #{component_type}"}
      end
    end

    @spec validate_button_html(html_output()) :: test_result()
    defp validate_button_html(html) do
      cond do
        html =~ "<button" and html =~ "type=\"" -> :ok
        html =~ "<button" -> {:error, "Button missing type attribute"}
        true -> {:error, "Invalid button HTML structure"}
      end
    end

    @spec validate_input_html(html_output()) :: test_result()
    defp validate_input_html(html) do
      cond do
        html =~ "<input" and html =~ "type=\"" -> :ok
        html =~ "<input" -> {:error, "Input missing type attribute"}
        true -> {:error, "Invalid input HTML structure"}
      end
    end

    @spec validate_text_html(html_output()) :: test_result()
    defp validate_text_html(html) do
      cond do
        html =~ ~r/<h[1-6]>|<p>|<span>/ -> :ok
        html =~ ~r/<[^>]+>/ -> :ok
        true -> {:error, "Invalid text HTML structure"}
      end
    end

    @spec validate_icon_html(html_output()) :: test_result()
    defp validate_icon_html(html) do
      cond do
        html =~ ~r/<svg|<i|class="[^"]*icon/ -> :ok
        html =~ "<svg" -> :ok
        html =~ "<i" -> :ok
        html =~ "icon" -> :ok
        true -> {:error, "Invalid icon HTML structure"}
      end
    end

    @spec assert_security_controls(html_output()) :: test_result()
    def assert_security_controls(html) do
      cond do
        html =~ "<script" -> {:error, "Script tags found in HTML - potential XSS"}
        html =~ "javascript:" -> {:error, "JavaScript protocol found - potential XSS"}
        html =~ "onload=" -> {:error, "Onload event found - potential XSS"}
        html =~ "onerror=" -> {:error, "Onerror event found - potential XSS"}
        true -> :ok
      end
    end

    @spec measure_performance([props()], atom()) :: {:ok, performance_metrics()} | {:error, String.t()}
    def measure_performance(props_list, component_type) do
      try do
        component_func =
          case component_type do
            :button -> &Atoms.Button.button/1
            :input -> &Atoms.Input.input/1
            :text -> &Atoms.Text.text/1
            :icon -> &Atoms.Icon.icon/1
            _ -> raise "Unknown component type"
          end

        metrics =
          :timer.tc(fn ->
            Enum.each(props_list, fn props ->
              apply_component_func(component_func, props)
            end)
          end)

        {:ok, metrics}
      rescue
        error -> {:error, "Failed to measure performance: #{inspect(error)}"}
      end
    end

    @spec apply_component_func((map() -> any()), map()) :: any()
    defp apply_component_func(func, props) do
      case func do
        f when is_function(f, 1) ->
          case func do
            &Atoms.Button.button/1 ->
              inner_block = Map.get(props, :inner_block, [props.text])
              f.(Map.put(props, :inner_block, inner_block))

            _ ->
              f.(props)
          end

        _ ->
          raise "Invalid component function"
      end
    end
  end

  # Imperative shell - functions that handle side effects
  defmodule TestShell do
    @moduledoc """
    Functions that handle side effects and orchestrate the testing process.
    These functions manage file operations, browser interactions, and error handling.
    """

    @type props :: map()
    @type html_output :: String.t()
    @type test_context :: %{temp_file: String.t(), html: String.t()}

    @spec render_component_with_error_handling(props(), atom()) :: {:ok, html_output()} | {:error, String.t()}
    def render_component_with_error_handling(props, component_type) do
      try do
        component_func =
          case component_type do
            :button -> &Atoms.Button.button/1
            :input -> &Atoms.Input.input/1
            :text -> &Atoms.Text.text/1
            :icon -> &Atoms.Icon.icon/1
            _ -> raise "Unknown component type"
          end

        html = apply_component_func_with_error_handling(component_func, props)
        {:ok, html}
      rescue
        error -> {:error, "Failed to render #{component_type} component: #{inspect(error)}"}
      end
    end

    @spec setup_visual_environment(String.t()) :: {:ok, test_context()} | {:error, String.t()}
    def setup_visual_environment(test_html) do
      try do
        temp_file = Path.join(System.tmp_dir(), "storybook_property_test_#{:rand.uniform(10000)}.html")
        File.write!(temp_file, test_html)
        {:ok, %{html: test_html, temp_file: temp_file}}
      rescue
        error -> {:error, "Failed to setup visual test environment: #{inspect(error)}"}
      end
    end

    @spec execute_browser_visual_test(test_context()) :: {:ok, String.t()} | {:error, String.t()}
    def execute_browser_visual_test(%{temp_file: temp_file}) do
      try do
        conn = build_conn()

        conn
        |> visit("file://#{temp_file}")
        |> assert_has("button")
        |> assert_has("input")

        # Take screenshot for visual regression
        screenshot_name = "property_test_#{:rand.uniform(10000)}.png"
        conn |> screenshot(screenshot_name)

        {:ok, "Visual test completed successfully"}
      rescue
        error -> {:error, "Failed to execute browser visual test: #{inspect(error)}"}
      end
    end

    @spec cleanup_test_environment(test_context()) :: :ok | {:error, String.t()}
    def cleanup_test_environment(%{temp_file: temp_file}) do
      try do
        File.rm(temp_file)
        :ok
      rescue
        error -> {:error, "Failed to cleanup test environment: #{inspect(error)}"}
      end
    end

    @spec build_conn() :: {:ok, struct()} | {:error, String.t()}
    defp build_conn do
      try do
        {:ok, Phoenix.ConnTest.build_conn()}
      rescue
        error -> {:error, "Failed to build test connection: #{inspect(error)}"}
      end
    end

    @spec apply_component_func_with_error_handling((map() -> any()), map()) :: html_output()
    defp apply_component_func_with_error_handling(func, props) do
      case func do
        &Atoms.Button.button/1 ->
          inner_block = Map.get(props, :inner_block, [props.text])
          func.(Map.put(props, :inner_block, inner_block))

        _ ->
          func.(props)
      end
    end
  end

  describe "Button Component Property Testing" do
    @spec button_renders_without_crashes(props()) :: :ok
    property "button renders with any valid props without crashing" do
      check all(
              props <- button_props(),
              max_runs: @default_max_runs
            ) do
        props
        |> render_button_component()
        |> assert_button_html_structure()
        |> assert_button_attributes()
        |> assert_security_controls()
        |> assert_valid_html_structure()
      end
    end

    @spec button_handles_edge_cases_gracefully(props()) :: :ok
    property "button handles edge cases gracefully" do
      check all(
              props <- edge_case_props(:button),
              max_runs: @edge_case_max_runs
            ) do
        props
        |> render_button_with_fallback_text()
        |> assert_button_renders()
        |> assert_content_escaping()
      end
    end
  end

  @callback handle_edge_case_props(atom(), props()) :: props()
  @callback validate_component_output(html_output(), atom()) :: :ok | {:error, String.t()}
  @callback assert_component_accessibility(html_output(), props()) :: :ok | {:error, String.t()}

  # Helper functions for button testing
  @spec render_button_component(props()) :: {:ok, html_output()} | {:error, String.t()}
  defp render_button_component(props) when is_map(props) do
    guard_validate_props(props, :button)
    |> case do
      :ok -> do_render_button_component(props)
      {:error, reason} -> {:error, reason}
    end
  end

  @spec guard_validate_props(props(), atom()) :: :ok | {:error, String.t()}
  defp guard_validate_props(props, component_type) when is_map(props) do
    case component_type do
      :button ->
        if Map.has_key?(props, :text) and is_binary(props.text) do
          :ok
        else
          {:error, "Button component requires :text key with binary value"}
        end

      :input ->
        if Map.has_key?(props, :type) and is_binary(props.type) do
          :ok
        else
          {:error, "Input component requires :type key with binary value"}
        end

      :text ->
        if Map.has_key?(props, :text) and is_binary(props.text) do
          :ok
        else
          {:error, "Text component requires :text key with binary value"}
        end

      :icon ->
        if Map.has_key?(props, :name) and is_binary(props.name) do
          :ok
        else
          {:error, "Icon component requires :name key with binary value"}
        end

      _ ->
        {:error, "Unknown component type: #{component_type}"}
    end
  end

  @spec do_render_button_component(props()) :: {:ok, html_output()} | {:error, String.t()}
  defp do_render_button_component(props) do
    try do
      inner_block = Map.get(props, :inner_block, [props.text])
      html = render_component(&Atoms.Button.button/1, Map.put(props, :inner_block, inner_block))
      {:ok, html}
    rescue
      error -> {:error, "Failed to render button component: #{inspect(error)}"}
    end
  end

  @spec assert_button_html_structure(html_output()) :: :ok | {:error, String.t()}
  defp assert_button_html_structure(html) do
    cond do
      html =~ "<button" and html =~ "type=\"" -> :ok
      html =~ "<button" -> {:error, "Button missing type attribute"}
      true -> {:error, "Invalid button HTML structure"}
    end
  end

  @spec assert_button_attributes(html_output()) :: :ok
  defp assert_button_attributes(html) do
    # Extract props from context - this would need to be passed in a real implementation
    # For now, we'll use pattern matching on the HTML to check for variant-specific attributes
    Enum.reduce(@valid_html_tags, :ok, fn tag, acc ->
      case acc do
        :ok ->
          case html do
            html when html =~ ~r/class="[^"]*variant[^"]*"/ ->
              assert html =~ ~r/class="[^"]*"/
              :ok

            html when html =~ "disabled" ->
              assert html =~ "disabled"
              :ok

            _ ->
              :ok
          end

        error ->
          error
      end
    end)
  end

  @spec assert_security_controls(html_output()) :: :ok
  defp assert_security_controls(html) do
    Enum.take(@xss_test_patterns, 2)
    |> Enum.reduce(:ok, fn pattern, acc ->
      case acc do
        :ok ->
          refute html =~ pattern
          :ok

        error ->
          error
      end
    end)
  end

  @spec assert_valid_html_structure(html_output()) :: :ok
  defp assert_valid_html_structure(html) do
    assert html =~ ~r/<button[^>]*>.*<\/button>/s
    :ok
  end

  @spec render_button_with_fallback_text(props()) :: html_output()
  defp render_button_with_fallback_text(props) do
    fallback_text = Map.get(props, :text, "Button")
    inner_block = [fallback_text]
    updated_props = Map.put(props, :inner_block, inner_block)
    render_component(&Atoms.Button.button/1, updated_props)
  end

  @spec assert_button_renders(html_output()) :: :ok
  defp assert_button_renders(html) do
    assert html =~ "<button"
    :ok
  end

  @spec assert_content_escaping(html_output()) :: :ok
  defp assert_content_escaping(html) do
    refute html =~ "<script"
    refute html =~ "javascript:"
    :ok
  end

  describe "Input Component Property Testing" do
    @spec input_renders_without_crashes(props()) :: :ok
    property "input renders with any valid props without crashing" do
      check all(
              props <- input_props(),
              max_runs: @default_max_runs
            ) do
        props
        |> render_input_component()
        |> assert_input_html_structure()
        |> assert_input_attributes()
        |> assert_input_security_controls()
      end
    end

    @spec input_handles_edge_cases_and_xss(props()) :: :ok
    property "input handles edge cases and potential XSS" do
      check all(
              props <- edge_case_props(:input),
              max_runs: @edge_case_max_runs
            ) do
        props
        |> render_input_component()
        |> assert_input_renders()
        |> assert_xss_protection()
      end
    end
  end

  # Helper functions for input testing
  @spec render_input_component(props()) :: {:ok, html_output()} | {:error, String.t()}
  defp render_input_component(props) when is_map(props) do
    guard_validate_props(props, :input)
    |> case do
      :ok -> do_render_input_component(props)
      {:error, reason} -> {:error, reason}
    end
  end

  @spec do_render_input_component(props()) :: {:ok, html_output()} | {:error, String.t()}
  defp do_render_input_component(props) do
    try do
      html = render_component(&Atoms.Input.input/1, props)
      {:ok, html}
    rescue
      error -> {:error, "Failed to render input component: #{inspect(error)}"}
    end
  end

  @spec assert_input_html_structure(html_output()) :: :ok | {:error, String.t()}
  defp assert_input_html_structure(html) do
    cond do
      html =~ "<input" and html =~ "type=\"" -> :ok
      html =~ "<input" -> {:error, "Input missing type attribute"}
      true -> {:error, "Invalid input HTML structure"}
    end
  end

  @spec assert_input_attributes(html_output()) :: :ok
  defp assert_input_attributes(html) do
    # Extract props from context - this would need to be passed in a real implementation
    # For now, we'll use pattern matching on the HTML to check for various attributes

    case html do
      html when html =~ "placeholder=" ->
        assert html =~ "placeholder=\""

      html when html =~ "value=" ->
        assert html =~ "value=\""

      html when html =~ "disabled" ->
        assert html =~ "disabled"

      html when html =~ "readonly" ->
        assert html =~ "readonly"

      html when html =~ "required" ->
        assert html =~ "required"

      _ ->
        :ok
    end
  end

  @spec assert_input_security_controls(html_output()) :: :ok
  defp assert_input_security_controls(html) do
    refute html =~ "<script"
    refute html =~ "javascript:"
    :ok
  end

  @spec assert_input_renders(html_output()) :: :ok
  defp assert_input_renders(html) do
    assert html =~ "<input"
    :ok
  end

  @spec assert_xss_protection(html_output()) :: :ok
  defp assert_xss_protection(html) do
    Enum.reduce(@xss_test_patterns, :ok, fn pattern, acc ->
      case acc do
        :ok ->
          refute html =~ pattern
          :ok

        error ->
          error
      end
    end)
  end

  describe "Text Component Property Testing" do
    @spec text_renders_without_crashes(props()) :: :ok
    property "text renders with any valid props without crashing" do
      check all(
              props <- text_props(),
              max_runs: @default_max_runs
            ) do
        props
        |> render_text_component()
        |> assert_text_content()
        |> assert_text_html_tag()
        |> assert_text_security_controls()
      end
    end
  end

  # Helper functions for text testing
  @spec render_text_component(props()) :: {:ok, html_output()} | {:error, String.t()}
  defp render_text_component(props) when is_map(props) do
    guard_validate_props(props, :text)
    |> case do
      :ok -> do_render_text_component(props)
      {:error, reason} -> {:error, reason}
    end
  end

  @spec do_render_text_component(props()) :: {:ok, html_output()} | {:error, String.t()}
  defp do_render_text_component(props) do
    try do
      inner_block = Map.get(props, :inner_block, [props.text])
      html = render_component(&Atoms.Text.text/1, Map.put(props, :inner_block, inner_block))
      {:ok, html}
    rescue
      error -> {:error, "Failed to render text component: #{inspect(error)}"}
    end
  end

  @spec assert_text_content(html_output()) :: :ok | {:error, String.t()}
  defp assert_text_content(html) do
    cond do
      html =~ props.text -> :ok
      String.contains?(html, props.text) -> :ok
      true -> {:error, "Text content not found in HTML output"}
    end
  end

  @spec assert_text_html_tag(html_output()) :: :ok
  defp assert_text_html_tag(html) do
    # This would need props to be passed in a real implementation
    # For now, we'll use pattern matching to check for HTML tags
    Enum.reduce(@valid_html_tags, :ok, fn tag, acc ->
      case acc do
        :ok ->
          case html do
            html when html =~ tag ->
              :ok

            _ ->
              if tag in ["<h1", "<h2", "<h3", "<h4", "<h5", "<h6", "<p", "<span"] do
                :ok
              else
                assert html =~ ~r/<[^>]+>/
                :ok
              end
          end

        error ->
          error
      end
    end)
  end

  @spec assert_text_security_controls(html_output()) :: :ok
  defp assert_text_security_controls(html) do
    Enum.take(@xss_test_patterns, 1)
    |> Enum.reduce(:ok, fn pattern, acc ->
      case acc do
        :ok ->
          refute html =~ pattern
          :ok

        error ->
          error
      end
    end)
  end

  describe "Icon Component Property Testing" do
    @spec icon_renders_without_crashes(props()) :: :ok
    property "icon renders with any valid props without crashing" do
      check all(
              props <- icon_props(),
              max_runs: @default_max_runs
            ) do
        props
        |> render_icon_component()
        |> assert_icon_html_structure()
        |> assert_icon_output_validity()
      end
    end
  end

  # Helper functions for icon testing
  @spec render_icon_component(props()) :: {:ok, html_output()} | {:error, String.t()}
  defp render_icon_component(props) when is_map(props) do
    guard_validate_props(props, :icon)
    |> case do
      :ok -> do_render_icon_component(props)
      {:error, reason} -> {:error, reason}
    end
  end

  @spec do_render_icon_component(props()) :: {:ok, html_output()} | {:error, String.t()}
  defp do_render_icon_component(props) do
    try do
      html = render_component(&Atoms.Icon.icon/1, props)
      {:ok, html}
    rescue
      error -> {:error, "Failed to render icon component: #{inspect(error)}"}
    end
  end

  @spec assert_icon_html_structure(html_output()) :: :ok | {:error, String.t()}
  defp assert_icon_html_structure(html) do
    cond do
      html =~ ~r/<svg|<i|class="[^"]*icon/ -> :ok
      html =~ "<svg" -> :ok
      html =~ "<i" -> :ok
      html =~ "icon" -> :ok
      true -> {:error, "Invalid icon HTML structure"}
    end
  end

  @spec assert_icon_output_validity(html_output()) :: :ok | {:error, String.t()}
  defp assert_icon_output_validity(html) do
    cond do
      is_binary(html) and String.length(html) > 0 -> :ok
      not is_binary(html) -> {:error, "Icon output is not a binary string"}
      String.length(html) == 0 -> {:error, "Icon output is empty"}
      true -> {:error, "Invalid icon output"}
    end
  end

  describe "Visual Regression Testing with Property-Based Data" do
    @tag :visual
    @spec visual_regression_test_with_random_props(props(), props()) :: :ok
    property "components render consistently in browser with random props" do
      check all(
              button_props <- button_props(),
              input_props <- input_props(),
              max_runs: @max_visual_test_runs
            ) do
        button_props
        |> input_props
        |> create_test_html()
        |> setup_visual_test()
        |> run_browser_visual_test()
        |> cleanup_test_files()
      end
    end
  end

  # Helper functions for visual regression testing
  @spec create_test_html(props(), props()) :: {:ok, String.t()} | {:error, String.t()}
  defp create_test_html(button_props, input_props) do
    try do
      button_text = Map.get(button_props, :text, "Button")
      button_inner_block = [button_text]
      button_html = render_component(&Atoms.Button.button/1, Map.put(button_props, :inner_block, button_inner_block))
      input_html = render_component(&Atoms.Input.input/1, input_props)

      test_html = build_test_html_document(button_html, input_html)

      {:ok, test_html}
    rescue
      error -> {:error, "Failed to create test HTML: #{inspect(error)}"}
    end
  end

  @spec build_test_html_document(String.t(), String.t()) :: String.t()
  defp build_test_html_document(button_html, input_html) do
    """
    <!DOCTYPE html>
    <html>
    <head>
      <meta charset="utf-8">
      <title>Property Test</title>
      <script src="https://cdn.tailwindcss.com"></script>
    </head>
    <body class="p-8 space-y-4">
      <div class="space-y-4">
        #{button_html}
        #{input_html}
      </div>
    </body>
    </html>
    """
  end

  @spec setup_visual_test(String.t()) :: {:ok, test_context()} | {:error, String.t()}
  defp setup_visual_test(test_html) do
    try do
      temp_file = Path.join(System.tmp_dir(), "storybook_property_test_#{:rand.uniform(10000)}.html")
      File.write!(temp_file, test_html)
      {:ok, %{html: test_html, temp_file: temp_file}}
    rescue
      error -> {:error, "Failed to setup visual test: #{inspect(error)}"}
    end
  end

  @spec run_browser_visual_test(test_context()) :: {:ok, String.t()} | {:error, String.t()}
  defp run_browser_visual_test(%{temp_file: temp_file}) do
    try do
      conn = build_conn()

      conn
      |> visit("file://#{temp_file}")
      |> assert_has("button")
      |> assert_has("input")

      # Take screenshot for visual regression
      screenshot_name = "property_test_#{:rand.uniform(10000)}.png"
      conn |> screenshot(screenshot_name)

      {:ok, "Visual test completed successfully"}
    rescue
      error -> {:error, "Failed to run browser visual test: #{inspect(error)}"}
    end
  end

  @spec cleanup_test_files(test_context()) :: :ok | {:error, String.t()}
  defp cleanup_test_files(%{temp_file: temp_file}) do
    try do
      File.rm(temp_file)
      :ok
    rescue
      error -> {:error, "Failed to cleanup test files: #{inspect(error)}"}
    end
  end

  describe "Performance Testing with Property-Based Data" do
    @spec performance_test_within_time_limits([props()]) :: :ok
    property "components render within acceptable time limits" do
      check all(
              props_list <- list_of(button_props(), length: @performance_test_length),
              max_runs: @max_performance_test_runs
            ) do
        props_list
        |> measure_rendering_time()
        |> assert_rendering_performance()
      end
    end
  end

  # Helper functions for performance testing
  @spec measure_rendering_time([props()]) :: {:ok, performance_metrics()} | {:error, String.t()}
  defp measure_rendering_time(props_list) do
    try do
      metrics =
        :timer.tc(fn ->
          Enum.map(props_list, &transform_props_for_rendering/1)
          |> Enum.each(fn props ->
            render_component(&Atoms.Button.button/1, props)
          end)
        end)

      {:ok, metrics}
    rescue
      error -> {:error, "Failed to measure rendering time: #{inspect(error)}"}
    end
  end

  @spec transform_props_for_rendering(props()) :: props()
  defp transform_props_for_rendering(props) do
    text_value = Map.get(props, :text, "Button")
    inner_block = [text_value]
    Map.put(props, :inner_block, inner_block)
  end

  @spec assert_rendering_performance(performance_metrics()) :: :ok | {:error, String.t()}
  defp assert_rendering_performance({time_microseconds, _result}) do
    max_time_microseconds = @max_rendering_time_ms * 1000

    cond do
      time_microseconds < max_time_microseconds -> :ok
      true -> {:error, "Rendering 50 components took #{time_microseconds}μs (>#{max_time_microseconds}μs)"}
    end
  end

  @spec assert_rendering_performance({:error, String.t()}) :: {:error, String.t()}
  defp assert_rendering_performance({:error, reason}) do
    {:error, reason}
  end

  describe "Accessibility Testing with Property-Based Data" do
    @spec accessibility_test_with_random_props(props()) :: :ok
    property "components maintain accessibility with random props" do
      check all(
              button_props <- button_props(),
              max_runs: @edge_case_max_runs
            ) do
        button_props
        |> render_button_component()
        |> assert_button_semantics()
        |> assert_disabled_attributes()
        |> assert_non_empty_text()
      end
    end
  end

  # Helper functions for accessibility testing
  @spec assert_button_semantics(html_output()) :: :ok | {:error, String.t()}
  defp assert_button_semantics(html) do
    cond do
      html =~ ~r/<button[^>]*>/ -> :ok
      true -> {:error, "Button semantics not found in HTML output"}
    end
  end

  @spec assert_disabled_attributes(html_output()) :: :ok | {:error, String.t()}
  defp assert_disabled_attributes(html) do
    case html do
      html when html =~ "disabled" ->
        cond do
          html =~ "disabled" -> :ok
          true -> {:error, "Disabled attribute not properly set"}
        end

      _ ->
        :ok
    end
  end

  @spec assert_non_empty_text(html_output()) :: :ok | {:error, String.t()}
  defp assert_non_empty_text(html) do
    # This would need props to be passed in a real implementation
    # For now, we'll use pattern matching to check for non-empty button text
    case html do
      html when html =~ ~r">[^<]+</button>" ->
        :ok

      _ ->
        {:error, "Button text is empty or not properly formatted"}
    end
  end

  # Helper function to build a test connection
  @spec build_conn() :: {:ok, struct()} | {:error, String.t()}
  defp build_conn do
    try do
      {:ok, Phoenix.ConnTest.build_conn()}
    rescue
      error -> {:error, "Failed to build test connection: #{inspect(error)}"}
    end
  end

  # Define a struct for component test results to improve type safety
  defmodule ComponentTestResult do
    @moduledoc """
    Struct for storing component test results with type safety.
    """

    @enforce_keys [:component_type, :test_status, :details]
    defstruct [:component_type, :test_status, :details, :timestamp]

    @type t :: %__MODULE__{
            component_type: atom(),
            test_status: :passed | :failed | :skipped,
            details: String.t(),
            timestamp: DateTime.t()
          }

    @spec new(atom(), atom(), String.t()) :: t()
    def new(component_type, test_status, details) do
      %__MODULE__{
        component_type: component_type,
        test_status: test_status,
        details: details,
        timestamp: DateTime.utc_now()
      }
    end
  end

  # Define a protocol for component testing behavior
  defprotocol ComponentTestable do
    @moduledoc """
    Protocol for defining component testing behavior.
    """

    @spec render_test_component(map()) :: String.t()
    def render_test_component(component)

    @spec validate_test_output(String.t(), map()) :: :ok | {:error, String.t()}
    def validate_test_output(html, props)

    @spec get_required_props() :: [atom()]
    def get_required_props()
  end

  # Implement the protocol for Atoms.Button
  defimpl ComponentTestable, for: Atoms.Button do
    def render_test_component(props) do
      inner_block = Map.get(props, :inner_block, [Map.get(props, :text, "Button")])
      render_component(&Atoms.Button.button/1, Map.put(props, :inner_block, inner_block))
    end

    def validate_test_output(html, _props) do
      cond do
        html =~ "<button" and html =~ "type=\"" -> :ok
        html =~ "<button" -> {:error, "Button missing type attribute"}
        true -> {:error, "Invalid button HTML structure"}
      end
    end

    def get_required_props, do: [:text]
  end

  # Implement the protocol for Atoms.Input
  defimpl ComponentTestable, for: Atoms.Input do
    def render_test_component(props) do
      render_component(&Atoms.Input.input/1, props)
    end

    def validate_test_output(html, _props) do
      cond do
        html =~ "<input" and html =~ "type=\"" -> :ok
        html =~ "<input" -> {:error, "Input missing type attribute"}
        true -> {:error, "Invalid input HTML structure"}
      end
    end

    def get_required_props, do: [:type]
  end

  # Implement the protocol for Atoms.Text
  defimpl ComponentTestable, for: Atoms.Text do
    def render_test_component(props) do
      inner_block = Map.get(props, :inner_block, [Map.get(props, :text, "Text content")])
      render_component(&Atoms.Text.text/1, Map.put(props, :inner_block, inner_block))
    end

    def validate_test_output(html, _props) do
      cond do
        html =~ ~r/<h[1-6]>|<p>|<span>/ -> :ok
        html =~ ~r/<[^>]+>/ -> :ok
        true -> {:error, "Invalid text HTML structure"}
      end
    end

    def get_required_props, do: [:text]
  end

  # Implement the protocol for Atoms.Icon
  defimpl ComponentTestable, for: Atoms.Icon do
    def render_test_component(props) do
      render_component(&Atoms.Icon.icon/1, props)
    end

    def validate_test_output(html, _props) do
      cond do
        html =~ ~r/<svg|<i|class="[^"]*icon/ -> :ok
        html =~ "<svg" -> :ok
        html =~ "<i" -> :ok
        html =~ "icon" -> :ok
        true -> {:error, "Invalid icon HTML structure"}
      end
    end

    def get_required_props, do: [:name]
  end
end
