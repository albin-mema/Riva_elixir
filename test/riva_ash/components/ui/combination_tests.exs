defmodule RivaAsh.Components.UI.CombinationTests do
  use ExUnit.Case, async: true
  use Wallaby.Feature
  import StreamData
  alias RivaAsh.Components.UI.CombinationGenerators

  setup do
    {:ok, session} = Wallaby.start_session()
    {:ok, session: session}
  end

  describe "AppShell combinations" do
    property "passes accessibility checks across all variants", [
      max_runs: 100
    ] do
      combination <- CombinationGenerators.app_shell_combinations()

      check_accessibility(
        combination,
        "organisms-appshell",
        "default"
      )
    end

    property "maintains visual consistency across states", [
      max_runs: 100
    ] do
      combination <- CombinationGenerators.app_shell_combinations()

      capture_visual_snapshot(
        combination,
        "app_shell_combinations"
      )
    end
  end

  describe "DataTable combinations" do
    property "passes accessibility checks across all variants", [
      max_runs: 100
    ] do
      combination <- CombinationGenerators.data_table_combinations()

      check_accessibility(
        combination,
        "organisms-datatable",
        "default"
      )
    end

    property "maintains visual consistency across states", [
      max_runs: 100
    ] do
      combination <- CombinationGenerators.data_table_combinations()

      capture_visual_snapshot(
        combination,
        "data_table_combinations"
      )
    end
  end

  describe "Form combinations" do
    property "passes accessibility checks across all variants", [
      max_runs: 100
    ] do
      combination <- CombinationGenerators.form_combinations()

      check_accessibility(
        combination,
        "organisms-form",
        "default"
      )
    end

    property "maintains visual consistency across states", [
      max_runs: 100
    ] do
      combination <- CombinationGenerators.form_combinations()

      capture_visual_snapshot(
        combination,
        "form_combinations"
      )
    end
  end

  describe "CommandPalette combinations" do
    property "passes accessibility checks across all variants", [
      max_runs: 100
    ] do
      combination <- CombinationGenerators.command_palette_combinations()

      check_accessibility(
        combination,
        "molecules-commandpalette",
        "default"
      )
    end

    property "maintains visual consistency across states", [
      max_runs: 100
    ] do
      combination <- CombinationGenerators.command_palette_combinations()

      capture_visual_snapshot(
        combination,
        "command_palette_combinations"
      )
    end
  end

  defp check_accessibility(session, combination, story_path, story_name) do
    url = build_storybook_url(story_path, story_name, combination)

    session
    |> visit(url)
    |> Percy.Snapshot.snapshot("Accessibility Check: #{story_path}/#{story_name}",
      widths: [375, 768, 1024, 1440],
      percy_css: """
        .sb-show-main.sb-main-padded { padding: 0 !important; }
      """
    )
    |> assert_no_axe_violations()
  end

  defp capture_visual_snapshot(session, combination, name_prefix) do
    url = build_storybook_url(
      combination[:components] |> Enum.join("-"),
      "combination",
      combination
    )

    session
    |> visit(url)
    |> Percy.Snapshot.snapshot(
      "#{name_prefix} - #{inspect(combination)}",
      widths: [375, 768, 1024, 1440],
      percy_css: """
        .sb-show-main.sb-main-padded { padding: 0 !important; }
      """,
      scope: ".sb-story"
    )
  end

  defp build_storybook_url(story_path, story_name, combination) do
    args =
      combination
      |> Map.drop([:components, :configurations])
      |> URI.encode_query()

    "http://localhost:6006/iframe.html?id=#{story_path}--#{story_name}&args=#{args}"
  end

  defp assert_no_axe_violations(session) do
    script = """
    return new Promise((resolve) => {
      const script = document.createElement('script');
      script.src = 'https://cdnjs.cloudflare.com/ajax/libs/axe-core/4.7.0/axe.min.js';
      script.onload = () => {
        axe.run({ include: ['.sb-story'] }).then(results => {
          resolve({
            violations: results.violations,
            passes: results.passes
          });
        });
      };
      document.head.appendChild(script);
    });
    """

    %{violations: violations} = session |> execute_script(script)

    assert violations == [],
      "Accessibility violations found: #{inspect(violations, pretty: true)}"
  end
end
