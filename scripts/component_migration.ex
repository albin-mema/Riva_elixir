defmodule ComponentMigration do
  def run do
    Mix.Task.run("app.start")
    # Create new directories
    File.mkdir_p!("lib/riva_ash_web/components/organisms/core_components")
    File.mkdir_p!("lib/riva_ash_web/components/templates/layouts")

    # Move core components
    File.rename!(
      "lib/riva_ash_web/components/core_components.ex",
      "lib/riva_ash_web/components/organisms/core_components/core_components.ex"
    )

    # Reclassify interactive components
    interactive_components = [
      {"interactive/availability_grid.ex", "molecules/availability_grid.ex"},
      {"interactive/daily_schedule.ex", "molecules/daily_schedule.ex"},
      {"interactive/grid_position_picker.ex", "organisms/layout/grid_position_picker.ex"},
      {"interactive/monthly_calendar.ex", "molecules/monthly_calendar.ex"},
      {"interactive/plot_layout_designer.ex", "organisms/layout/designer.ex"},
      {"interactive/recurrence_pattern.ex", "organisms/reservations/recurrence_pattern.ex"},
      {"interactive/time_slot_picker.ex", "molecules/time_slot_picker.ex"},
      {"interactive/weekly_calendar.ex", "molecules/weekly_calendar.ex"}
    ]

    Enum.each(interactive_components, fn {src, dest} ->
      File.rename!(
        "lib/riva_ash_web/components/#{src}",
        "lib/riva_ash_web/components/#{dest}"
      )
    end)

    # Update references in affected files
    update_references()

    # Verify migration success
    verify_migration()
  end

  defp verify_migration do
    required_paths = [
      "lib/riva_ash_web/components/organisms/core_components/core_components.ex",
      "lib/riva_ash_web/components/templates/layouts"
    ]

    Enum.each(required_paths, fn path ->
      unless File.exists?(path) do
        raise "Migration failed: #{path} not created"
      end
    end)

    moved_components = [
      "lib/riva_ash_web/components/molecules/availability_grid.ex",
      "lib/riva_ash_web/components/organisms/layout/designer.ex"
    ]

    Enum.each(moved_components, fn path ->
      unless File.exists?(path) do
        raise "Component move failed: #{path} not found"
      end
    end)
  end

  defp update_references do
    # Pattern to update CoreComponents references
    files = Path.wildcard("lib/riva_ash_web/**/*.ex") ++
            Path.wildcard("test/riva_ash_web/**/*.exs")

    Enum.each(files, fn file ->
      content = File.read!(file)
      updated = String.replace(
        content,
        "RivaAshWeb.CoreComponents",
        "RivaAshWeb.Components.Organisms.CoreComponents"
      )
      File.write!(file, updated)
    end)
  end
end
