defmodule RivaAsh.Components.UI.CombinationGenerators do
  use ExUnit.CaseTemplate
  import StreamData

  # Component state generators
  def size_variants, do: member_of([:compact, :comfortable, :spacious])
  def color_schemes, do: member_of([:light, :dark, :system])
  def density_states, do: member_of([:default, :dense])
  def viewport_sizes, do: member_of([:xs, :s, :m, :l])
  def interaction_states, do: member_of([:hover, :focus, :active, :disabled])

  # Base component combinations
  def component_combinations do
    bind(
      size_variants(),
      color_schemes(),
      density_states(),
      viewport_sizes(),
      interaction_states(),
      fn size, color, density, viewport, interaction ->
        %{
          size: size,
          color_scheme: color,
          density: density,
          viewport: viewport,
          interaction: interaction
        }
      end
    )
  end

  # Critical component groupings
  def app_shell_combinations do
    bind(
      component_combinations(),
      fn base ->
        map(base, fn b ->
          Map.merge(b, %{
            components: [:app_shell, :sidebar, :header],
            configurations: [
              sidebar_collapsed: boolean(),
              header_variant: member_of([:default, :compact]),
              navigation_depth: integer(1..3)
            ]
          })
        end)
      end
    )
  end

  def data_table_combinations do
    bind(
      component_combinations(),
      fn base ->
        map(base, fn b ->
          Map.merge(b, %{
            components: [:data_table, :filter_panel, :pagination, :empty_state],
            configurations: [
              column_count: integer(3..10),
              row_count: integer(5..100),
              filter_types: list_of(member_of([:text, :date, :select]), min_length: 1),
              pagination_type: member_of([:standard, :infinite])
            ]
          })
        end)
      end
    )
  end

  def form_combinations do
    bind(
      component_combinations(),
      fn base ->
        map(base, fn b ->
          Map.merge(b, %{
            components: [:form_field, :validation, :confirm_dialog],
            configurations: [
              field_count: integer(2..8),
              validation_scenarios: list_of(member_of([:required, :format, :range]), min_length: 1),
              dialog_triggers: member_of([:submit, :cancel, :partial])
            ]
          })
        end)
      end
    )
  end

  def command_palette_combinations do
    bind(
      component_combinations(),
      fn base ->
        map(base, fn b ->
          Map.merge(b, %{
            components: [:command_palette, :search_bar],
            configurations: [
              command_count: integer(5..50),
              search_type: member_of([:instant, :debounced]),
              result_display: member_of([:list, :grid]),
              keyboard_shortcuts: boolean()
            ]
          })
        end)
      end
    )
  end
end
