defmodule Storybook.Organisms.LayoutDesigner do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.LayoutDesigner.layout_designer/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          layout: %{
            id: "layout-1",
            name: "Beach Resort Layout"
          },
          items: [
            %{id: "1", name: "Umbrella 1", row: 2, column: 3},
            %{id: "2", name: "Sunbed 1", row: 2, column: 4},
            %{id: "3", name: "Cabin 1", row: 5, column: 7}
          ],
          grid_rows: 10,
          grid_columns: 10,
          on_item_move: "move_item",
          on_item_add: "add_item",
          on_item_remove: "remove_item",
          on_grid_resize: "resize_grid",
          editable: true
        }
      },
      %Variation{
        id: :readonly,
        attributes: %{
          layout: %{
            id: "layout-2",
            name: "Pool Area Layout"
          },
          items: [
            %{id: "4", name: "Sunbed 2", row: 3, column: 2},
            %{id: "5", name: "Sunbed 3", row: 3, column: 3},
            %{id: "6", name: "Cabin 2", row: 7, column: 5}
          ],
          grid_rows: 8,
          grid_columns: 8,
          on_item_move: "move_item",
          on_item_add: "add_item",
          on_item_remove: "remove_item",
          on_grid_resize: "resize_grid",
          editable: false
        }
      },
      %Variation{
        id: :empty_grid,
        attributes: %{
          layout: %{
            id: "layout-3",
            name: "Empty Layout"
          },
          items: [],
          grid_rows: 5,
          grid_columns: 5,
          on_item_move: "move_item",
          on_item_add: "add_item",
          on_item_remove: "remove_item",
          on_grid_resize: "resize_grid",
          editable: true
        }
      }
    ]
  end
end
