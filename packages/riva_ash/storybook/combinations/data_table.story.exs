defmodule RivaAshWeb.Storybook.Combinations.DataTable do
  use PhoenixStorybook.Story, :component
  use RivaAshWeb.Components.AtomicComponents

  def function, do: &data_table_example/1

  def data_table_example(assigns) do
    ~H"""
    <div class="p-6">
      <.data_table
        data={@data}
        columns={[
          %{key: :id, label: "ID", sortable: true},
          %{key: :name, label: "Name", sortable: true},
          %{key: :status, label: "Status", sortable: false}
        ]}
      />
      <div class="mt-4 flex justify-center">
        <.pagination
          current_page={@current_page}
          total_pages={5}
        />
      </div>
    </div>
    """
  end

  def variations do
    [
      %Variation{
        id: :paginated,
        attributes: %{
          data: Enum.map(1..10, &%{id: &1, name: "Item #{&1}", status: "active"}),
          current_page: 1
        }
      }
    ]
  end
end
