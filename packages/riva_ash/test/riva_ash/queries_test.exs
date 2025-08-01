defmodule RivaAsh.QueriesTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Queries

  describe "build_query/2" do
    test "builds query with filters" do
      filters = %{status: "active", type: "premium"}
      query = Queries.build_query("users", filters)
      assert is_struct(query, Ecto.Query)
    end

    test "builds query with empty filters" do
      query = Queries.build_query("users", %{})
      assert is_struct(query, Ecto.Query)
    end

    test "handles complex filters" do
      filters = %{
        status: "active",
        inserted_at: %{gte: ~D[2024-01-01], lte: ~D[2024-12-31]},
        name: %{ilike: "test"}
      }

      query = Queries.build_query("users", filters)
      assert is_struct(query, Ecto.Query)
    end
  end

  describe "apply_pagination/2" do
    test "applies pagination to query" do
      base_query = Queries.build_query("users", %{})
      paginated_query = Queries.apply_pagination(base_query, %{limit: 10, offset: 0})

      assert is_struct(paginated_query, Ecto.Query)
    end

    test "handles nil pagination" do
      base_query = Queries.build_query("users", %{})
      paginated_query = Queries.apply_pagination(base_query, nil)

      assert is_struct(paginated_query, Ecto.Query)
    end
  end

  describe "apply_sorting/2" do
    test "applies sorting to query" do
      base_query = Queries.build_query("users", %{})
      sorted_query = Queries.apply_sorting(base_query, %{field: :name, direction: :asc})

      assert is_struct(sorted_query, Ecto.Query)
    end

    test "handles multiple sort fields" do
      base_query = Queries.build_query("users", %{})

      sorted_query =
        Queries.apply_sorting(base_query, [
          %{field: :name, direction: :asc},
          %{field: :inserted_at, direction: :desc}
        ])

      assert is_struct(sorted_query, Ecto.Query)
    end
  end

  describe "apply_joins/2" do
    test "applies joins to query" do
      base_query = Queries.build_query("users", %{})

      joined_query =
        Queries.apply_joins(base_query, [
          %{table: :businesses, on: [user_id: :id]}
        ])

      assert is_struct(joined_query, Ecto.Query)
    end
  end

  describe "execute_query/1" do
    test "executes query and returns results" do
      query = Queries.build_query("users", %{})
      assert {:ok, results} = Queries.execute_query(query)
      assert is_list(results)
    end

    test "handles empty results" do
      query = Queries.build_query("users", %{id: "nonexistent"})
      assert {:ok, []} = Queries.execute_query(query)
    end
  end

  describe "count_query/1" do
    test "returns count of query results" do
      query = Queries.build_query("users", %{})
      assert {:ok, count} = Queries.count_query(query)
      assert is_integer(count)
      assert count >= 0
    end
  end

  describe "build_aggregate_query/2" do
    test "builds aggregate query" do
      query =
        Queries.build_aggregate_query("users", %{
          group_by: [:status],
          aggregates: [count: :id, max: :inserted_at]
        })

      assert is_struct(query, Ecto.Query)
    end
  end

  describe "search_query/2" do
    test "builds search query" do
      search_term = "test"
      fields = [:name, :email]

      query = Queries.search_query("users", %{term: search_term, fields: fields})
      assert is_struct(query, Ecto.Query)
    end

    test "handles empty search term" do
      query = Queries.search_query("users", %{term: "", fields: [:name]})
      assert is_struct(query, Ecto.Query)
    end
  end
end
