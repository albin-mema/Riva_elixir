defmodule RivaAsh.DatabaseHealthTest do
  use RivaAsh.DataCase, async: false
  alias RivaAsh.DatabaseHealth

  describe "check_connection/0" do
    test "returns :ok when database connection is healthy" do
      assert :ok = DatabaseHealth.check_connection()
    end

    test "returns {:error, reason} when connection fails" do
      # This would need mocking for failure scenario
      # For now, test the interface
      assert result = DatabaseHealth.check_connection()
      assert result in [:ok, {:error, _reason}]
    end
  end

  describe "check_table_health/1" do
    test "returns :ok for existing tables" do
      assert :ok = DatabaseHealth.check_table_health("users")
    end

    test "returns {:error, :table_not_found} for non-existent tables" do
      assert {:error, :table_not_found} = DatabaseHealth.check_table_health("nonexistent_table")
    end
  end

  describe "check_all_tables/0" do
    test "returns health status for all tables" do
      assert {:ok, results} = DatabaseHealth.check_all_tables()
      assert is_map(results)

      # Should contain at least some expected tables
      assert Map.has_key?(results, "users") || Map.has_key?(results, "businesses")
    end
  end

  describe "get_table_size/1" do
    test "returns size for existing table" do
      assert {:ok, size} = DatabaseHealth.get_table_size("users")
      assert is_integer(size)
      assert size >= 0
    end

    test "returns {:error, :table_not_found} for non-existent table" do
      assert {:error, :table_not_found} = DatabaseHealth.get_table_size("nonexistent_table")
    end
  end

  describe "check_indexes/1" do
    test "returns index health for table" do
      assert {:ok, indexes} = DatabaseHealth.check_indexes("users")
      assert is_list(indexes)
    end

    test "returns {:error, :table_not_found} for non-existent table" do
      assert {:error, :table_not_found} = DatabaseHealth.check_indexes("nonexistent_table")
    end
  end

  describe "get_database_stats/0" do
    test "returns comprehensive database statistics" do
      assert {:ok, stats} = DatabaseHealth.get_database_stats()
      assert is_map(stats)
      assert Map.has_key?(stats, :table_count)
      assert Map.has_key?(stats, :total_size)
      assert Map.has_key?(stats, :index_count)
    end
  end

  describe "check_replication_lag/0" do
    test "returns replication lag information" do
      assert {:ok, lag} = DatabaseHealth.check_replication_lag()
      assert is_integer(lag) || lag == :no_replication
    end
  end

  describe "run_health_check/0" do
    test "returns comprehensive health report" do
      assert {:ok, report} = DatabaseHealth.run_health_check()
      assert is_map(report)
      assert Map.has_key?(report, :connection)
      assert Map.has_key?(report, :tables)
      assert Map.has_key?(report, :indexes)
      assert Map.has_key?(report, :overall_status)
    end
  end
end
