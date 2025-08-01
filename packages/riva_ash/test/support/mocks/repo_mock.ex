defmodule RivaAsh.Repo.Mock do
  @moduledoc """
  Mock module for RivaAsh.Repo to be used in tests.
  """

  @behaviour Ecto.Adapter
  @behaviour Ecto.Adapter.Storage
  @behaviour Ecto.Adapter.Structure
  @behaviour Ecto.Adapter.Queryable
  @behaviour Ecto.Adapter.Schema
  @behaviour Ecto.Adapter.Transaction
  @behaviour Ecto.Adapter.Migration

  # Mock implementation for the query function
  def query(_query, _params \\ [], _opts \\ []) do
    # This will be overridden by Mox in tests
    {:ok, %{rows: [[1]]}}
  end

  # Required callbacks for Ecto.Adapter
  defmacro __before_compile__(_env), do: :ok
  def ensure_all_started(_, _), do: {:ok, []}
  def init(_config), do: {:ok, %{}}

  # Storage callbacks
  def storage_up(_opts), do: :ok
  def storage_down(_opts), do: :ok
  def storage_status(_opts), do: :up

  # Structure callbacks
  def structure_dump(_, _), do: ""
  def structure_load(_, _), do: :ok

  # Queryable callbacks
  def execute(_repo, _query_meta, _query, _params, _process, _opts), do: {:ok, [], []}
  def stream(_repo, _query_meta, _query, _params, _opts), do: []
  def prepare(_atom, _query), do: {:nocache, nil}

  # Schema callbacks
  def autogenerate(:id), do: nil
  def autogenerate(_), do: nil

  def insert_all(_repo, _schema_meta, _header, _rows, _on_conflict, _returning, _opts, _options),
    do: {0, nil}

  def insert(_repo, _schema_meta, _fields, _on_conflict, _returning, _opts), do: {:ok, %{}}
  def update(_repo, _schema_meta, _fields, _filter, _returning, _opts), do: {:ok, %{}}
  def delete(_repo, _schema_meta, _filter, _returning, _opts), do: {:ok, %{}}
  def loaders(_primitive_type, _ecto_type), do: [& &1]
  def dumpers(_primitive_type, _ecto_type), do: [& &1]

  # Transaction callbacks
  def transaction(_repo, _opts, _fun), do: {:ok, :ok}
  def in_transaction?(_repo), do: false
  def rollback(_repo, value), do: throw({:ecto_rollback, value})

  # Connection callbacks
  def checkout(_repo, _opts, _fun), do: {:ok, :dummy_conn}
  def checked_out?(_repo), do: false

  # Migration callbacks
  def supports_ddl_transaction?, do: true
  def lock_for_migrations(_repo, _opts, _fun), do: {:ok, :dummy_lock}
  def execute_ddl(_repo, _command, _opts), do: {:ok, :dummy_result}
  def execute_migration_in_transaction(_repo, _migration, _cmd), do: true
end
