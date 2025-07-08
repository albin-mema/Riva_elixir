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

  # Schema callbacks
  def autogenerate(:id), do: nil
  def autogenerate(_), do: nil
  def insert_all(_repo, _schema_meta, _header, _rows, _on_conflict, _returning, _opts), do: {0, nil}
  def insert(_repo, _schema_meta, _fields, _on_conflict, _returning, _opts), do: {:ok, %{}}
  def update(_repo, _schema_meta, _fields, _filter, _returning, _opts), do: {:ok, %{}}
  def delete(_repo, _schema_meta, _filter, _opts), do: {:ok, %{}}

  # Transaction callbacks
  def transaction(_repo, _opts, _fun), do: {:ok, :ok}
  def in_transaction?(_repo), do: false
  def rollback(_repo, _value), do: throw({:ecto_rollback, _value})
end
