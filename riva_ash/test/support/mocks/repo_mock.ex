defmodule RivaAsh.Repo.Mock do
  @moduledoc """
  Mock module for RivaAsh.Repo to be used in tests.
  """
  
  @behaviour Ecto.Adapter
  @behaviour Ecto.Adapter.Storage
  @behaviour Ecto.Adapter.Structure
  
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
end
