defmodule RivaAsh.TestHelpers do
  @moduledoc """
  Helper functions for testing.

  This module provides common test helpers for setting up test data,
  making requests, and asserting responses in a consistent way.

  ## Usage

  Add this to your test module:

      use RivaAsh.TestHelpers

  Or import specific functions:

      import RivaAsh.TestHelpers, only: [create_item: 1]
  """

  import ExUnit.Assertions
  import Plug.Conn
  import Phoenix.ConnTest

  alias RivaAsh.Resources.Item
  alias RivaAsh.Domain
  alias Plug.Conn

  @doc """
  Setup the SQL sandbox for tests.

  This should be called in the `setup` block of test modules.
  """
  @spec setup_sandbox(Keyword.t()) :: :ok
  def setup_sandbox(tags) do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, {:shared, self()})
    end

    :ok
  end

  @doc """
  Run a function in a sandbox transaction.

  This is useful for isolating test data within a test case.
  """
  @spec sandboxed((-> any())) :: any()
  def sandboxed(fun) when is_function(fun, 0) do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)
    fun.()
  end

  @doc """
  Create a test item with the given attributes.

  ## Examples

      # Create with default name
      {:ok, item} = create_item()
      
      # Create with custom attributes
      {:ok, item} = create_item(%{name: "Custom Name"})
      
  """
  @spec create_item(map()) :: {:ok, Item.t()} | {:error, any()}
  def create_item(attrs) when is_map(attrs) do
    defaults = %{
      name: "Test Item #{System.unique_integer([:positive, :monotonic])}"
    }

    attrs = Map.merge(defaults, attrs)
    Ash.create(Item, attrs, domain: Domain)
  end

  @doc """
  Create a test item with default attributes.
  """
  @spec create_item() :: {:ok, Item.t()} | {:error, any()}
  def create_item do
    create_item(%{})
  end

  @doc """
  Create a test item and return it, raising on error.

  ## Examples

      # Create with default name
      item = create_item!()
      
      # Create with custom attributes
      item = create_item!(%{name: "Custom Name"})
      
  """
  @spec create_item!(map()) :: Item.t()
  def create_item!(attrs) when is_map(attrs) do
    case create_item(attrs) do
      {:ok, item} -> item
      {:error, error} -> raise "Failed to create test item: #{inspect(error)}"
    end
  end

  @doc """
  Create a test item with default attributes and return it, raising on error.
  """
  @spec create_item!() :: Item.t()
  def create_item! do
    create_item!(%{})
  end

  @doc """
  Recursively assert that all key-value pairs in expected exist in actual.

  This is more flexible than a direct comparison as it allows for partial matches.
  """
  @spec assert_maps_match(any(), any(), [String.t()]) :: :ok | no_return()
  def assert_maps_match(expected, actual, path \\ []) do
    cond do
      is_map(expected) and is_map(actual) ->
        Enum.each(expected, fn {key, expected_value} ->
          assert Map.has_key?(actual, key),
                 "Expected key `#{key}` not found in #{inspect(actual)} at path #{inspect(path)}"

          assert_maps_match(
            expected_value,
            actual[key] || actual[to_string(key)],
            path ++ [to_string(key)]
          )
        end)

      is_list(expected) and is_list(actual) ->
        assert length(expected) == length(actual),
               "Expected list of length #{length(expected)} but got #{length(actual)} at path #{inspect(path)}"

        Enum.zip(expected, actual)
        |> Enum.with_index()
        |> Enum.each(fn {{expected_item, actual_item}, index} ->
          assert_maps_match(expected_item, actual_item, path ++ ["[#{index}]"])
        end)

      true ->
        assert expected == actual,
               "Expected #{inspect(expected)} but got #{inspect(actual)} at path #{inspect(path)}"
    end

    :ok
  end

  @doc """
  Generate a unique string for testing.

  This is useful for creating unique values in tests to avoid conflicts.

  ## Examples

      # Generate a unique string
      unique = unique_string()
      
      # With a prefix
      email = unique_string("user_") <> "@example.com"
      
  """
  @spec unique_string(String.t()) :: String.t()
  def unique_string(prefix) when is_binary(prefix) do
    "#{prefix}#{System.unique_integer([:positive, :monotonic])}"
  end

  @doc """
  Generate a unique string with no prefix.
  """
  @spec unique_string() :: String.t()
  def unique_string do
    unique_string("")
  end
end
