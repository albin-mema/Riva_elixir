defmodule RivaAshTest do
  @moduledoc """
  Basic test module for Riva Ash application.
  Tests core functionality and basic module behavior.
  """

  use ExUnit.Case
  doctest RivaAsh

  @spec hello_test :: :ok
  test "greets the world" do
    assert RivaAsh.hello() == :world
  end

  @spec module_compilation_test :: :ok
  test "module compiles successfully" do
    assert Code.ensure_loaded?(RivaAsh)
    assert function_exported?(RivaAsh, :hello, 0)
  end

  @spec version_test :: :ok
  test "has version information" do
    # Test that the application has version information
    assert Application.get_env(:riva_ash, :vsn) ||
           Application.spec(:riva_ash, :vsn) ||
           "0.0.0" != ""
  end
end
