#!/usr/bin/env elixir

# Script to generate Mermaid diagrams from Reactor modules
# Usage: elixir scripts/generate_reactor_diagram.exs ModuleName

Mix.install([
  {:reactor, "~> 0.9.1"}
])

defmodule DiagramGenerator do
  def generate_for_module(module_name) do
    try do
      module = String.to_existing_atom("Elixir.#{module_name}")
      
      if Code.ensure_loaded?(module) do
        case Reactor.Mermaid.to_mermaid(module) do
          {:ok, mermaid} -> 
            IO.puts("Generated Mermaid diagram for #{module_name}:")
            IO.puts(mermaid)
          {:error, reason} -> 
            IO.puts("Error generating diagram: #{inspect(reason)}")
        end
      else
        IO.puts("Module #{module_name} not found or not loaded")
      end
    rescue
      e -> IO.puts("Error: #{inspect(e)}")
    end
  end
end

# Get module name from command line arguments
case System.argv() do
  [module_name] -> 
    DiagramGenerator.generate_for_module(module_name)
  _ -> 
    IO.puts("Usage: elixir generate_reactor_diagram.exs ModuleName")
    IO.puts("Example: elixir generate_reactor_diagram.exs RivaAsh.Reactors.BasicReactor")
end
