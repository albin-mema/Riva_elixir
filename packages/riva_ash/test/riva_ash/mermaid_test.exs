defmodule RivaAsh.MermaidTest do
  use ExUnit.Case, async: true
  alias RivaAsh.Mermaid

  @moduletag :unit
  @moduletag :pure
  @moduletag :fast

  describe "generate_flowchart/2" do
    test "generates valid mermaid flowchart" do
      nodes = [
        %{id: "start", label: "Start", type: "start"},
        %{id: "process", label: "Process", type: "process"},
        %{id: "end", label: "End", type: "end"}
      ]

      connections = [
        %{from: "start", to: "process"},
        %{from: "process", to: "end"}
      ]

      result = Mermaid.generate_flowchart(nodes, connections)
      assert is_binary(result)
      assert String.contains?(result, "flowchart TD")
      assert String.contains?(result, "start")
      assert String.contains?(result, "process")
      assert String.contains?(result, "end")
    end

    test "handles empty nodes and connections" do
      result = Mermaid.generate_flowchart([], [])
      assert is_binary(result)
      assert String.contains?(result, "flowchart TD")
    end
  end

  describe "generate_sequence_diagram/2" do
    test "generates valid mermaid sequence diagram" do
      participants = ["User", "API", "Database"]
      messages = [
        %{from: "User", to: "API", message: "Request"},
        %{from: "API", to: "Database", message: "Query"},
        %{from: "Database", to: "API", message: "Response"},
        %{from: "API", to: "User", message: "Result"}
      ]

      result = Mermaid.generate_sequence_diagram(participants, messages)
      assert is_binary(result)
      assert String.contains?(result, "sequenceDiagram")
      assert String.contains?(result, "User")
      assert String.contains?(result, "API")
      assert String.contains?(result, "Database")
    end
  end

  describe "generate_class_diagram/1" do
    test "generates valid mermaid class diagram" do
      classes = [
        %{
          name: "User",
          attributes: ["id: string", "name: string", "email: string"],
          methods: ["login()", "logout()"]
        },
        %{
          name: "Business",
          attributes: ["id: string", "name: string"],
          methods: ["create()", "update()"]
        }
      ]

      relationships = [
        %{from: "User", to: "Business", type: "has_many"}
      ]

      result = Mermaid.generate_class_diagram(classes, relationships)
      assert is_binary(result)
      assert String.contains?(result, "classDiagram")
      assert String.contains?(result, "User")
      assert String.contains?(result, "Business")
    end
  end

  describe "generate_state_diagram/1" do
    test "generates valid mermaid state diagram" do
      states = ["Pending", "Processing", "Completed", "Failed"]
      transitions = [
        %{from: "Pending", to: "Processing", label: "start"},
        %{from: "Processing", to: "Completed", label: "success"},
        %{from: "Processing", to: "Failed", label: "error"}
      ]

      result = Mermaid.generate_state_diagram(states, transitions)
      assert is_binary(result)
      assert String.contains?(result, "stateDiagram-v2")
      assert String.contains?(result, "Pending")
      assert String.contains?(result, "Processing")
    end
  end

  describe "save_diagram/2" do
    test "saves diagram to file" do
      diagram = "flowchart TD\n    A-->B"
      filename = "test_diagram_#{System.unique_integer()}.mmd"

      assert :ok = Mermaid.save_diagram(diagram, filename)

      # Clean up
      File.rm(filename)
    end

    test "handles invalid filename" do
      diagram = "flowchart TD\n    A-->B"
      assert {:error, _} = Mermaid.save_diagram(diagram, "/invalid/path/test.mmd")
    end
  end

  describe "render_diagram/1" do
    test "renders diagram to SVG" do
      diagram = "flowchart TD\n    A-->B"
      assert {:ok, svg} = Mermaid.render_diagram(diagram)
      assert is_binary(svg)
      assert String.contains?(svg, "<svg")
    end

    test "handles invalid diagram syntax" do
      invalid_diagram = "invalid mermaid syntax"
      assert {:error, _} = Mermaid.render_diagram(invalid_diagram)
    end
  end

  describe "validate_diagram/1" do
    test "validates correct diagram syntax" do
      valid_diagram = "flowchart TD\n    A-->B"
      assert :ok = Mermaid.validate_diagram(valid_diagram)
    end

    test "returns error for invalid syntax" do
      invalid_diagram = "invalid syntax"
      assert {:error, _} = Mermaid.validate_diagram(invalid_diagram)
    end
  end
end
