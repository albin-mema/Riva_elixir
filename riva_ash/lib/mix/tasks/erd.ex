defmodule Mix.Tasks.Erd do
  @moduledoc """
  Generates and optionally displays an ERD for the application.

  This is a wrapper around Ash's built-in diagram generation.

  ## Examples

      # Generate ERD in Mermaid format (default)
      mix erd
      
      # Generate and open in browser (requires Mermaid CLI)
      mix erd --open
      
      # Generate SVG output
      mix erd --format svg
  """
  use Mix.Task

  @shortdoc "Generate an ERD for the application"

  @switches [
    open: :boolean,
    format: :string,
    type: :string
  ]

  @impl true
  def run(args) do
    {opts, _} = OptionParser.parse!(args, strict: @switches)

    format = opts[:format] || "mmd"
    type = opts[:type] || "er"

    # Generate the diagram
    Mix.Task.run("ash.generate_resource_diagrams", [
      "--type",
      type,
      "--format",
      format
    ])

    output_file = "lib/riva_ash/domain-mermaid-#{type}-diagram.#{format}"

    if File.exists?(output_file) do
      if opts[:open] do
        open_diagram(output_file, format)
      else
        IO.puts("\nERD generated at: #{output_file}")
        IO.puts("To view: mix erd --open")
      end
    end
  end

  defp open_diagram(file, "svg") do
    if System.find_executable("xdg-open") do
      System.cmd("xdg-open", [file])
    else
      IO.puts("Could not find xdg-open. Please open manually: #{file}")
    end
  end

  defp open_diagram(file, "mmd") do
    content = File.read!(file)

    # Create a simple HTML file to view the Mermaid diagram
    html = """
    <!DOCTYPE html>
    <html>
    <head>
      <title>ERD Viewer</title>
      <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
      <style>
        body { margin: 0; padding: 20px; }
        .mermaid { 
          font-family: Arial, sans-serif;
          margin: 20px auto;
          max-width: 1200px;
        }
      </style>
    </head>
    <body>
      <div class="mermaid">
        #{content}
      </div>
      <script>
        mermaid.initialize({ startOnLoad: true });
      </script>
    </body>
    </html>
    """

    html_file = "erd_viewer.html"
    File.write!(html_file, html)

    if System.find_executable("xdg-open") do
      System.cmd("xdg-open", [html_file])
    else
      IO.puts("Could not find xdg-open. Please open manually: #{html_file}")
    end
  end

  defp open_diagram(file, _format) do
    IO.puts("Generated file: #{file}")
    IO.puts("To open automatically, use --format svg or --format mmd")
  end
end
