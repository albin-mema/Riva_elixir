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
  import OK, only: [success: 1, failure: 1, ~>>: 2, for: 1, required: 2]

  @shortdoc "Generate an ERD for the application"

  @switches [
    open: :boolean,
    format: :string,
    type: :string
  ]

  @impl true
  def run(args) do
    OK.for do
      {opts, _} <- OK.wrap(OptionParser.parse!(args, strict: @switches))
      format = opts[:format] || "mmd"
      type = opts[:type] || "er"
      _ <- generate_diagram(type, format)
      output_file = "lib/riva_ash/domain-mermaid-#{type}-diagram.#{format}"
      _ <- OK.required(File.exists?(output_file), :output_file_not_found)
    after
      if opts[:open] do
        open_diagram(output_file, format)
      else
        IO.puts("\nERD generated at: #{output_file}")
        IO.puts("To view: mix erd --open")
      end
    else
      error ->
        IO.puts("Failed to generate ERD: #{inspect(error)}")
    end
  end

  defp generate_diagram(type, format) do
    Mix.Task.run("ash.generate_resource_diagrams", [
      "--type",
      type,
      "--format",
      format
    ])
    |> OK.wrap()
  end

  defp open_diagram(file, "svg") do
    System.find_executable("xdg-open")
    |> OK.required(:xdg_open_not_found)
    ~>> fn _ ->
      System.cmd("xdg-open", [file])
    end
    |> case do
      {:ok, _} -> :ok
      {:error, :xdg_open_not_found} ->
        IO.puts("Could not find xdg-open. Please open manually: #{file}")
    end
  end

  defp open_diagram(file, "mmd") do
    OK.for do
      content <- File.read(file)
      html = create_html_content(content)
      html_file = "erd_viewer.html"
      _ <- File.write(html_file, html)
      _ <- open_html_file(html_file)
    after
      :ok
    else
      error ->
        IO.puts("Failed to open diagram: #{inspect(error)}")
    end
  end

  defp create_html_content(content) do
    """
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
  end

  defp open_html_file(html_file) do
    System.find_executable("xdg-open")
    |> OK.required(:xdg_open_not_found)
    ~>> fn _ ->
      System.cmd("xdg-open", [html_file])
    end
    |> case do
      {:ok, _} -> success(:opened)
      {:error, :xdg_open_not_found} ->
        IO.puts("Could not find xdg-open. Please open manually: #{html_file}")
        success(:manual_open_required)
    end
  end

  defp open_diagram(file, _format) do
    IO.puts("Generated file: #{file}")
    IO.puts("To open automatically, use --format svg or --format mmd")
  end
end
