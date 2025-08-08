alias Mix.Task
alias Mix.Tasks

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
    {opts, _unmatched} = OptionParser.parse!(args, strict: @switches)
    format = opts[:format] || "mmd"
    type = opts[:type] || "er"

    case generate_diagram(type, format) do
      :ok ->
        handle_successful_generation(type, format, opts)

      {:error, error} ->
        IO.puts("Failed to generate ERD: #{inspect(error)}")
    end
  rescue
    error ->
      IO.puts("Failed to generate ERD: #{inspect(error)}")
  end

  defp handle_successful_generation(type, format, opts) do
    output_file = "lib/riva_ash/domain-mermaid-#{type}-diagram.#{format}"

    if File.exists?(output_file) do
      if opts[:open] do
        open_diagram(output_file, format)
      else
        IO.puts("\nERD generated at: #{output_file}")
        IO.puts("To view: mix erd --open")
      end
    else
      IO.puts("Failed to generate ERD: output file not found")
    end
  end

  defp generate_diagram(type, format) do
    Mix.Task.run("ash.generate_resource_diagrams", [
      "--type",
      type,
      "--format",
      format
    ])

    :ok
  rescue
    error -> {:error, error}
  end

  defp open_diagram(file, format) when format not in ["svg", "mmd"] do
    IO.puts("Generated file: #{file}")
    IO.puts("To open automatically, use --format svg or --format mmd")
  end

  defp open_diagram(file, "svg") do
    case System.find_executable("xdg-open") do
      nil ->
        IO.puts("Could not find xdg-open. Please open manually: #{file}")

      _path ->
        System.cmd("xdg-open", [file])
        :ok
    end
  end

  defp open_diagram(file, "mmd") do
    case File.read(file) do
      {:ok, content} ->
        html = create_html_content(content)
        html_file = "erd_viewer.html"

        case File.write(html_file, html) do
          :ok ->
            open_html_file(html_file)

          {:error, error} ->
            IO.puts("Failed to write HTML file: #{inspect(error)}")
        end

      {:error, error} ->
        IO.puts("Failed to read diagram file: #{inspect(error)}")
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
    case System.find_executable("xdg-open") do
      nil ->
        IO.puts("Could not find xdg-open. Please open manually: #{html_file}")
        :manual_open_required

      _path ->
        System.cmd("xdg-open", [html_file])
        :opened
    end
  end
end
