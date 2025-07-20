defmodule RivaAshWeb.MermaidController do
  use RivaAshWeb, :controller
  alias RivaAsh.ErrorHelpers

  def show(conn, _params) do
    # Generate the Mermaid diagram using Ash's built-in functionality
    mermaid_code = generate_mermaid_diagram()

    html = """
    <!DOCTYPE html>
    <html>
    <head>
      <title>Ash ERD Diagram</title>
      <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
      <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .mermaid { border: 1px solid #ddd; padding: 20px; border-radius: 5px; background: white; }
        pre { background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; }
        .container { max-width: 1200px; margin: 0 auto; }
        h1 { color: #333; }
        .error { color: #d32f2f; background-color: #ffebee; padding: 15px; border-radius: 4px; }
      </style>
    </head>
    <body>
      <div class="container">
        <h1>Ash Entity Relationship Diagram</h1>

        <h2>Interactive Diagram</h2>
        <div class="mermaid">
          #{mermaid_code}
        </div>

        <h2>Mermaid Code</h2>
        <pre><code>#{mermaid_code}</code></pre>

        <p>
          <a href="/admin" class="btn">Back to Admin</a>
        </p>
      </div>

      <script>
        mermaid.initialize({
          startOnLoad: true,
          theme: 'default',
          flowchart: {
            useMaxWidth: true,
            htmlLabels: true,
            curve: 'basis'
          }
        });
      </script>
    </body>
    </html>
    """

    html(conn, html)
  end

  defp generate_mermaid_diagram do
    # Generate Mermaid diagram using Ash's built-in functionality
    with {:ok, _} <- Mix.Task.run("ash.generate_resource_diagrams", ["--domain", "RivaAsh.Domain"]) |> ErrorHelpers.to_result(),
         {:ok, content} <- read_generated_diagram() do
      ErrorHelpers.success(content)
    else
      _ -> ErrorHelpers.failure("%% Error: Could not generate or read the diagram file")
    end
  end

  defp read_generated_diagram do
    # The diagram is typically generated in priv/static/ash_resource_diagrams/domain_name.mmd
    path = "priv/static/ash_resource_diagrams/riva_ash_domain.mmd"

    File.read(path)
    |> case do
      {:ok, content} -> ErrorHelpers.success(content)
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end
end
