alias Plug.Conn, as: Conn
alias Mix.Task, as: Task

defmodule RivaAshWeb.MermaidController do
  @moduledoc """
  Controller for generating and displaying Ash entity relationship diagrams.

  Provides endpoints to:
  - Generate Ash resource diagrams using Mermaid syntax
  - Display interactive ERD diagrams
  - Handle diagram generation errors gracefully

  Uses functional programming patterns with proper error handling and
  type safety specifications.
  """

  use RivaAshWeb, :controller
  alias RivaAsh.ErrorHelpers

  @type conn :: Plug.Conn.t()
  @type params :: map()
  @type result :: {:ok, String.t()} | {:error, String.t()}
  @type diagram_content :: String.t()

  @doc """
  Displays the Ash ERD diagram page.

  Generates and renders an interactive entity relationship diagram
  using Mermaid.js for visualization.

  ## Returns
    - 200: HTML page with interactive Mermaid diagram
    - 500: Error page if diagram generation fails
  """
  @spec show(conn(), params()) :: conn()
  def show(conn, _params) do
    case generate_mermaid_diagram() do
      {:ok, mermaid_code} ->
        render_diagram_page(conn, mermaid_code)

      {:error, reason} ->
        render_error_page(conn, reason)
    end
  end

  @doc """
  Downloads the Mermaid diagram as a file.

  Provides the raw Mermaid content for download or external use.

  ## Returns
    - 200: File download with .mmd extension
    - 500: Error response if generation fails
  """
  @spec download(conn(), params()) :: conn()
  def download(conn, _params) do
    case generate_mermaid_diagram() do
      {:ok, mermaid_code} ->
        conn
        |> put_resp_content_type("text/plain")
        |> put_resp_header("content-disposition", "attachment; filename=\"riva_ash_domain.mmd\"")
        |> send_resp(200, mermaid_code)

      {:error, reason} ->
        conn
        |> put_status(:internal_server_error)
        |> json(%{error: reason})
    end
  end

  # Private helper functions

  defp render_diagram_page(conn, mermaid_code) do
    html = build_diagram_html(mermaid_code)
    html(conn, html)
  end

  defp render_error_page(conn, reason) do
    html = build_error_html(reason)
    html(conn, html)
  end

  defp build_diagram_html(mermaid_code) do
    """
    <!DOCTYPE html>
    <html>
    <head>
      <title>Ash ERD Diagram - Riva Ash</title>
      <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
      <style>
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          margin: 0;
          padding: 20px;
          background-color: #f5f5f5;
        }
        .container {
          max-width: 1400px;
          margin: 0 auto;
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
          padding: 30px;
        }
        .header {
          text-align: center;
          margin-bottom: 30px;
          padding-bottom: 20px;
          border-bottom: 2px solid #e0e0e0;
        }
        .header h1 {
          color: #2c3e50;
          margin: 0 0 10px 0;
          font-size: 2.5em;
        }
        .header p {
          color: #7f8c8d;
          margin: 0;
          font-size: 1.1em;
        }
        .diagram-container {
          border: 2px solid #e0e0e0;
          border-radius: 8px;
          background: white;
          padding: 30px;
          margin-bottom: 30px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        .code-container {
          background: #f8f9fa;
          border: 1px solid #e9ecef;
          border-radius: 8px;
          padding: 20px;
          margin-bottom: 20px;
          overflow-x: auto;
        }
        .code-container h3 {
          margin-top: 0;
          color: #495057;
          border-bottom: 1px solid #dee2e6;
          padding-bottom: 10px;
        }
        pre {
          background: #f8f9fa;
          padding: 20px;
          border-radius: 6px;
          overflow-x: auto;
          margin: 0;
          font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
          font-size: 14px;
          line-height: 1.5;
        }
        .actions {
          text-align: center;
          margin-top: 30px;
        }
        .btn {
          display: inline-block;
          padding: 12px 24px;
          margin: 0 10px;
          background-color: #007bff;
          color: white;
          text-decoration: none;
          border-radius: 6px;
          border: none;
          cursor: pointer;
          font-size: 16px;
          transition: background-color 0.2s;
        }
        .btn:hover {
          background-color: #0056b3;
        }
        .btn-secondary {
          background-color: #6c757d;
        }
        .btn-secondary:hover {
          background-color: #545b62;
        }
        .error {
          color: #721c24;
          background-color: #f8d7da;
          border: 1px solid #f5c6cb;
          padding: 20px;
          border-radius: 6px;
          margin: 20px 0;
        }
        .loading {
          text-align: center;
          padding: 40px;
          color: #6c757d;
        }
        .loading::after {
          content: "...";
          animation: dots 1.5s steps(5, end) infinite;
        }
        @keyframes dots {
          0%, 20% { content: "."; }
          40% { content: ".."; }
          60% { content: "..."; }
          80%, 100% { content: ""; }
        }
        @media (max-width: 768px) {
          .container {
            padding: 15px;
          }
          .header h1 {
            font-size: 2em;
          }
          .btn {
            display: block;
            margin: 10px auto;
            width: 200px;
          }
        }
      </style>
    </head>
    <body>
      <div class="container">
        <div class="header">
          <h1>Ash Entity Relationship Diagram</h1>
          <p>Interactive visualization of Riva Ash domain resources and relationships</p>
        </div>

        <div id="diagram-content">
          <div class="loading">Generating diagram</div>
        </div>

        <div class="actions">
          <a href="/admin" class="btn">Back to Admin</a>
          <a href="/mermaid/download" class="btn btn-secondary">Download Diagram</a>
        </div>
      </div>

      <script>
        // Initialize Mermaid with enhanced configuration
        mermaid.initialize({
          startOnLoad: false,
          theme: 'default',
          themeVariables: {
            primaryColor: '#007bff',
            primaryTextColor: '#ffffff',
            primaryBorderColor: '#0056b3',
            lineColor: '#333333',
            secondaryColor: '#6c757d',
            tertiaryColor: '#28a745'
          },
          flowchart: {
            useMaxWidth: true,
            htmlLabels: true,
            curve: 'basis',
            nodeSpacing: 50,
            rankSpacing: 50
          },
          sequence: {
            actorMargin: 50,
            width: 150,
            height: 65,
            boxMargin: 10,
            boxTextMargin: 5,
            noteMargin: 10,
            messageMargin: 35
          }
        });

        // Generate diagram when page loads
        document.addEventListener('DOMContentLoaded', function() {
          const diagramCode = #{inspect(mermaid_code)};

          try {
            const element = document.createElement('div');
            element.className = 'diagram-container';
            element.innerHTML = '<div class="mermaid">' + diagramCode + '</div>';

            const container = document.getElementById('diagram-content');
            container.innerHTML = '';
            container.appendChild(element);

            // Render the diagram
            mermaid.init(undefined, element.querySelector('.mermaid'));
          } catch (error) {
            console.error('Error rendering diagram:', error);
            const container = document.getElementById('diagram-content');
            container.innerHTML = '<div class="error">Failed to render diagram: ' + error.message + '</div>';
          }
        });
      </script>
    </body>
    </html>
    """
  end

  defp build_error_html(reason) do
    """
    <!DOCTYPE html>
    <html>
    <head>
      <title>Diagram Generation Error - Riva Ash</title>
      <style>
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          margin: 0;
          padding: 20px;
          background-color: #f5f5f5;
        }
        .container {
          max-width: 800px;
          margin: 0 auto;
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
          padding: 40px;
          text-align: center;
        }
        .error-icon {
          font-size: 4em;
          color: #dc3545;
          margin-bottom: 20px;
        }
        h1 {
          color: #2c3e50;
          margin-bottom: 20px;
        }
        .error-message {
          background-color: #f8d7da;
          border: 1px solid #f5c6cb;
          color: #721c24;
          padding: 20px;
          border-radius: 6px;
          margin: 20px 0;
          font-family: monospace;
        }
        .btn {
          display: inline-block;
          padding: 12px 24px;
          margin: 10px;
          background-color: #007bff;
          color: white;
          text-decoration: none;
          border-radius: 6px;
          border: none;
          cursor: pointer;
          font-size: 16px;
        }
        .btn:hover {
          background-color: #0056b3;
        }
      </style>
    </head>
    <body>
      <div class="container">
        <div class="error-icon">⚠️</div>
        <h1>Diagram Generation Failed</h1>
        <p>We encountered an error while trying to generate the Ash entity relationship diagram.</p>

        <div class="error-message">
          <strong>Error:</strong> #{inspect(reason)}
        </div>

        <div>
          <a href="/admin" class="btn">Back to Admin</a>
          <a href="/mermaid" class="btn">Try Again</a>
        </div>
      </div>
    </body>
    </html>
    """
  end

  defp generate_mermaid_diagram do
    with {:ok, _unmatched} <- generate_diagram_files(),
         {:ok, content} <- read_generated_diagram() do
      {:ok, content}
    else
      {:error, reason} -> {:error, reason}
      _unmatchedunmatched -> {:error, "Could not generate or read the diagram file"}
    end
  end

  defp generate_diagram_files do
    case Mix.Task.run("ash.generate_resource_diagrams", ["--domain", "RivaAsh.Domain"]) do
      :ok -> {:ok, :generated}
      {:error, reason} -> {:error, reason}
      _unmatchedunmatched -> {:error, "Diagram generation task failed"}
    end
  rescue
    error -> {:error, "Exception during diagram generation: #{inspect(error)}"}
  end

  defp read_generated_diagram do
    path = "priv/static/ash_resource_diagrams/riva_ash_domain.mmd"

    case File.read(path) do
      {:ok, content} -> {:ok, content}
      {:error, :enoent} -> {:error, "Diagram file not found. Run mix ash.generate_resource_diagrams first."}
      {:error, reason} -> {:error, "Failed to read diagram file: #{inspect(reason)}"}
    end
  end
end
