defmodule RivaAshWeb.SwaggerController do
  @moduledoc """
  Controller for serving Swagger UI API documentation.

  Provides an interactive API documentation interface using Swagger UI
  to visualize and test the Riva Ash API endpoints.

  Features:
  - Interactive API documentation with Swagger UI
  - Deep linking for direct navigation to specific endpoints
  - Download URL functionality for API specifications
  - Responsive design for various screen sizes

  Uses functional programming patterns with proper error handling and
  type safety specifications.
  """

  use RivaAshWeb, :controller

  @type conn :: Plug.Conn.t()
  @type params :: map()
  @type result :: {:ok, String.t()} | {:error, String.t()}
  @type html_content :: String.t()

  @doc """
  Serves the Swagger UI documentation page.

  Renders an interactive API documentation interface that allows users
  to explore and test the Riva Ash API endpoints.

  ## Returns
    - 200: HTML page with Swagger UI interface
    - 500: Error response if documentation generation fails
  """
  @spec index(conn(), params()) :: conn()
  def index(conn, _params) do
    case generate_swagger_html() do
      {:ok, html} ->
        conn
        |> put_resp_content_type("text/html")
        |> send_resp(200, html)

      {:error, reason} ->
        conn
        |> put_status(:internal_server_error)
        |> put_resp_content_type("text/plain")
        |> send_resp(500, "Failed to generate API documentation: #{inspect(reason)}")
    end
  end

  # Private helper functions

  defp generate_swagger_html do
    html = build_swagger_html()
    {:ok, html}
  rescue
    error -> {:error, "Failed to generate Swagger HTML: #{inspect(error)}"}
  end

  defp build_swagger_html do
    """
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Riva Ash API Documentation</title>
        <link rel="stylesheet" type="text/css" href="https://unpkg.com/swagger-ui-dist@4.15.5/swagger-ui.css" />
        <style>
            html {
                box-sizing: border-box;
                overflow: -moz-scrollbars-vertical;
                overflow-y: scroll;
            }
            *, *:before, *:after {
                box-sizing: inherit;
            }
            body {
                margin: 0;
                background: #fafafa;
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
            }
            .custom-header {
                background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                color: white;
                padding: 20px 0;
                text-align: center;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
            .custom-header h1 {
                margin: 0;
                font-size: 2em;
                font-weight: 300;
            }
            .custom-header p {
                margin: 5px 0 0 0;
                opacity: 0.9;
                font-size: 1.1em;
            }
            .swagger-container {
                padding: 20px;
            }
            @media (max-width: 768px) {
                .custom-header h1 {
                    font-size: 1.5em;
                }
                .swagger-container {
                    padding: 10px;
                }
            }
        </style>
    </head>
    <body>
        <div class="custom-header">
            <h1>Riva Ash API Documentation</h1>
            <p>Interactive API reference for the Riva Ash platform</p>
        </div>

        <div class="swagger-container">
            <div id="swagger-ui"></div>
        </div>

        <script src="https://unpkg.com/swagger-ui-dist@4.15.5/swagger-ui-bundle.js"></script>
        <script src="https://unpkg.com/swagger-ui-dist@4.15.5/swagger-ui-standalone-preset.js"></script>
        <script>
            window.onload = function() {
                try {
                    const ui = SwaggerUIBundle({
                        url: '/api/open_api',
                        dom_id: '#swagger-ui',
                        deepLinking: true,
                        presets: [
                            SwaggerUIBundle.presets.apis,
                            SwaggerUIStandalonePreset
                        ],
                        plugins: [
                            SwaggerUIBundle.plugins.DownloadUrl
                        ],
                        layout: "StandaloneLayout",
                        validatorUrl: null,
                        oauth2RedirectUrl: window.location.origin + '/oauth2-redirect.html',
                        persistAuthorization: true,
                        displayRequestDuration: true,
                        displayRequestDuration: true,
                        docExpansion: 'list',
                        filter: true,
                        showExtensions: true,
                        showCommonExtensions: true
                    });

                    // Custom event listeners for enhanced user experience
                    ui.specActions.subscribe({
                        urlChange: (url) => {
                            console.log('Swagger UI URL changed:', url);
                        },
                        requestWillStart: (req) => {
                            console.log('API request starting:', req.url);
                        },
                        requestComplete: (req, resp) => {
                            console.log('API request completed:', req.url, resp.status);
                        }
                    });

                    // Add keyboard shortcuts
                    document.addEventListener('keydown', function(e) {
                        // Ctrl/Cmd + K to focus search
                        if ((e.ctrlKey || e.metaKey) && e.key === 'k') {
                            e.preventDefault();
                            const searchInput = document.querySelector('.swagger-ui .topbar .download-url-wrapper input');
                            if (searchInput) {
                                searchInput.focus();
                            }
                        }
                    });

                } catch (error) {
                    console.error('Error initializing Swagger UI:', error);
                    const container = document.getElementById('swagger-ui');
                    container.innerHTML = `
                        <div style="padding: 40px; text-align: center; color: #721c24; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;">
                            <h2>Error Loading API Documentation</h2>
                            <p>Failed to initialize Swagger UI: ${error.message}</p>
                            <p>Please try refreshing the page or contact support if the issue persists.</p>
                        </div>
                    `;
                }
            };
        </script>
    </body>
    </html>
    """
  end
end
