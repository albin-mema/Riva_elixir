defmodule RivaAshWeb.SwaggerController do
  use Phoenix.Controller, formats: [:html]
  alias RivaAsh.ErrorHelpers

  def index(conn, _params) do
    with {:ok, html} <- generate_swagger_html(),
         {:ok, response_conn} <- conn
                                 |> put_resp_content_type("text/html")
                                 |> send_resp(200, html)
                                 |> ErrorHelpers.to_result() do
      response_conn
    else
      {:error, error} ->
        conn
        |> put_resp_content_type("text/plain")
        |> send_resp(500, "Failed to generate API documentation: #{inspect(error)}")
    end
  end

  defp generate_swagger_html do
    html = """
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
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
                margin:0;
                background: #fafafa;
            }
        </style>
    </head>
    <body>
        <div id="swagger-ui"></div>

        <script src="https://unpkg.com/swagger-ui-dist@4.15.5/swagger-ui-bundle.js"></script>
        <script src="https://unpkg.com/swagger-ui-dist@4.15.5/swagger-ui-standalone-preset.js"></script>
        <script>
            window.onload = function() {
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
                    layout: "StandaloneLayout"
                });
            };
        </script>
    </body>
    </html>
    """

    ErrorHelpers.success(html)
  end
end
