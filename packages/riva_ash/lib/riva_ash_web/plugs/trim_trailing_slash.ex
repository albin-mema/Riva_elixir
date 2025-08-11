defmodule RivaAshWeb.Plugs.TrimTrailingSlash do
  @moduledoc """
  Plug that removes trailing slashes from URLs and redirects to the canonical version.

  This ensures consistent URL structure without trailing slashes, which is important for:
  - SEO
  - Cache consistency
  - Avoiding duplicate content issues
  - Meeting the project's URL convention requirements

  The plug will:
  - Check if the request path ends with a slash (and isn't just "/")
  - Redirect to the same path without the trailing slash with 301 status
  - Preserve query parameters during redirect
  """

  import Plug.Conn
  import Phoenix.Controller

  @spec init(any) :: any
  def init(options), do: options

  @spec call(Plug.Conn.t(), any) :: Plug.Conn.t()
  def call(%{request_path: path} = conn, _opts) do
    # Check if path ends with slash and isn't just "/"
    if String.ends_with?(path, "/") and path != "/" do
      # Remove trailing slash
      new_path = String.trim_trailing(path, "/")

      # Preserve query string
      query_string = conn.query_string
      redirect_path = if query_string != "", do: "#{new_path}?#{query_string}", else: new_path

      conn
      |> put_status(301)
      |> redirect(to: redirect_path)
      |> halt()
    else
      conn
    end
  end
end
