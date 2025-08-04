defmodule RivaAsh.Test.JsonApiHelpers do
  @moduledoc """
  JSON:API testing helpers wrapping Plug/Phoenix Conn helpers.

  Usage examples:

      import Phoenix.ConnTest
      import RivaAsh.Test.JsonApiHelpers

      conn =
        build_conn()
        |> json_api_post(~p"/api/v1/businesses", %{data: %{type: "business", attributes: %{name: "Acme"}}})

      json = json_response(conn, 201)
      assert_pagination_links(json)
      assert_error_shape(%{"errors" => [%{"status" => _, "detail" => _}]})


  All requests automatically set:
  - content-type: application/vnd.api+json
  - accept: application/vnd.api+json
  """

  import Plug.Conn
  import ExUnit.Assertions

  @jsonapi "application/vnd.api+json"

  @type headers :: [{String.t(), String.t()}]
  @type body :: map()

  # --------------- HTTP wrappers ----------------

  defmacro json_api_get(conn, path, headers \\ []) do
    quote do
      unquote(conn)
      |> put_jsonapi_headers(unquote(headers))
      |> get(unquote(path))
    end
  end

  defmacro json_api_post(conn, path, body \\ quote(do: %{}), headers \\ []) do
    quote do
      unquote(conn)
      |> put_jsonapi_headers(unquote(headers))
      |> post(unquote(path), Jason.encode!(unquote(body)))
    end
  end

  defmacro json_api_patch(conn, path, body \\ quote(do: %{}), headers \\ []) do
    quote do
      unquote(conn)
      |> put_jsonapi_headers(unquote(headers))
      |> patch(unquote(path), Jason.encode!(unquote(body)))
    end
  end

  defmacro json_api_delete(conn, path, body \\ quote(do: %{}), headers \\ []) do
    quote do
      unquote(conn)
      |> put_jsonapi_headers(unquote(headers))
      |> delete(unquote(path), Jason.encode!(unquote(body)))
    end
  end

  # --------------- Assertions ----------------

  @doc """
  Assert the JSON payload conforms to minimal JSON:API error shape.
  """
  @spec assert_error_shape(map()) :: :ok
  def assert_error_shape(%{"errors" => errors}) when is_list(errors) and length(errors) > 0 do
    Enum.each(errors, fn err ->
      assert is_map(err)
      assert Map.has_key?(err, "status")
      assert Map.has_key?(err, "detail") or Map.has_key?(err, "title")
    end)

    :ok
  end

  def assert_error_shape(other) do
    flunk("Expected JSON:API error object with `errors: [...]`, got: #{inspect(other)}")
  end

  @doc """
  Assert top-level pagination links are present when using JSON:API pagination conventions.
  """
  @spec assert_pagination_links(map()) :: :ok
  def assert_pagination_links(%{"links" => links}) when is_map(links) do
    # presence check; not enforcing specific values
    assert Map.has_key?(links, "self")
    assert Map.has_key?(links, "first")
    assert Map.has_key?(links, "last")
    :ok
  end

  def assert_pagination_links(other) do
    flunk("Expected top-level `links` with pagination keys, got: #{inspect(other)}")
  end

  # --------------- Internal ----------------

  defp put_jsonapi_headers(conn, headers) do
    conn
    |> put_req_header("accept", @jsonapi)
    |> put_req_header("content-type", @jsonapi)
    |> put_additional_headers(headers)
  end

  defp put_additional_headers(conn, []), do: conn

  defp put_additional_headers(conn, headers) when is_list(headers) do
    Enum.reduce(headers, conn, fn {k, v}, acc -> put_req_header(acc, k, v) end)
  end
end
