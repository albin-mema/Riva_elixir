defmodule RivaAshWeb.FallbackController do
  use Phoenix.Controller

  def call(conn, {:error, %Ash.Error.Query.NotFound{}}) do
    conn
    |> put_status(:not_found)
    |> json(%{error: "Resource not found"})
  end

  def call(conn, {:error, %Ash.Error.Invalid{} = error}) do
    conn
    |> put_status(:unprocessable_entity)
    |> json(%{
      error: "Validation failed",
      details: format_changeset_errors(error)
    })
  end

  def call(conn, {:error, %Ash.Error.Forbidden{}}) do
    conn
    |> put_status(:forbidden)
    |> json(%{error: "Access forbidden"})
  end

  def call(conn, {:error, error}) when is_binary(error) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: error})
  end

  def call(conn, {:error, error}) do
    conn
    |> put_status(:internal_server_error)
    |> json(%{error: "Internal server error", details: inspect(error)})
  end

  defp format_changeset_errors(%Ash.Error.Invalid{errors: errors}) do
    Enum.map(errors, fn error ->
      %{
        field: error.field || "unknown",
        message: error.message || "is invalid"
      }
    end)
  end
end