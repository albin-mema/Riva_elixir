defmodule RivaAshWeb.FallbackController do
  @moduledoc """
  Fallback controller for handling errors in Phoenix controllers.

  Provides centralized error handling for various types of errors including:
  - Ash resource errors (not found, validation, forbidden)
  - Generic errors with proper HTTP status codes
  - Structured error responses for API clients

  Uses functional programming patterns with proper error handling and
  type safety specifications.
  """

  use Phoenix.Controller

  @type conn :: Plug.Conn.t()
  @type error :: any()
  @type ash_error :: struct()

  @doc """
  Handles Ash query not found errors.
  """
  @spec call(conn(), {:error, struct()}) :: conn()
  def call(conn, {:error, %Ash.Error.Query.NotFound{}}) do
    conn
    |> put_status(:not_found)
    |> json(%{error: "Resource not found"})
  end

  @doc """
  Handles Ash validation errors.
  """
  @spec call(conn(), {:error, struct()}) :: conn()
  def call(conn, {:error, %Ash.Error.Invalid{} = error}) do
    conn
    |> put_status(:unprocessable_entity)
    |> json(%{
      error: "Validation failed",
      details: format_ash_validation_errors(error)
    })
  end

  @doc """
  Handles Ash authorization errors.
  """
  @spec call(conn(), {:error, struct()}) :: conn()
  def call(conn, {:error, %Ash.Error.Forbidden{}}) do
    conn
    |> put_status(:forbidden)
    |> json(%{error: "Access forbidden"})
  end

  @doc """
  Handles binary error messages.
  """
  @spec call(conn(), {:error, binary()}) :: conn()
  def call(conn, {:error, error}) when is_binary(error) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: error})
  end

  @doc """
  Handles generic errors.
  """
  @spec call(conn(), {:error, error()}) :: conn()
  def call(conn, {:error, error}) do
    conn
    |> put_status(:internal_server_error)
    |> json(%{error: "Internal server error", details: inspect(error)})
  end

  # Private helper functions

  defp format_ash_validation_errors(%Ash.Error.Invalid{errors: errors}) do
    Enum.reject(Enum.map(errors, &format_ash_error/1), &is_nil/1)
  end

  defp format_ash_error(%{field: field, message: message}) when is_atom(field) do
    %{field: to_string(field), message: message}
  end

  defp format_ash_error(%{message: message}) do
    %{field: "unknown", message: message}
  end

  defp format_ash_error(%{input: input}) do
    %{field: "input", message: "Invalid input: #{input}"}
  end

  defp format_ash_error(%{field: field}) when is_atom(field) do
    %{field: to_string(field), message: "is invalid"}
  end

  defp format_ash_error(error) when is_binary(error) do
    %{field: "unknown", message: error}
  end

  defp format_ash_error(_), do: nil
end
