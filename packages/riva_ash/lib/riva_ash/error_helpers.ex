defmodule RivaAsh.ErrorHelpers do
  @moduledoc """
  Provides centralized error handling and formatting for the RivaAsh application.

  This module defines standard error structures and formatting for consistent
  error responses across the application.
  """

  @doc """
  Wraps a value in an {:ok, value} tuple.
  """
  @spec success(any()) :: {:ok, any()}
  def success(value), do: {:ok, value}

  @doc """
  Wraps a reason in an {:error, reason} tuple.
  """
  @spec failure(any()) :: {:error, any()}
  def failure(reason), do: {:error, reason}

  @doc """
  Standardizes error responses using native Elixir error handling.

  ## Examples

      iex> RivaAsh.ErrorHelpers.format_error(%{errors: ["Invalid email format"]})
      %{code: :validation_failed, message: "Invalid email format"}

      iex> RivaAsh.ErrorHelpers.format_error(%Ash.Error.Invalid{errors: [%{field: :email, message: "has invalid format"}]})
      %{code: :validation_failed, message: "email has invalid format"}
  """
  @spec format_error(map() | Ash.Error.t() | String.t()) :: map() | String.t()
  def format_error(%{errors: errors}) when is_list(errors) do
    %{
      code: :validation_failed,
      message: Enum.map_join(errors, ", ", &format_error/1)
    }
  end

  def format_error(%Ash.Error.Invalid{errors: errors}) do
    %{
      code: :validation_failed,
      errors: Enum.map(errors, &format_error/1),
      message: Enum.map_join(errors, ", ", &format_error/1)
    }
  end

  def format_error(%Ash.Error.Forbidden{}) do
    %{
      code: :forbidden,
      message: "You are not authorized to perform this action"
    }
  end

  def format_error(%Ash.Error.Changes.Required{field: field}) do
    %{
      code: :validation_failed,
      message: "#{field} is required"
    }
  end

  def format_error(%{field: field, message: message}) do
    "#{field} #{message}"
  end

  def format_error(error) when is_binary(error) do
    %{
      code: :error,
      message: error
    }
  end

  def format_error(error) do
    %{
      code: :error,
      message: "An unexpected error occurred: #{inspect(error)}"
    }
  end

  # Implement String.Chars protocol for error maps
  defimpl String.Chars, for: Map do
    def to_string(%{code: _code, message: message}) when is_binary(message) do
      message
    end
    def to_string(map) do
      "#{inspect(map)}"
    end
  end

  @doc """
  Handles errors in a consistent way, logging them and returning a standardized error map.
  """
  @spec handle_error(any()) :: map()
  def handle_error(error) do
    # Log the full error for debugging
    require Logger
    log_error(error)

    # Format the error for the client
    format_error(error)
  end

  @spec log_error(any()) :: :ok
  defp log_error(error) do
    Logger.error("Error in RivaAsh: #{inspect(error, pretty: true)}")
  end

  @doc """
  Wraps a function call with error handling.

  ## Examples

      iex> RivaAsh.ErrorHelpers.with_error_handling(fn -> {:ok, :success} end)
      {:ok, :success}

      iex> RivaAsh.ErrorHelpers.with_error_handling(fn -> {:error, "Something went wrong"} end)
      {:error, %{code: :error, message: "Something went wrong"}}
  """
  @spec with_error_handling((-> any())) :: {:ok, any()} | {:error, map()}
  def with_error_handling(fun) when is_function(fun, 0) do
    try do
      case fun.() do
        {:ok, result} ->
          success(result)

        {:error, error} ->
          failure(handle_error(error))

        other ->
          handle_unexpected_result(other)
      end
    rescue
      e ->
        failure(handle_error(e))
    end
  end

  @spec handle_unexpected_result(any()) :: {:error, map()}
  defp handle_unexpected_result(other) do
    error_message = "Unexpected result: #{inspect(other)}"
    failure(handle_error(error_message))
  end

  @doc """
  Similar to `with_error_handling/1` but raises on error instead of returning a tuple.
  """
  @spec with_error_handling!((-> any())) :: any()
  def with_error_handling!(fun) when is_function(fun, 0) do
    case with_error_handling(fun) do
      {:ok, result} -> result
      {:error, error} -> raise "Operation failed: #{error.message}"
    end
  end

  @doc """
  Converts any value into an {:ok, value} tuple, or {:error, value} if it's already an error tuple.
  """
  @spec to_result({:ok, any()} | {:error, any()} | any()) :: {:ok, any()} | {:error, any()}
  def to_result({:ok, value}), do: {:ok, value}
  def to_result({:error, error}), do: {:error, error}
  def to_result(value), do: {:ok, value}

  @doc """
  Ensures a value is present, returning {:ok, value} or {:error, reason}.
  """
  @spec required(any(), any()) :: {:ok, any()} | {:error, any()}
  def required(value, error_reason) do
    case is_nil(value) do
      true -> failure(error_reason)
      false -> success(value)
    end
  end
end
