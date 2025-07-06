defmodule RivaAsh.ErrorHelpers do
  @moduledoc """
  Provides centralized error handling and formatting for the RivaAsh application.
  
  This module defines standard error structures and formatting for consistent
  error responses across the application.
  """
  
  @doc """
  Standardizes error responses from Ash changesets and other error sources.
  
  ## Examples
  
      iex> ErrorHelpers.format_error(%{errors: ["Invalid email format"]})
      %{code: :validation_failed, message: "Invalid email format"}
      
      iex> ErrorHelpers.format_error(%Ash.Error.Invalid{errors: [%{field: :email, message: "has invalid format"}]})
      %{code: :validation_failed, message: "email has invalid format"}
  """
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
  
  @doc """
  Handles errors in a consistent way, logging them and returning a standardized error map.
  """
  def handle_error(error) do
    # Log the full error for debugging
    require Logger
    Logger.error("Error in RivaAsh: #{inspect(error, pretty: true)}")
    
    # Format the error for the client
    format_error(error)
  end
  
  @doc """
  Wraps a function call with error handling.
  
  ## Examples
  
      iex> ErrorHelpers.with_error_handling(fn -> {:ok, :success} end)
      {:ok, :success}
      
      iex> ErrorHelpers.with_error_handling(fn -> {:error, "Something went wrong"} end)
      {:error, %{code: :error, message: "Something went wrong"}}
  """
  def with_error_handling(fun) do
    try do
      case fun.() do
        {:ok, result} -> 
          {:ok, result}
          
        {:error, error} -> 
          {:error, handle_error(error)}
          
        other -> 
          {:error, handle_error("Unexpected result: #{inspect(other)}")}
      end
    rescue
      e -> 
        {:error, handle_error(e)}
    end
  end
  
  @doc """
  Similar to `with_error_handling/1` but raises on error instead of returning a tuple.
  """
  def with_error_handling!(fun) do
    case with_error_handling(fun) do
      {:ok, result} -> result
      {:error, error} -> raise "Operation failed: #{error.message}"
    end
  end
end
