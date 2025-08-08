alias Ecto.Changeset, as: Changeset
alias Phoenix.Controller, as: Controller
alias Ash.Error, as: Error
alias Ash.Error.Query, as: Query

defmodule RivaAshWeb.ErrorJSON do
  @moduledoc """
  Error JSON response handler for Riva Ash web interface.

  This module is invoked by your endpoint in case of errors on JSON requests.
  Provides standardized error response format for all JSON API errors.

  Implements functional programming patterns with proper error handling and
  type safety specifications.
  """

  @type template :: String.t()
  @type assigns :: map()
  @type error_response :: map()
  @type changeset :: Ecto.Changeset.t()
  @type resource :: String.t()
  @type id :: String.t() | nil

  @doc """
  Renders error response for JSON requests.

  ## Parameters
  - template: The error template name (e.g., "404.json", "500.json")
  - assigns: Additional assigns (currently unused)

  ## Returns
  - Map with standardized error format
  """
  @spec render(template(), assigns()) :: error_response()
  def render(template, _assigns) do
    %{errors: %{detail: Phoenix.Controller.status_message_from_template(template)}}
  end

  @doc """
  Renders validation errors for JSON requests.

  ## Parameters
  - changeset: Ecto.Changeset with validation errors

  ## Returns
  - Map with validation error details
  """
  @spec render_validation_errors(changeset()) :: error_response()
  def render_validation_errors(changeset) do
    %{errors: %{detail: translate_errors(changeset)}}
  end

  @doc """
  Renders authorization errors for JSON requests.

  ## Parameters
  - reason: Authorization error reason

  ## Returns
  - Map with authorization error details
  """
  @spec render_authorization_error(String.t()) :: error_response()
  def render_authorization_error(reason) do
    %{errors: %{detail: reason}}
  end

  @doc """
  Renders not found errors for JSON requests.

  ## Parameters
  - resource: Resource type that was not found
  - id: Resource ID that was not found (optional)

  ## Returns
  - Map with not found error details
  """
  @spec render_not_found(resource(), id()) :: error_response()
  def render_not_found(resource, id \\ nil) do
    detail = build_not_found_detail(resource, id)
    %{errors: %{detail: detail}}
  end

  @doc """
  Renders Ash-specific errors for JSON requests.

  ## Parameters
  - ash_error: Ash.Error struct with error details

  ## Returns
  - Map with Ash error details
  """
  @spec render_ash_error(struct()) :: error_response()
  def render_ash_error(%Ash.Error.Invalid{} = error) do
    %{errors: %{detail: format_ash_validation_errors(error)}}
  end

  def render_ash_error(%Ash.Error.Forbidden{}) do
    %{errors: %{detail: "Access forbidden"}}
  end

  def render_ash_error(%Ash.Error.Query.NotFound{}) do
    %{errors: %{detail: "Resource not found"}}
  end

  def render_ash_error(%Ash.Error.Unknown{} = error) do
    %{errors: %{detail: "Server error: #{error.message}"}}
  end

  def render_ash_error(error) do
    %{errors: %{detail: "Unexpected error: #{inspect(error)}"}}
  end

  # Private helper functions

  defp build_not_found_detail(resource, nil), do: "#{resource} not found"
  defp build_not_found_detail(resource, id), do: "#{resource} with ID #{id} not found"

  defp format_ash_validation_errors(%Ash.Error.Invalid{errors: errors}) do
    errors
    |> Enum.map(&format_ash_error/1)
    |> Enum.join(", ")
  end

  defp format_ash_error(%{field: field, message: message}) when is_atom(field) do
    "#{field} #{message}"
  end

  defp format_ash_error(%{message: message}), do: message
  defp format_ash_error(%{input: input}), do: "Invalid input: #{input}"
  defp format_ash_error(%{field: field}) when is_atom(field), do: "Invalid field: #{field}"
  defp format_ash_error(error) when is_binary(error), do: error
  defp format_unmatchedash_unmatchederror(_unmatched), do: "Validation failed"

  # Helper function to translate changeset errors
  @spec translate_errors(changeset()) :: list(String.t())
  defp translate_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, &translate_error_message/1)
    |> Map.values()
    |> List.flatten()
  end

  defp translate_error_message({msg, opts}) do
    Enum.reduce(opts, msg, fn {key, value}, acc ->
      String.replace(acc, "%{#{key}}", to_string(value))
    end)
  end
end
