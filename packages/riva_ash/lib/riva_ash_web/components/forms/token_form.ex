defmodule RivaAshWeb.Components.Forms.TokenForm do
  @moduledoc """
  Form component for creating and editing API tokens using atomic design system.

  This component follows the functional core, imperative shell pattern,
  with pure functions for data transformation and validation, and
  the LiveView component handling UI state and side effects.

  ## Styleguide Compliance

  This module follows the Riva Ash styleguide principles:

  - **Functional Programming**: Uses pure functions, pattern matching, and pipelines
  - **Type Safety**: Comprehensive type specifications with @spec annotations
  - **Single Level of Abstraction**: Each function has a clear, focused responsibility
  - **Error Handling**: Consistent use of result tuples and guard clauses
  - **Immutable Data**: All transformations use immutable data structures
  - **Security**: Proper input validation and safe rendering patterns
  - **Phoenix/Ash Integration**: Follows Phoenix LiveView and Ash framework patterns
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Molecules.NotificationToast
  import RivaAshWeb.Components.Atoms.Button

  @type assigns :: %{
          optional(:form) => map(),
          optional(:editing) => boolean(),
          optional(:loading) => boolean(),
          optional(:on_submit) => String.t(),
          optional(:on_change) => String.t(),
          optional(:on_cancel) => String.t(),
          optional(:users) => list(),
          optional(:rest) => any()
        }

  @type token_form_data :: %{
          user_id: String.t() | integer(),
          purpose: String.t(),
          expires_at: DateTime.t() | nil
        }

  @doc """
  Renders a form for creating or editing tokens.

  ## Examples
      <.token_form
        form={@form}
        editing={@editing}
        loading={@loading}
        on_submit="save_token"
        on_change="validate_token"
        on_cancel="cancel_token"
        users={@users}
      />
  """
  attr(:form, :any, required: true)
  attr(:editing, :boolean, default: false)
  attr(:loading, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:users, :list, default: [])
  attr(:rest, :global)

  @spec token_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def token_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_token_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:editing, false)
    |> Map.put_new(:loading, false)
    |> Map.put_new(:users, [])
  end

  @spec render_token_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_token_form(assigns) do
    ~H"""
    <.card variant="elevated" {@rest}>
      <:header>
        <.card_title><%= if @editing, do: "Edit Token", else: "Create New Token" %></.card_title>
      </:header>
      <:body>
        <.form for={@form} phx-change={@on_change} phx-submit={@on_submit} phx-reset={@on_cancel} class="space-y-6">
          <.render_error_toast form={@form} />
          <.render_form_fields form={@form} users={@users} />
          <.render_form_actions loading={@loading} on_cancel={@on_cancel} editing={@editing} />
        </.form>
      </:body>
    </.card>
    """
  end

  @spec render_error_toast(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_error_toast(assigns) do
    ~H"""
    <.notification_toast
      :if={@form.errors != []}
      type="error"
      message={format_errors(@form.errors)}
      show={@form.errors != []}
      aria-live="assertive"
      aria-atomic="true"
    />
    """
  end

  @spec render_form_fields(form :: map(), users :: list()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_fields(assigns) do
    ~H"""
    <.select_field
      field={@form[:user_id]}
      label="User"
      options={user_options(@users)}
      prompt="Select a user"
      helper_text="Select the user this token belongs to."
    />

    <.form_field
      field={@form[:purpose]}
      label="Purpose"
      type="text"
      placeholder="API access, integration, etc."
      helper_text="Describe the purpose of this token."
    />

    <.form_field
      field={@form[:expires_at]}
      label="Expires At"
      type="datetime-local"
      helper_text="Set when this token should expire."
    />
    """
  end

  @spec render_form_actions(loading :: boolean(), on_cancel :: String.t(), editing :: boolean()) ::
          Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div class="flex justify-end space-x-3 pt-4 border-t">
      <.button type="button" variant="outline" phx-click={@on_cancel} disabled={@loading}>
        Cancel
      </.button>
      <.button type="submit" loading={@loading}>
        <%= if @editing, do: "Update Token", else: "Create Token" %>
      </.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates token form data.

  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_token_data(map()) :: {:ok, token_form_data()} | {:error, map()}
  def validate_token_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_user_exists(params[:user_id], @users),
         :ok <- validate_expiration_date(params[:expires_at]) do
      {:ok, transform_token_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:user_id, :purpose]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_user_exists(nil, _), do: {:error, %{user_id: "user is required"}}

  defp validate_user_exists(user_id, users) when is_list(users) do
    case Enum.find(users, fn user -> user.id == user_id end) do
      %{id: ^user_id} -> :ok
      _ -> {:error, %{user_id: "user not found"}}
    end
  end

  defp validate_expiration_date(nil), do: :ok

  defp validate_expiration_date(expires_at) when is_binary(expires_at) do
    case DateTime.from_iso8601(expires_at <> ":00") do
      {:ok, datetime, _} ->
        if DateTime.compare(datetime, DateTime.utc_now()) == :gt do
          :ok
        else
          {:error, %{expires_at: "expiration date must be in the future"}}
        end

      {:error, _} ->
        {:error, %{expires_at: "invalid date format"}}
    end
  end

  defp validate_expiration_date(_), do: :ok

  @doc """
  Transforms raw token data into structured format.
  """
  @spec transform_token_data(map()) :: token_form_data()
  def transform_token_data(params) do
    %{
      user_id: parse_id(params[:user_id]),
      purpose: Map.get(params, :purpose, "") |> String.trim(),
      expires_at: parse_datetime(params[:expires_at])
    }
  end

  defp parse_id(nil), do: nil
  defp parse_id(value) when is_binary(value), do: Integer.parse(value) |> elem(0)
  defp parse_id(value) when is_integer(value), do: value
  defp parse_id(_), do: nil

  defp parse_datetime(nil), do: nil

  defp parse_datetime(datetime_str) when is_binary(datetime_str) do
    case DateTime.from_iso8601(datetime_str <> ":00") do
      {:ok, datetime, _} -> datetime
      {:error, _} -> nil
    end
  end

  defp parse_datetime(_), do: nil

  # UI Helper functions

  @spec user_options(list()) :: list({String.t(), integer()})
  defp user_options(users) when is_list(users) do
    Enum.map(users, fn user ->
      {user.email, user.id}
    end)
  end

  # Error formatting function

  @spec format_errors(map()) :: String.t()
  defp format_errors(errors) do
    errors
    |> Enum.map(fn {field, {message, _}} -> "#{field}: #{message}" end)
    |> Enum.join(", ")
  end
end
