defmodule RivaAsh.Policies.AuthorizationContext do
  @moduledoc """
  Context struct and protocol for authorization operations.

  This module provides a structured approach to handling authorization context,
  ensuring type safety and consistent error handling across policy checks.
  """

  defstruct [:actor, :resource, :record, :action, :tenant]

  @type t :: %__MODULE__{
          actor: map() | nil,
          resource: module() | nil,
          record: map() | nil,
          action: atom() | nil,
          tenant: binary() | nil
        }

  @type context :: map()
  @type error_reason :: :invalid_actor | :invalid_context | :no_record | :no_permission

  @doc """
  Create a new authorization context from raw context data.
  """
  @spec from_context(context()) :: {:ok, t()} | {:error, error_reason()}
  def from_context(context) when is_map(context) do
    with :ok <- validate_context(context),
         actor <- Map.get(context, :actor),
         resource <- Map.get(context, :resource),
         record <- Map.get(context, :record),
         action <- Map.get(context, :action),
         tenant <- Map.get(context, :tenant) do
      {:ok,
       %__MODULE__{
         actor: actor,
         resource: resource,
         record: record,
         action: action,
         tenant: tenant
       }}
    else
      _ -> {:error, :invalid_context}
    end
  end

  def from_context(_context), do: {:error, :invalid_context}

  @doc """
  Validate that the context contains required fields.
  """
  @spec validate_context(context()) :: :ok | {:error, error_reason()}
  def validate_context(%{resource: _resource} = _context), do: :ok
  def validate_context(_context), do: {:error, :invalid_context}

  @doc """
  Check if the context contains a record.
  """
  @spec has_record?(t()) :: boolean()
  def has_record?(%__MODULE__{record: record}) when not is_nil(record), do: true
  def has_record?(_context), do: false

  @doc """
  Get the actor ID from the context.
  """
  @spec actor_id(t()) :: binary() | nil
  def actor_id(%__MODULE__{actor: %{id: actor_id}}) when is_binary(actor_id), do: actor_id
  def actor_id(_context), do: nil

  @doc """
  Get the record ID from the context.
  """
  @spec record_id(t()) :: binary() | nil
  def record_id(%__MODULE__{record: %{id: record_id}}) when is_binary(record_id), do: record_id
  def record_id(_context), do: nil
end
