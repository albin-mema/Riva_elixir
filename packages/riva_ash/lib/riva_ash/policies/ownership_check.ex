defmodule RivaAsh.Policies.OwnershipCheck do
  @moduledoc """
  Custom Ash policy check for ownership-based authorization.

  This check works with Ash's SAT solver to efficiently evaluate ownership-based
  authorization rules. It can be combined with permission checks to create
  sophisticated authorization policies.

  ## Usage in policies:

      policies do
        # Allow viewing own reservations OR having view_all_reservations permission
        policy action_type(:read) do
          authorize_if(OwnershipCheck.owns_record(:employee_id))
          authorize_if(PermissionCheck.can_view_all_reservations())
        end
      end
  """

  use Ash.Policy.SimpleCheck

  require Logger
  alias RivaAsh.Policies.AuthorizationContext

  @type actor :: map()
  @type context :: map()
  @type opts :: keyword()
  @type field :: atom()
  @type check_result :: boolean()

  @default_field :employee_id
  @config_key :riva_ash

  @impl true
  @spec describe(opts()) :: String.t()
  def describe(opts) do
    field = opts[:field] || default_field()
    "actor owns record via #{field}"
  end

  @impl true
  @spec match?(actor(), context(), opts()) :: check_result()
  def match?(actor, context, opts) do
    Logger.debug("Starting ownership check for actor: #{inspect(actor)}")

    with :ok <- validate_actor(actor),
         :ok <- validate_context(context),
         field <- extract_field(opts),
         {:ok, record} <- extract_record(context) do
      result = check_ownership(actor, record, field)
      Logger.debug("Ownership check result: #{result}")
      result
    else
      :error ->
        Logger.debug("Ownership check failed during validation")
        false
    end
  end

  @doc """
  Check if actor owns the record via the specified field.

  ## Examples:
      authorize_if(OwnershipCheck.owns_record(:employee_id))
      authorize_if(OwnershipCheck.owns_record(:client_id))
  """
  @spec owns_record(field()) :: {__MODULE__, field: field()}
  def owns_record(field \\ default_field()) do
    {__MODULE__, field: field}
  end

  @doc """
  Convenience function for employee ownership.
  """
  @spec owns_as_employee() :: {__MODULE__, field: field()}
  def owns_as_employee, do: owns_record(:employee_id)

  @doc """
  Convenience function for client ownership.
  """
  @spec owns_as_client() :: {__MODULE__, field: field()}
  def owns_as_client, do: owns_record(:client_id)

  @doc """
  Convenience function for business ownership.
  """
  @spec owns_as_business() :: {__MODULE__, field: field()}
  def owns_as_business, do: owns_record(:business_id)

  # Private helper functions with single level of abstraction

  @spec default_field() :: field()
  defp default_field do
    Application.get_env(@config_key, :default_ownership_field, @default_field)
  end

  @spec validate_actor(actor()) :: :ok | :error
  defp validate_actor(%{id: actor_id} = _actor) when is_binary(actor_id) do
    Logger.debug("Validating actor with ID: #{actor_id}")
    :ok
  end

  defp validate_actor(_actor) do
    Logger.debug("Actor validation failed: missing or invalid ID")
    :error
  end

  @spec validate_context(context()) :: :ok | :error
  defp validate_context(%{resource: _resource} = _context) do
    Logger.debug("Context validation passed: resource present")
    :ok
  end

  defp validate_context(_context) do
    Logger.debug("Context validation failed: missing resource")
    :error
  end

  @spec extract_field(opts()) :: field()
  defp extract_field(opts) do
    Keyword.get(opts, :field, default_field())
  end

  @spec extract_record(context()) :: {:ok, map()} | {:error, :no_record}
  defp extract_record(context) do
    case Map.get(context, :record) do
      nil -> {:error, :no_record}
      record -> {:ok, record}
    end
  end

  @spec check_ownership(actor(), map(), field()) :: boolean()
  defp check_ownership(%{id: actor_id}, record, field) when is_binary(actor_id) do
    Logger.debug("Checking ownership for actor ID: #{actor_id}, field: #{field}")

    record
    |> Map.get(field)
    |> match_actor_id?(actor_id)
  end

  defp check_ownership(_actor, _record, _field) do
    Logger.debug("Ownership check failed: invalid actor or record")
    false
  end

  @spec match_actor_id?(term(), binary()) :: boolean()
  defp match_actor_id?(record_id, actor_id) when is_binary(record_id) do
    result = record_id == actor_id
    Logger.debug("Actor ID match check: #{record_id} == #{actor_id} -> #{result}")
    result
  end

  defp match_actor_id?(_record_id, _actor_id) do
    Logger.debug("Actor ID match check failed: non-binary IDs")
    false
  end
end
