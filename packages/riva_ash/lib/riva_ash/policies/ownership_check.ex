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

  @impl true
  def describe(opts) do
    field = opts[:field] || :employee_id
    "actor owns record via #{field}"
  end

  @impl true
  def match?(actor, context, opts) do
    field = opts[:field] || :employee_id
    
    case {actor, context} do
      # Actor must have an ID
      {%{id: actor_id}, %{resource: resource}} when is_binary(actor_id) ->
        # For queries, we can't easily check ownership without the record
        # The SAT solver will handle this via the authorize_if expression
        case Map.get(context, :record) do
          nil -> 
            # No record available, let the expression handle it
            true
          record ->
            # We have the record, check ownership directly
            Map.get(record, field) == actor_id
        end
      
      _ -> 
        false
    end
  end

  @doc """
  Check if actor owns the record via the specified field.
  
  ## Examples:
      authorize_if(OwnershipCheck.owns_record(:employee_id))
      authorize_if(OwnershipCheck.owns_record(:client_id))
  """
  def owns_record(field \\ :employee_id) do
    {__MODULE__, field: field}
  end

  @doc """
  Convenience function for employee ownership.
  """
  def owns_as_employee, do: owns_record(:employee_id)

  @doc """
  Convenience function for client ownership.
  """
  def owns_as_client, do: owns_record(:client_id)

  @doc """
  Convenience function for business ownership.
  """
  def owns_as_business, do: owns_record(:business_id)
end
