defmodule RivaAsh.PropertyTesting.StateMachine do
  @moduledoc """
  State machine for modeling user navigation flows in property-based browser testing.

  This module defines the possible user states and valid transitions between them,
  enabling the generation of realistic user navigation sequences.
  """

  @type user_state :: :anonymous | :authenticated | :admin | :error
  @type action :: atom()
  @type transition :: {user_state(), action(), user_state()}

  @doc """
  All possible user states in the application.
  """
  def states do
    [:anonymous, :authenticated, :admin, :error]
  end

  @doc """
  Initial state for new user sessions.
  """
  def initial_state, do: :anonymous

  @doc """
  Valid transitions between states.

  Returns a list of {from_state, action, to_state} tuples.
  """
  def transitions do
    [
      # Anonymous user transitions
      {:anonymous, :register, :anonymous},
      {:anonymous, :login, :authenticated},
      {:anonymous, :admin_login, :admin},
      {:anonymous, :visit_public_page, :anonymous},
      {:anonymous, :visit_protected_page, :error},

      # Authenticated user transitions
      {:authenticated, :logout, :anonymous},
      {:authenticated, :visit_public_page, :authenticated},
      {:authenticated, :visit_protected_page, :authenticated},
      {:authenticated, :create_resource, :authenticated},
      {:authenticated, :update_resource, :authenticated},
      {:authenticated, :delete_resource, :authenticated},
      {:authenticated, :session_expire, :error},
      {:authenticated, :elevate_to_admin, :admin},

      # Admin user transitions
      {:admin, :logout, :anonymous},
      {:admin, :visit_public_page, :admin},
      {:admin, :visit_protected_page, :admin},
      {:admin, :visit_admin_page, :admin},
      {:admin, :create_resource, :admin},
      {:admin, :update_resource, :admin},
      {:admin, :delete_resource, :admin},
      {:admin, :session_expire, :error},
      {:admin, :demote_from_admin, :authenticated},

      # Error state transitions
      {:error, :login, :authenticated},
      {:error, :admin_login, :admin},
      {:error, :visit_public_page, :anonymous},
      {:error, :refresh_session, :authenticated},
      {:error, :clear_error, :anonymous}
    ]
  end

  @doc """
  Get valid actions for a given state.
  """
  def valid_actions(state) do
    transitions()
    |> Enum.filter(fn {from_state, _action, _to_state} -> from_state == state end)
    |> Enum.map(fn {_from_state, action, _to_state} -> action end)
    |> Enum.uniq()
  end

  @doc """
  Get the resulting state after performing an action from a given state.
  """
  def next_state(current_state, action) do
    case Enum.find(transitions(), fn {from, act, _to} ->
      from == current_state && act == action
    end) do
      {_from, _action, to_state} -> {:ok, to_state}
      nil -> {:error, :invalid_transition}
    end
  end

  @doc """
  Check if a transition is valid.
  """
  def valid_transition?(from_state, action, to_state) do
    {from_state, action, to_state} in transitions()
  end

  @doc """
  Get actions that are available based on current user permissions and state.
  """
  def available_actions(state, user_context \\ %{}) do
    base_actions = valid_actions(state)

    # Filter actions based on user context (permissions, business ownership, etc.)
    case state do
      :anonymous ->
        [:register, :login, :visit_public_page, :visit_protected_page]

      :authenticated ->
        actions = [:logout, :visit_public_page, :visit_protected_page,
                  :create_resource, :update_resource, :delete_resource]

        # Add admin elevation if user has admin permissions
        if Map.get(user_context, :can_be_admin, false) do
          [:elevate_to_admin | actions]
        else
          actions
        end

      :admin ->
        [:logout, :visit_public_page, :visit_protected_page, :visit_admin_page,
         :create_resource, :update_resource, :delete_resource, :demote_from_admin]

      :error ->
        [:login, :admin_login, :visit_public_page, :refresh_session, :clear_error]
    end
  end

  @doc """
  Get weighted probabilities for actions based on realistic user behavior.

  Returns a list of {action, weight} tuples where higher weights indicate
  more likely actions.
  """
  def action_weights(state) do
    case state do
      :anonymous ->
        [
          {:visit_public_page, 40},
          {:register, 25},
          {:login, 30},
          {:visit_protected_page, 5}  # Users sometimes try protected pages
        ]

      :authenticated ->
        [
          {:visit_protected_page, 35},
          {:create_resource, 20},
          {:update_resource, 15},
          {:visit_public_page, 15},
          {:delete_resource, 5},
          {:logout, 10}
        ]

      :admin ->
        [
          {:visit_admin_page, 30},
          {:visit_protected_page, 25},
          {:create_resource, 15},
          {:update_resource, 10},
          {:visit_public_page, 10},
          {:delete_resource, 5},
          {:logout, 5}
        ]

      :error ->
        [
          {:login, 40},
          {:visit_public_page, 30},
          {:refresh_session, 20},
          {:clear_error, 10}
        ]
    end
  end

  @doc """
  Generate a random action based on weighted probabilities.
  """
  def random_action(state, user_context \\ %{}) do
    available = available_actions(state, user_context)
    weights = action_weights(state)

    # Filter weights to only include available actions
    filtered_weights = Enum.filter(weights, fn {action, _weight} ->
      action in available
    end)

    # Select random action based on weights
    total_weight = Enum.sum(Enum.map(filtered_weights, fn {_action, weight} -> weight end))
    random_value = :rand.uniform(total_weight)

    select_weighted_action(filtered_weights, random_value, 0)
  end

  defp select_weighted_action([{action, weight} | _rest], random_value, acc)
       when random_value <= acc + weight do
    action
  end

  defp select_weighted_action([{_action, weight} | rest], random_value, acc) do
    select_weighted_action(rest, random_value, acc + weight)
  end

  defp select_weighted_action([], _random_value, _acc) do
    # Fallback to first available action if something goes wrong
    :visit_public_page
  end

  @doc """
  Check if a state represents an error condition.
  """
  def error_state?(:error), do: true
  def error_state?(_), do: false

  @doc """
  Check if a state allows access to protected resources.
  """
  def authenticated_state?(:authenticated), do: true
  def authenticated_state?(:admin), do: true
  def authenticated_state?(_), do: false

  @doc """
  Check if a state allows admin actions.
  """
  def admin_state?(:admin), do: true
  def admin_state?(_), do: false

  @doc """
  Get recovery actions for error states.
  """
  def recovery_actions(:error) do
    [:login, :visit_public_page, :refresh_session, :clear_error]
  end

  def recovery_actions(_state), do: []

  @doc """
  Validate a sequence of state transitions.
  """
  def validate_flow(flow) do
    validate_flow_recursive(flow, initial_state(), [])
  end

  defp validate_flow_recursive([], _current_state, acc) do
    {:ok, Enum.reverse(acc)}
  end

  defp validate_flow_recursive([action | rest], current_state, acc) do
    case next_state(current_state, action) do
      {:ok, next_state} ->
        validate_flow_recursive(rest, next_state, [{current_state, action, next_state} | acc])
      {:error, reason} ->
        {:error, {reason, current_state, action}}
    end
  end
end
