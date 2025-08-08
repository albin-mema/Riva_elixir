defmodule RivaAsh.Accounts.RateLimiter do
  @moduledoc """
  Provides rate limiting functionality for authentication attempts.

  This module implements a sliding window rate limiting algorithm to prevent
  brute force attacks on authentication endpoints.
  """

  use GenServer

  # Configuration - moved to application config for flexibility
  @table_name :sign_in_attempts
  @default_max_attempts Application.compile_env(:riva_ash, :rate_limiter_max_attempts, 5)
  @default_window_seconds Application.compile_env(:riva_ash, :rate_limiter_window_seconds, 300)
  @cleanup_interval Application.compile_env(:riva_ash, :rate_limiter_cleanup_interval, 100)

  @type ip_address :: String.t() | tuple()
  @type result :: {:ok, :allowed} | {:error, :rate_limited}
  @type state :: map()

  # Client API

  @doc """
  Starts the rate limiter GenServer.

  ## Parameters
    - opts: Options for starting the server

  ## Returns
    - {:ok, pid}: Server started successfully
    - {:error, reason}: Failed to start server
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Checks if an IP address is allowed to make a sign-in attempt.

  ## Parameters
    - ip_address: The IP address to check

  ## Returns
    - {:ok, :allowed}: Attempt is allowed
    - {:error, :rate_limited}: Attempt is rate limited
  """
  @spec check_rate(ip_address) :: result
  def check_rate(ip_address) when is_binary(ip_address) or is_tuple(ip_address) do
    GenServer.call(__MODULE__, {:check_rate, ip_address})
  end

  def check_rate(_ip_address) do
    {:error, :invalid_ip_address}
  end

  @doc """
  Records a sign-in attempt for an IP address.

  ## Parameters
    - ip_address: The IP address to record
  """
  @spec record_attempt(ip_address) :: :ok
  def record_attempt(ip_address) when is_binary(ip_address) or is_tuple(ip_address) do
    GenServer.cast(__MODULE__, {:record_attempt, ip_address})
  end

  def record_attempt(_ip_address) do
    :ok
  end

  @doc """
  Resets the rate limit counter for an IP address.

  ## Parameters
    - ip_address: The IP address to reset
  """
  @spec reset_rate(ip_address) :: :ok
  def reset_rate(ip_address) when is_binary(ip_address) or is_tuple(ip_address) do
    GenServer.cast(__MODULE__, {:reset_rate, ip_address})
  end

  def reset_rate(_ip_address) do
    :ok
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    # Create ETS table for storing rate limit data
    :ets.new(@table_name, [:named_table, :public, :set, {:read_concurrency, true}])
    {:ok, %{}}
  end

  @impl true
  def handle_call({:check_rate, ip_address}, _from, state) do
    case get_attempts(ip_address) do
      attempts when attempts >= @default_max_attempts ->
        {:reply, {:error, :rate_limited}, state}

      _attempts_below_limit ->
        {:reply, {:ok, :allowed}, state}
    end
  end

  @impl true
  def handle_cast({:record_attempt, ip_address}, state) do
    current_time = System.system_time(:second)
    key = generate_key(ip_address, current_time)

    :ets.update_counter(@table_name, key, 1, {key, 0})

    # Clean up old entries periodically
    if should_cleanup?(current_time) do
      cleanup_old_entries(current_time)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:reset_rate, ip_address}, state) do
    current_time = System.system_time(:second)
    key = generate_key(ip_address, current_time)

    :ets.delete(@table_name, key)

    {:noreply, state}
  end

  # Private functions for single level of abstraction

  defp generate_key(ip_address, current_time) do
    {ip_address, div(current_time, @default_window_seconds)}
  end

  defp get_attempts(ip_address) do
    current_time = System.system_time(:second)
    key = generate_key(ip_address, current_time)

    case :ets.lookup(@table_name, key) do
      [{^key, attempts}] -> attempts
      [] -> 0
    end
  end

  defp should_cleanup?(_current_time) do
    rem(System.unique_integer(), @cleanup_interval) == 0
  end

  defp cleanup_old_entries(current_time) do
    current_window = div(current_time, @default_window_seconds)
    oldest_window = current_window - 2

    :ets.match_delete(@table_name, {{:"$1", :"$2"}, :_ets_value})
    |> Enum.filter(&should_delete_window?(&1, oldest_window))
    |> Enum.each(&delete_old_entry/1)
  end

  defp should_delete_window?([{_ip, window}], oldest_window) do
    window < oldest_window
  end

  defp delete_old_entry([{ip, window}]) do
    :ets.delete(@table_name, {ip, window})
  end
end
