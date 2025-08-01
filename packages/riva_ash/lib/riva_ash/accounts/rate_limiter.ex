defmodule RivaAsh.Accounts.RateLimiter do
  @moduledoc """
  Provides rate limiting functionality for authentication attempts.
  """

  use GenServer

  @table_name :sign_in_attempts
  @max_attempts 5
  # 5 minutes
  @window_seconds 300

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc """
  Checks if an IP address is allowed to make a sign-in attempt.
  Returns {:ok, :allowed} if the attempt is allowed, {:error, :rate_limited} otherwise.
  """
  def check_rate(ip_address) do
    GenServer.call(__MODULE__, {:check_rate, ip_address})
  end

  @doc """
  Records a sign-in attempt for an IP address.
  """
  def record_attempt(ip_address) do
    GenServer.cast(__MODULE__, {:record_attempt, ip_address})
  end

  @doc """
  Resets the rate limit counter for an IP address.
  """
  def reset_rate(ip_address) do
    GenServer.cast(__MODULE__, {:reset_rate, ip_address})
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    # Create ETS table for storing rate limit data
    :ets.new(@table_name, [:named_table, :public, :set])
    {:ok, %{}}
  end

  @impl true
  def handle_call({:check_rate, ip_address}, _from, state) do
    case get_attempts(ip_address) do
      attempts when attempts >= @max_attempts ->
        {:reply, {:error, :rate_limited}, state}

      _ ->
        {:reply, {:ok, :allowed}, state}
    end
  end

  @impl true
  def handle_cast({:record_attempt, ip_address}, state) do
    current_time = System.system_time(:second)
    key = {ip_address, div(current_time, @window_seconds)}

    :ets.update_counter(@table_name, key, 1, {key, 0})

    # Clean up old entries periodically
    if rem(System.unique_integer(), 100) == 0 do
      cleanup_old_entries(current_time)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:reset_rate, ip_address}, state) do
    current_time = System.system_time(:second)
    key = {ip_address, div(current_time, @window_seconds)}

    :ets.delete(@table_name, key)

    {:noreply, state}
  end

  # Private functions

  defp get_attempts(ip_address) do
    current_time = System.system_time(:second)
    key = {ip_address, div(current_time, @window_seconds)}

    case :ets.lookup(@table_name, key) do
      [{^key, attempts}] -> attempts
      [] -> 0
    end
  end

  defp cleanup_old_entries(current_time) do
    current_window = div(current_time, @window_seconds)
    # Keep data for at least 2 windows
    oldest_window = current_window - 2

    :ets.match_delete(@table_name, {{:"$1", :"$2"}, :_})
    |> Enum.filter(fn [{_ip, window}] -> window < oldest_window end)
    |> Enum.each(fn [{ip, window}] ->
      :ets.delete(@table_name, {ip, window})
    end)
  end
end
