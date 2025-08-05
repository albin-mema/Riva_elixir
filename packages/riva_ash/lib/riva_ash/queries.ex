defmodule RivaAsh.Queries do
  @moduledoc """
  Optimized query patterns and helper functions for common operations.
  These functions provide efficient ways to query the most common data patterns.
  """

  import Ash.Expr
  require Ash.Query

  alias RivaAsh.Resources.{Item, Reservation}

  @doc """
  Efficiently check item availability for a given date range.
  Uses optimized indexes and minimal data loading.
  """
  @spec check_item_availability(String.t(), DateTime.t(), DateTime.t()) ::
        {:ok, :available} | {:error, any()}
  def check_item_availability(item_id, start_datetime, end_datetime) do
    RivaAsh.Validations.check_item_availability(item_id, start_datetime, end_datetime)
  end

  @doc """
  Get all available items for a business on a specific date.
  """
  @spec available_items_for_business_and_date(String.t(), Date.t()) ::
        {:ok, list(Item.t())} | {:error, any()}
  def available_items_for_business_and_date(business_id, date)
      when is_binary(business_id) and is_struct(date, Date) do
    with {:ok, start_datetime} <- create_start_datetime(date),
         {:ok, end_datetime} <- create_end_datetime(date),
         {:ok, items} <- get_business_items(business_id),
         {:ok, available_items} <- filter_available_items(items, start_datetime, end_datetime) do
      {:ok, available_items}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to get available items: #{inspect(error)}"}
    end
  end

  @spec create_start_datetime(Date.t()) :: {:ok, DateTime.t()} | {:error, any()}
  defp create_start_datetime(date) do
    case DateTime.new(date, ~T[00:00:00], "Etc/UTC") do
      {:ok, datetime} -> {:ok, datetime}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec create_end_datetime(Date.t()) :: {:ok, DateTime.t()} | {:error, any()}
  defp create_end_datetime(date) do
    case DateTime.new(date, ~T[23:59:59], "Etc/UTC") do
      {:ok, datetime} -> {:ok, datetime}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec get_business_items(String.t()) :: {:ok, list(Item.t())} | {:error, any()}
  defp get_business_items(business_id) do
    Item
    |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
    |> Ash.Query.filter(expr(is_active == true))
    |> Ash.Query.filter(expr(is_nil(archived_at)))
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @spec filter_available_items(list(Item.t()), DateTime.t(), DateTime.t()) ::
        {:ok, list(Item.t())} | {:error, any()}
  defp filter_available_items(items, start_datetime, end_datetime) do
    available_items =
      Enum.filter(items, fn item ->
        case check_item_availability(item.id, start_datetime, end_datetime) do
          {:ok, :available} -> true
          _ -> false
        end
      end)

    {:ok, available_items}
  end

  @doc """
  Get upcoming reservations for a business.
  """
  @spec upcoming_reservations_for_business(String.t(), non_neg_integer()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  def upcoming_reservations_for_business(business_id, limit \\ 10)
      when is_binary(business_id) and is_integer(limit) and limit >= 0 do
    with {:ok, now} <- get_current_time(),
         {:ok, reservations} <- get_upcoming_reservations(business_id, now, limit) do
      {:ok, reservations}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to get upcoming reservations: #{inspect(error)}"}
    end
  end

  @spec get_current_time() :: {:ok, DateTime.t()} | {:error, any()}
  defp get_current_time() do
    case Timex.now() do
      %DateTime{} = datetime -> {:ok, datetime}
      error -> {:error, error}
    end
  end

  @spec get_upcoming_reservations(String.t(), DateTime.t(), non_neg_integer()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  defp get_upcoming_reservations(business_id, now, limit) do
    Reservation
    |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
    |> Ash.Query.filter(expr(reserved_from > ^now))
    |> Ash.Query.filter(expr(status in [:confirmed, :pending]))
    |> Ash.Query.sort(reserved_from: :asc)
    |> Ash.Query.limit(limit)
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @doc """
  Get reservation history for a client.
  """
  @spec client_reservation_history(String.t(), non_neg_integer()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  def client_reservation_history(client_id, limit \\ 50)
      when is_binary(client_id) and is_integer(limit) and limit >= 0 do
    with {:ok, reservations} <- get_client_reservations(client_id, limit) do
      {:ok, reservations}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to get client reservation history: #{inspect(error)}"}
    end
  end

  @spec get_client_reservations(String.t(), non_neg_integer()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  defp get_client_reservations(client_id, limit) do
    Reservation
    |> Ash.Query.filter(expr(client_id == ^client_id))
    |> Ash.Query.sort(reserved_from: :desc)
    |> Ash.Query.limit(limit)
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @doc """
  Get business metrics and statistics.
  """
  @spec business_metrics(String.t(), list()) :: {:ok, map()} | {:error, any()}
  def business_metrics(business_id, opts \\ [])
      when is_binary(business_id) and is_list(opts) do
    with {:ok, period} <- get_period(opts),
         {:ok, {start_date, end_date}} <- get_period_range(period),
         {:ok, reservations} <- get_reservations_for_period(business_id, start_date, end_date),
         {:ok, metrics} <- calculate_metrics(reservations, period, start_date, end_date) do
      {:ok, metrics}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to calculate metrics: #{inspect(error)}"}
    end
  end

  @spec get_period(list()) :: {:ok, atom()} | {:error, any()}
  defp get_period(opts) do
    period = Keyword.get(opts, :period, :month)
    if is_atom(period) do
      {:ok, period}
    else
      {:error, "Invalid period: #{inspect(period)}"}
    end
  end

  @spec get_reservations_for_period(String.t(), DateTime.t(), DateTime.t()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  defp get_reservations_for_period(business_id, start_date, end_date) do
    Reservation
    |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
    |> Ash.Query.filter(expr(reserved_from >= ^start_date and reserved_from <= ^end_date))
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @spec calculate_metrics(list(Reservation.t()), atom(), DateTime.t(), DateTime.t()) ::
        {:ok, map()} | {:error, any()}
  defp calculate_metrics(reservations, period, start_date, end_date) do
    metrics = %{
      total_reservations: length(reservations),
      confirmed_reservations: Enum.count(reservations, &(&1.status == :confirmed)),
      pending_reservations: Enum.count(reservations, &(&1.status == :pending)),
      cancelled_reservations: Enum.count(reservations, &(&1.status == :cancelled)),
      period: period,
      start_date: start_date,
      end_date: end_date
    }

    {:ok, metrics}
  end

  @doc """
  Get optimized business reservations with minimal data loading.
  """
  @spec business_reservations_optimized(String.t(), list()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  def business_reservations_optimized(business_id, opts \\ [])
      when is_binary(business_id) and is_list(opts) do
    with {:ok, limit} <- get_limit(opts),
         {:ok, status_filter} <- get_status_filter(opts),
         {:ok, reservations} <- get_business_reservations(business_id, limit, status_filter) do
      {:ok, reservations}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to get business reservations: #{inspect(error)}"}
    end
  end

  @spec get_limit(list()) :: {:ok, non_neg_integer()} | {:error, any()}
  defp get_limit(opts) do
    limit = Keyword.get(opts, :limit, 100)
    if is_integer(limit) and limit >= 0 do
      {:ok, limit}
    else
      {:error, "Invalid limit: #{inspect(limit)}"}
    end
  end

  @spec get_status_filter(list()) :: {:ok, list(atom())} | {:error, any()}
  defp get_status_filter(opts) do
    status_filter = Keyword.get(opts, :status, [:confirmed, :pending])
    if is_list(status_filter) and Enum.all?(status_filter, &is_atom/1) do
      {:ok, status_filter}
    else
      {:error, "Invalid status filter: #{inspect(status_filter)}"}
    end
  end

  @spec get_business_reservations(String.t(), non_neg_integer(), list(atom())) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  defp get_business_reservations(business_id, limit, status_filter) do
    Reservation
    |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
    |> Ash.Query.filter(expr(status in ^status_filter))
    |> Ash.Query.sort(reserved_from: :desc)
    |> Ash.Query.limit(limit)
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @doc """
  Find items with scheduling conflicts in a given time range.
  """
  @spec items_with_conflicts(String.t(), DateTime.t(), DateTime.t()) ::
        {:ok, list(Item.t())} | {:error, any()}
  def items_with_conflicts(business_id, start_datetime, end_datetime)
      when is_binary(business_id) and is_struct(start_datetime, DateTime) and is_struct(end_datetime, DateTime) do
    with {:ok, items} <- get_business_items(business_id),
         {:ok, conflicted_items} <- find_conflicted_items(items, start_datetime, end_datetime) do
      {:ok, conflicted_items}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to find conflicted items: #{inspect(error)}"}
    end
  end

  @spec find_conflicted_items(list(Item.t()), DateTime.t(), DateTime.t()) ::
        {:ok, list(Item.t())} | {:error, any()}
  defp find_conflicted_items(items, start_datetime, end_datetime) do
    conflicted_items =
      Enum.filter(items, fn item ->
        case RivaAsh.Validations.check_reservation_overlap(item.id, start_datetime, end_datetime) do
          {:error, _} -> true
          {:ok, :no_overlap} -> false
        end
      end)

    {:ok, conflicted_items}
  end

  @doc """
  Get employee workload statistics.
  """
  @spec employee_workload(String.t(), list()) :: {:ok, map()} | {:error, any()}
  def employee_workload(employee_id, opts \\ [])
      when is_binary(employee_id) and is_list(opts) do
    with {:ok, period} <- get_period(opts),
         {:ok, {start_date, end_date}} <- get_period_range(period),
         {:ok, reservations} <- get_employee_reservations(employee_id, start_date, end_date),
         {:ok, workload} <- calculate_workload(reservations, period, start_date, end_date) do
      {:ok, workload}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to calculate workload: #{inspect(error)}"}
    end
  end

  @spec get_employee_reservations(String.t(), DateTime.t(), DateTime.t()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  defp get_employee_reservations(employee_id, start_date, end_date) do
    Reservation
    |> Ash.Query.filter(expr(employee_id == ^employee_id))
    |> Ash.Query.filter(expr(reserved_from >= ^start_date and reserved_from <= ^end_date))
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @spec calculate_workload(list(Reservation.t()), atom(), DateTime.t(), DateTime.t()) ::
        {:ok, map()} | {:error, any()}
  defp calculate_workload(reservations, period, start_date, end_date) do
    workload = %{
      total_reservations: length(reservations),
      confirmed_reservations: Enum.count(reservations, &(&1.status == :confirmed)),
      pending_reservations: Enum.count(reservations, &(&1.status == :pending)),
      period: period,
      start_date: start_date,
      end_date: end_date
    }

    {:ok, workload}
  end

  @doc """
  Get popular items for a business based on reservation count.
  """
  @spec popular_items_for_business(String.t(), non_neg_integer()) ::
        {:ok, list(Item.t())} | {:error, any()}
  def popular_items_for_business(business_id, limit \\ 10)
      when is_binary(business_id) and is_integer(limit) and limit >= 0 do
    with {:ok, items} <- get_business_items(business_id),
         {:ok, items_with_counts} <- calculate_item_popularity(items),
         {:ok, popular_items} <- extract_popular_items(items_with_counts, limit) do
      {:ok, popular_items}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to get popular items: #{inspect(error)}"}
    end
  end

  @spec calculate_item_popularity(list(Item.t())) ::
        {:ok, list({Item.t(), non_neg_integer()})} | {:error, any()}
  defp calculate_item_popularity(items) do
    items_with_counts =
      Enum.map(items, fn item ->
        count = get_reservation_count(item.id)
        {item, count}
      end)

    {:ok, items_with_counts}
  end

  @spec get_reservation_count(String.t()) :: non_neg_integer()
  defp get_reservation_count(item_id) do
    case Reservation
         |> Ash.Query.filter(expr(item_id == ^item_id))
         |> Ash.Query.filter(expr(status in [:confirmed, :completed]))
         |> Ash.read(domain: RivaAsh.Domain) do
      {:ok, reservations} -> length(reservations)
      {:error, _} -> 0
    end
  end

  @spec extract_popular_items(list({Item.t(), non_neg_integer()}), non_neg_integer()) ::
        {:ok, list(Item.t())} | {:error, any()}
  defp extract_popular_items(items_with_counts, limit) do
    popular_items =
      items_with_counts
      |> Enum.sort_by(fn {_item, count} -> count end, :desc)
      |> Enum.take(limit)
      |> Enum.map(fn {item, _count} -> item end)

    {:ok, popular_items}
  end

  @doc """
  Search items by name or description.
  """
  @spec search_items(String.t(), String.t(), list()) :: {:ok, list(Item.t())} | {:error, any()}
  def search_items(business_id, search_term, opts \\ [])
      when is_binary(business_id) and is_binary(search_term) and is_list(opts) do
    with {:ok, include_inactive} <- get_include_inactive(opts),
         {:ok, query} <- build_search_query(business_id, search_term, include_inactive),
         {:ok, results} <- execute_search_query(query) do
      {:ok, results}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to search items: #{inspect(error)}"}
    end
  end

  @spec get_include_inactive(list()) :: {:ok, boolean()} | {:error, any()}
  defp get_include_inactive(opts) do
    include_inactive = Keyword.get(opts, :include_inactive, false)
    if is_boolean(include_inactive) do
      {:ok, include_inactive}
    else
      {:error, "Invalid include_inactive value: #{inspect(include_inactive)}"}
    end
  end

  @spec build_search_query(String.t(), String.t(), boolean()) ::
        {:ok, Ash.Query.t()} | {:error, any()}
  defp build_search_query(business_id, search_term, include_inactive) do
    query =
      Item
      |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
      |> Ash.Query.filter(
        expr(
          ilike(name, ^"%#{search_term}%") or
            ilike(section.name, ^"%#{search_term}%")
        )
      )

    query =
      if include_inactive do
        query
      else
        Ash.Query.filter(query, expr(is_active == true and is_nil(archived_at)))
      end

    {:ok, query}
  end

  @spec execute_search_query(Ash.Query.t()) :: {:ok, list(Item.t())} | {:error, any()}
  defp execute_search_query(query) do
    query
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @doc """
  Get reservation calendar data for a business in a date range.
  """
  @spec reservation_calendar_data(String.t(), Date.t(), Date.t()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  def reservation_calendar_data(business_id, start_date, end_date)
      when is_binary(business_id) and is_struct(start_date, Date) and is_struct(end_date, Date) do
    with {:ok, start_datetime} <- create_start_datetime(start_date),
         {:ok, end_datetime} <- create_end_datetime(end_date),
         {:ok, reservations} <- get_calendar_reservations(business_id, start_datetime, end_datetime) do
      {:ok, reservations}
    else
      {:error, reason} -> {:error, reason}
      error -> {:error, "Failed to get calendar data: #{inspect(error)}"}
    end
  end

  @spec get_calendar_reservations(String.t(), DateTime.t(), DateTime.t()) ::
        {:ok, list(Reservation.t())} | {:error, any()}
  defp get_calendar_reservations(business_id, start_datetime, end_datetime) do
    Reservation
    |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
    |> Ash.Query.filter(expr(reserved_from >= ^start_datetime and reserved_from <= ^end_datetime))
    |> Ash.Query.filter(expr(status in [:confirmed, :pending]))
    |> Ash.Query.sort(reserved_from: :asc)
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @doc """
  Helper function to get date ranges for different periods.
  """
  @spec get_period_range(atom()) :: {:ok, {DateTime.t(), DateTime.t()}} | {:error, any()}
  defp get_period_range(period) when is_atom(period) do
    case Timex.today() do
      %Date{} = today ->
        {start_date, end_date} = calculate_period_dates(today, period)
        {:ok, {start_date, end_date}}

      error ->
        {:error, error}
    end
  end

  @spec calculate_period_dates(Date.t(), atom()) :: {DateTime.t(), DateTime.t()}
  defp calculate_period_dates(today, period) do
    case period do
      :day ->
        {Timex.beginning_of_day(today), Timex.end_of_day(today)}

      :week ->
        {Timex.beginning_of_week(today, :mon), Timex.end_of_week(today, :mon)}

      :month ->
        {Timex.beginning_of_month(today), Timex.end_of_month(today)}

      _ ->
        # Default to current month
        start_date = Timex.beginning_of_month(today)
        end_date = Timex.end_of_month(today)
        {Timex.beginning_of_day(start_date), Timex.end_of_day(end_date)}
    end
  end
end
