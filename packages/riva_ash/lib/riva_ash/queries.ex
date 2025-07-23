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
  def check_item_availability(item_id, start_datetime, end_datetime) do
    RivaAsh.Validations.check_item_availability(item_id, start_datetime, end_datetime)
  end

  # Removed duplicate validation functions as they are now handled by Validations module.
  # defp validate_item_active(%{is_active: true}), do: {:ok, :ok}
  # defp validate_item_active(_), do: {:error, :item_inactive}
  #
  # defp validate_item_not_archived(%{archived_at: nil}), do: {:ok, :ok}
  # defp validate_item_not_archived(_), do: {:error, :item_archived}
  #
  # defp check_reservations_and_holds(item_id, start_datetime, end_datetime) do
  #   with {:ok, :no_overlap} <- RivaAsh.Validations.check_reservation_overlap(item_id, start_datetime, end_datetime),
  #        {:ok, :available} <- RivaAsh.Validations.check_active_holds(item_id, start_datetime, end_datetime) do
  #     {:ok, :available}
  #   else
  #     {:error, reason} -> {:error, reason}
  #     error -> {:error, error}
  #   end
  # end

  @doc """
  Get all available items for a business on a specific date.
  """
  def available_items_for_business_and_date(business_id, date) do
    try do
      start_datetime = DateTime.new!(date, ~T[00:00:00], "Etc/UTC")
      end_datetime = DateTime.new!(date, ~T[23:59:59], "Etc/UTC")

      with {:ok, items} <- Item
                          |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
                          |> Ash.Query.filter(expr(is_active == true))
                          |> Ash.Query.filter(expr(is_nil(archived_at)))
                          |> Ash.read(domain: RivaAsh.Domain) do
        # Check each item's availability
        available_items = filter_available_items(items, start_datetime, end_datetime)
        {:ok, available_items}
      else
        {:error, error} -> {:error, error}
        error -> {:error, error}
      end
    rescue
      e -> {:error, "Failed to get available items: #{inspect(e)}"}
    end
  end

  defp filter_available_items(items, start_datetime, end_datetime) do
    Enum.filter(items, fn item ->
      case check_item_availability(item.id, start_datetime, end_datetime) do
        {:ok, :available} -> true
        _ -> false
      end
    end)
  end

  @doc """
  Get upcoming reservations for a business.
  """
  def upcoming_reservations_for_business(business_id, limit \\ 10) do
    try do
      now = Timex.utc_now()

      Reservation
      |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(reserved_from > ^now))
      |> Ash.Query.filter(expr(status in [:confirmed, :pending]))
      |> Ash.Query.sort(reserved_from: :asc)
      |> Ash.Query.limit(limit)
      |> Ash.read(domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Failed to get upcoming reservations: #{inspect(e)}"}
    end
  end

  @doc """
  Get reservation history for a client.
  """
  def client_reservation_history(client_id, limit \\ 50) do
    try do
      Reservation
      |> Ash.Query.filter(expr(client_id == ^client_id))
      |> Ash.Query.sort(reserved_from: :desc)
      |> Ash.Query.limit(limit)
      |> Ash.read(domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Failed to get client reservation history: #{inspect(e)}"}
    end
  end

  @doc """
  Get business metrics and statistics.
  """
  def business_metrics(business_id, opts \\ []) do
    try do
      period = Keyword.get(opts, :period, :month)
      {start_date, end_date} = get_period_range(period)

      with {:ok, reservations} <- Reservation
                                 |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
                                 |> Ash.Query.filter(expr(reserved_from >= ^start_date and reserved_from <= ^end_date))
                                 |> Ash.read(domain: RivaAsh.Domain) do
        metrics = calculate_metrics(reservations, period, start_date, end_date)
        {:ok, metrics}
      else
        {:error, error} -> {:error, error}
        error -> {:error, error}
      end
    rescue
      e -> {:error, "Failed to calculate metrics: #{inspect(e)}"}
    end
  end

  defp calculate_metrics(reservations, period, start_date, end_date) do
    %{
      total_reservations: length(reservations),
      confirmed_reservations: Enum.count(reservations, &(&1.status == :confirmed)),
      pending_reservations: Enum.count(reservations, &(&1.status == :pending)),
      cancelled_reservations: Enum.count(reservations, &(&1.status == :cancelled)),
      period: period,
      start_date: start_date,
      end_date: end_date
    }
  end

  @doc """
  Get optimized business reservations with minimal data loading.
  """
  def business_reservations_optimized(business_id, opts \\ []) do
    try do
      limit = Keyword.get(opts, :limit, 100)
      status_filter = Keyword.get(opts, :status, [:confirmed, :pending])

      Reservation
      |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(status in ^status_filter))
      |> Ash.Query.sort(reserved_from: :desc)
      |> Ash.Query.limit(limit)
      |> Ash.read(domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Failed to get business reservations: #{inspect(e)}"}
    end
  end

  @doc """
  Find items with scheduling conflicts in a given time range.
  """
  def items_with_conflicts(business_id, start_datetime, end_datetime) do
    try do
      with {:ok, items} <- Item
                          |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
                          |> Ash.Query.filter(expr(is_active == true))
                          |> Ash.read(domain: RivaAsh.Domain) do
        conflicted_items = find_conflicted_items(items, start_datetime, end_datetime)
        {:ok, conflicted_items}
      else
        {:error, error} -> {:error, error}
        error -> {:error, error}
      end
    rescue
      e -> {:error, "Failed to find conflicted items: #{inspect(e)}"}
    end
  end

  defp find_conflicted_items(items, start_datetime, end_datetime) do
    Enum.filter(items, fn item ->
      case RivaAsh.Validations.check_reservation_overlap(item.id, start_datetime, end_datetime) do
        {:error, _} -> true  # Consider errors as conflicts
        {:ok, :no_overlap} -> false
      end
    end)
  end

  @doc """
  Get employee workload statistics.
  """
  def employee_workload(employee_id, opts \\ []) do
    try do
      period = Keyword.get(opts, :period, :week)
      {start_date, end_date} = get_period_range(period)

      with {:ok, reservations} <- Reservation
                                 |> Ash.Query.filter(expr(employee_id == ^employee_id))
                                 |> Ash.Query.filter(expr(reserved_from >= ^start_date and reserved_from <= ^end_date))
                                 |> Ash.read(domain: RivaAsh.Domain) do
        workload = calculate_workload(reservations, period, start_date, end_date)
        {:ok, workload}
      else
        {:error, error} -> {:error, error}
        error -> {:error, error}
      end
    rescue
      e -> {:error, "Failed to calculate workload: #{inspect(e)}"}
    end
  end

  defp calculate_workload(reservations, period, start_date, end_date) do
    %{
      total_reservations: length(reservations),
      confirmed_reservations: Enum.count(reservations, &(&1.status == :confirmed)),
      pending_reservations: Enum.count(reservations, &(&1.status == :pending)),
      period: period,
      start_date: start_date,
      end_date: end_date
    }
  end

  @doc """
  Get popular items for a business based on reservation count.
  """
  def popular_items_for_business(business_id, limit \\ 10) do
    try do
      with {:ok, items} <- Item
                          |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
                          |> Ash.Query.filter(expr(is_active == true))
                          |> Ash.read(domain: RivaAsh.Domain) do
        items_with_counts = calculate_item_popularity(items)
        popular_items = extract_popular_items(items_with_counts, limit)
        {:ok, popular_items}
      else
        {:error, error} -> {:error, error}
        error -> {:error, error}
      end
    rescue
      e -> {:error, "Failed to get popular items: #{inspect(e)}"}
    end
  end

  defp calculate_item_popularity(items) do
    Enum.map(items, fn item ->
      count = Reservation
              |> Ash.Query.filter(expr(item_id == ^item.id))
              |> Ash.Query.filter(expr(status in [:confirmed, :completed]))
              |> Ash.read(domain: RivaAsh.Domain)
              |> case do
                {:ok, reservations} -> length(reservations)
                {:error, _} -> 0
              end
      {item, count}
    end)
  end

  defp extract_popular_items(items_with_counts, limit) do
    items_with_counts
    |> Enum.sort_by(fn {_item, count} -> count end, :desc)
    |> Enum.take(limit)
    |> Enum.map(fn {item, _count} -> item end)
  end

  @doc """
  Search items by name or description.
  """
  def search_items(business_id, search_term, opts \\ []) do
    try do
      include_inactive = Keyword.get(opts, :include_inactive, false)

      query = Item
      |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(
        ilike(name, ^"%#{search_term}%") or
        ilike(section.name, ^"%#{search_term}%")
      ))

      query = if include_inactive do
        query
      else
        Ash.Query.filter(query, expr(is_active == true and is_nil(archived_at)))
      end

      query
      |> Ash.read(domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Failed to search items: #{inspect(e)}"}
    end
  end

  @doc """
  Get reservation calendar data for a business in a date range.
  """
  def reservation_calendar_data(business_id, start_date, end_date) do
    try do
      start_datetime = DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
      end_datetime = DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")

      Reservation
      |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(reserved_from >= ^start_datetime and reserved_from <= ^end_datetime))
      |> Ash.Query.filter(expr(status in [:confirmed, :pending]))
      |> Ash.Query.sort(reserved_from: :asc)
      |> Ash.read(domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Failed to get calendar data: #{inspect(e)}"}
    end
  end

  # Helper function to get date ranges for different periods
  defp get_period_range(period) do
    today = Timex.today()

    case period do
      :day ->
        start_datetime = Timex.beginning_of_day(today)
        end_datetime = Timex.end_of_day(today)
        {start_datetime, end_datetime}
      :week ->
        start_date = Timex.beginning_of_week(today, :mon)
        end_date = Timex.end_of_week(today, :mon)
        {start_date, end_date}
      :month ->
        start_date = Timex.beginning_of_month(today)
        end_date = Timex.end_of_month(today)
        {start_date, end_date}
      _ ->
        # Default to current month
        start_date = Timex.beginning_of_month(today)
        end_date = Timex.end_of_month(today)
        start_datetime = Timex.beginning_of_day(start_date)
        end_datetime = Timex.end_of_day(end_date)
        {start_datetime, end_datetime}
    end
  end
end
