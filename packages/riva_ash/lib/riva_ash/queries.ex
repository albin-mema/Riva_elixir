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
    try do
      # First check if item exists and is active
      case Ash.get(Item, item_id, domain: RivaAsh.Domain) do
        {:ok, nil} -> {:ok, :unavailable, "Item not found"}
        {:ok, item} ->
          cond do
            not item.is_active -> {:ok, :unavailable, "Item is not active"}
            not is_nil(item.archived_at) -> {:ok, :unavailable, "Item is archived"}
            item.is_always_available -> check_reservations_and_holds(item_id, start_datetime, end_datetime)
            true -> check_reservations_and_holds(item_id, start_datetime, end_datetime)
          end
        {:error, error} -> {:error, "Failed to fetch item: #{inspect(error)}"}
      end
    rescue
      e -> {:error, "Exception during availability check: #{inspect(e)}"}
    end
  end

  defp check_reservations_and_holds(item_id, start_datetime, end_datetime) do
    # Check for conflicting reservations
    case RivaAsh.Validations.check_reservation_overlap(item_id, start_datetime, end_datetime) do
      {:ok, :no_overlap} ->
        # Check for active holds
        case RivaAsh.Validations.check_active_holds(item_id, start_datetime, end_datetime) do
          {:ok, :available} -> {:ok, :available}
          {:ok, :unavailable, reason} -> {:ok, :unavailable, reason}
          {:error, reason} -> {:error, reason}
        end
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Get all available items for a business on a specific date.
  """
  def available_items_for_business_and_date(business_id, date) do
    try do
      # Convert date to datetime range for the full day
      start_datetime = DateTime.new!(date, ~T[00:00:00], "Etc/UTC")
      end_datetime = DateTime.new!(date, ~T[23:59:59], "Etc/UTC")

      query = Item
      |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(is_active == true))
      |> Ash.Query.filter(expr(is_nil(archived_at)))

      case Ash.read(query, domain: RivaAsh.Domain) do
        {:ok, items} ->
          # Filter items that are available for the date
          available_items = Enum.filter(items, fn item ->
            case check_item_availability(item.id, start_datetime, end_datetime) do
              {:ok, :available} -> true
              _ -> false
            end
          end)
          {:ok, available_items}
        {:error, error} -> {:error, "Failed to fetch items: #{inspect(error)}"}
      end
    rescue
      e -> {:error, "Exception during query: #{inspect(e)}"}
    end
  end

  @doc """
  Get upcoming reservations for a business.
  """
  def upcoming_reservations_for_business(business_id, limit \\ 10) do
    try do
      now = DateTime.utc_now()

      query = Reservation
      |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(reserved_from > ^now))
      |> Ash.Query.filter(expr(status in [:confirmed, :pending]))
      |> Ash.Query.sort(reserved_from: :asc)
      |> Ash.Query.limit(limit)

      Ash.read(query, domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Exception during query: #{inspect(e)}"}
    end
  end

  @doc """
  Get reservation history for a client.
  """
  def client_reservation_history(client_id, limit \\ 50) do
    try do
      query = Reservation
      |> Ash.Query.filter(expr(client_id == ^client_id))
      |> Ash.Query.sort(reserved_from: :desc)
      |> Ash.Query.limit(limit)

      Ash.read(query, domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Exception during query: #{inspect(e)}"}
    end
  end

  @doc """
  Get business metrics and statistics.
  """
  def business_metrics(business_id, opts \\ []) do
    try do
      period = Keyword.get(opts, :period, :month)
      {start_date, end_date} = get_period_range(period)

      # Get reservation counts
      reservation_query = Reservation
      |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(reserved_from >= ^start_date and reserved_from <= ^end_date))

      case Ash.read(reservation_query, domain: RivaAsh.Domain) do
        {:ok, reservations} ->
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
        {:error, error} -> {:error, "Failed to fetch metrics: #{inspect(error)}"}
      end
    rescue
      e -> {:error, "Exception during metrics calculation: #{inspect(e)}"}
    end
  end

  @doc """
  Get optimized business reservations with minimal data loading.
  """
  def business_reservations_optimized(business_id, opts \\ []) do
    try do
      limit = Keyword.get(opts, :limit, 100)
      status_filter = Keyword.get(opts, :status, [:confirmed, :pending])

      query = Reservation
      |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(status in ^status_filter))
      |> Ash.Query.sort(reserved_from: :desc)
      |> Ash.Query.limit(limit)

      Ash.read(query, domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Exception during query: #{inspect(e)}"}
    end
  end

  @doc """
  Find items with scheduling conflicts in a given time range.
  """
  def items_with_conflicts(business_id, start_datetime, end_datetime) do
    try do
      # Get all items for the business
      items_query = Item
      |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(is_active == true))

      case Ash.read(items_query, domain: RivaAsh.Domain) do
        {:ok, items} ->
          # Check each item for conflicts
          conflicted_items = Enum.filter(items, fn item ->
            case RivaAsh.Validations.check_reservation_overlap(item.id, start_datetime, end_datetime) do
              {:error, _} -> true  # Consider errors as conflicts
              {:ok, :no_overlap} -> false
            end
          end)
          {:ok, conflicted_items}
        {:error, error} -> {:error, "Failed to fetch items: #{inspect(error)}"}
      end
    rescue
      e -> {:error, "Exception during conflict check: #{inspect(e)}"}
    end
  end

  @doc """
  Get employee workload statistics.
  """
  def employee_workload(employee_id, opts \\ []) do
    try do
      period = Keyword.get(opts, :period, :week)
      {start_date, end_date} = get_period_range(period)

      # Get reservations handled by this employee
      reservation_query = Reservation
      |> Ash.Query.filter(expr(employee_id == ^employee_id))
      |> Ash.Query.filter(expr(reserved_from >= ^start_date and reserved_from <= ^end_date))

      case Ash.read(reservation_query, domain: RivaAsh.Domain) do
        {:ok, reservations} ->
          workload = %{
            total_reservations: length(reservations),
            confirmed_reservations: Enum.count(reservations, &(&1.status == :confirmed)),
            pending_reservations: Enum.count(reservations, &(&1.status == :pending)),
            period: period,
            start_date: start_date,
            end_date: end_date
          }
          {:ok, workload}
        {:error, error} -> {:error, "Failed to fetch workload: #{inspect(error)}"}
      end
    rescue
      e -> {:error, "Exception during workload calculation: #{inspect(e)}"}
    end
  end

  @doc """
  Get popular items for a business based on reservation count.
  """
  def popular_items_for_business(business_id, limit \\ 10) do
    try do
      # Get items with reservation counts
      # Note: This is a simplified version. In production, you might want to use aggregates
      items_query = Item
      |> Ash.Query.filter(expr(section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(is_active == true))

      case Ash.read(items_query, domain: RivaAsh.Domain) do
        {:ok, items} ->
          # For each item, count reservations (simplified approach)
          items_with_counts = Enum.map(items, fn item ->
            reservation_query = Reservation
            |> Ash.Query.filter(expr(item_id == ^item.id))
            |> Ash.Query.filter(expr(status in [:confirmed, :completed]))

            case Ash.read(reservation_query, domain: RivaAsh.Domain) do
              {:ok, reservations} -> {item, length(reservations)}
              {:error, _} -> {item, 0}
            end
          end)

          # Sort by reservation count and take top items
          popular_items = items_with_counts
          |> Enum.sort_by(fn {_item, count} -> count end, :desc)
          |> Enum.take(limit)
          |> Enum.map(fn {item, _count} -> item end)

          {:ok, popular_items}
        {:error, error} -> {:error, "Failed to fetch items: #{inspect(error)}"}
      end
    rescue
      e -> {:error, "Exception during query: #{inspect(e)}"}
    end
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

      Ash.read(query, domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Exception during search: #{inspect(e)}"}
    end
  end

  @doc """
  Get reservation calendar data for a business in a date range.
  """
  def reservation_calendar_data(business_id, start_date, end_date) do
    try do
      start_datetime = DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
      end_datetime = DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")

      query = Reservation
      |> Ash.Query.filter(expr(item.section.plot.business_id == ^business_id))
      |> Ash.Query.filter(expr(reserved_from >= ^start_datetime and reserved_from <= ^end_datetime))
      |> Ash.Query.filter(expr(status in [:confirmed, :pending]))
      |> Ash.Query.sort(reserved_from: :asc)

      Ash.read(query, domain: RivaAsh.Domain)
    rescue
      e -> {:error, "Exception during calendar query: #{inspect(e)}"}
    end
  end

  # Helper function to get date ranges for different periods
  defp get_period_range(period) do
    today = Date.utc_today()

    case period do
      :day ->
        start_datetime = DateTime.new!(today, ~T[00:00:00], "Etc/UTC")
        end_datetime = DateTime.new!(today, ~T[23:59:59], "Etc/UTC")
        {start_datetime, end_datetime}
      :week ->
        start_date = Date.beginning_of_week(today)
        end_date = Date.end_of_week(today)
        start_datetime = DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
        end_datetime = DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")
        {start_datetime, end_datetime}
      :month ->
        start_date = Date.beginning_of_month(today)
        end_date = Date.end_of_month(today)
        start_datetime = DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
        end_datetime = DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")
        {start_datetime, end_datetime}
      _ ->
        # Default to current month
        start_date = Date.beginning_of_month(today)
        end_date = Date.end_of_month(today)
        start_datetime = DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
        end_datetime = DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")
        {start_datetime, end_datetime}
    end
  end
end
