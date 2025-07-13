defmodule RivaAsh.Queries do
  @moduledoc """
  Optimized query patterns and helper functions for common operations.
  These functions provide efficient ways to query the most common data patterns.
  """

  import Ecto.Query
  alias RivaAsh.Resources.{Item, Reservation, Client, Employee, Business}

  @doc """
  Efficiently check item availability for a given date range.
  Uses optimized indexes and minimal data loading.
  """
  def check_item_availability(item_id, start_datetime, end_datetime) do
    # Use exists query for better performance
    conflicting_reservations =
      Reservation
      |> Ash.Query.filter(
        item_id == ^item_id and
        status in [:confirmed, :pending] and
        not (reserved_until <= ^start_datetime or reserved_from >= ^end_datetime)
      )
      |> Ash.Query.select([:id])
      |> Ash.exists?()

    not conflicting_reservations
  end

  @doc """
  Get available items for a business on a specific date.
  Optimized for dashboard and booking interfaces.
  """
  def available_items_for_business_and_date(business_id, date) do
    start_of_day = DateTime.new!(date, ~T[00:00:00], "Etc/UTC")
    end_of_day = DateTime.new!(date, ~T[23:59:59], "Etc/UTC")

    Item
    |> Ash.Query.filter(
      business_id == ^business_id and
      is_active == true and
      is_nil(archived_at) and
      not exists(reservations,
        status in [:confirmed, :pending] and
        not (reserved_until <= ^start_of_day or reserved_from >= ^end_of_day)
      )
    )
    |> Ash.Query.load([:item_type, :section])
    |> Ash.read!()
  end

  @doc """
  Get upcoming reservations for a business with minimal data loading.
  Optimized for dashboard views using denormalized business_id.
  """
  def upcoming_reservations_for_business(business_id, limit \\ 50) do
    now = DateTime.utc_now()

    Reservation
    |> Ash.Query.filter(
      business_id == ^business_id and
      status in [:confirmed, :pending] and
      reserved_from >= ^now
    )
    |> Ash.Query.sort(reserved_from: :asc)
    |> Ash.Query.limit(limit)
    |> Ash.Query.load([:client, :item, :employee])
    |> Ash.read!(domain: RivaAsh.Domain)
  end

  @doc """
  Get client reservation history with pagination support.
  """
  def client_reservation_history(client_id, opts \\ []) do
    limit = Keyword.get(opts, :limit, 20)
    offset = Keyword.get(opts, :offset, 0)

    Reservation
    |> Ash.Query.filter(client_id == ^client_id)
    |> Ash.Query.sort(reserved_from: :desc)
    |> Ash.Query.limit(limit)
    |> Ash.Query.offset(offset)
    |> Ash.Query.load([:item, :payments])
    |> Ash.read!()
  end

  @doc """
  Get business performance metrics efficiently using denormalized business_id.
  Uses aggregates to avoid loading full datasets and deep joins.
  """
  def business_metrics(business_id, date_range \\ nil) do
    base_filter = [business_id: business_id]

    date_filter = case date_range do
      {start_date, end_date} ->
        start_datetime = DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
        end_datetime = DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")
        [reserved_from: [gte: start_datetime], reserved_until: [lte: end_datetime]]
      _ -> []
    end

    filter = Keyword.merge(base_filter, date_filter)

    # Get reservation counts by status (optimized with business_id index)
    reservation_counts =
      Reservation
      |> Ash.Query.filter(^filter)
      |> Ash.Query.aggregate(:count, :status)
      |> Ash.read!(domain: RivaAsh.Domain)

    # Get total reservations count
    total_reservations =
      Reservation
      |> Ash.Query.filter(^filter)
      |> Ash.Query.aggregate(:count, :id)
      |> Ash.read!(domain: RivaAsh.Domain)

    # Get payment metrics (optimized with business_id index)
    payment_metrics =
      RivaAsh.Resources.Payment
      |> Ash.Query.filter(business_id: business_id)
      |> Ash.Query.aggregate(:sum, :amount_paid)
      |> Ash.read!(domain: RivaAsh.Domain)

    %{
      total_reservations: total_reservations,
      reservation_counts: reservation_counts,
      payment_metrics: payment_metrics,
      date_range: date_range
    }
  end

  @doc """
  Get business reservations with date range filtering - optimized version.
  Uses denormalized business_id to avoid deep joins.
  """
  def business_reservations_optimized(business_id, opts \\ []) do
    limit = Keyword.get(opts, :limit, 100)
    status_filter = Keyword.get(opts, :status)
    date_from = Keyword.get(opts, :date_from)
    date_until = Keyword.get(opts, :date_until)

    query =
      Reservation
      |> Ash.Query.filter(business_id == ^business_id)

    # Add status filter if provided
    query = if status_filter do
      Ash.Query.filter(query, status == ^status_filter)
    else
      query
    end

    # Add date range filter if provided
    query = case {date_from, date_until} do
      {from, until} when not is_nil(from) and not is_nil(until) ->
        Ash.Query.filter(query, reserved_from >= ^from and reserved_until <= ^until)
      {from, nil} when not is_nil(from) ->
        Ash.Query.filter(query, reserved_from >= ^from)
      {nil, until} when not is_nil(until) ->
        Ash.Query.filter(query, reserved_until <= ^until)
      _ ->
        query
    end

    query
    |> Ash.Query.sort(reserved_from: :desc)
    |> Ash.Query.limit(limit)
    |> Ash.Query.load([:client, :item, :employee])
    |> Ash.read!(domain: RivaAsh.Domain)
  end

  @doc """
  Find items with availability conflicts for a given time range.
  Useful for maintenance and scheduling.
  """
  def items_with_conflicts(business_id, start_datetime, end_datetime) do
    Item
    |> Ash.Query.filter(
      business_id == ^business_id and
      is_active == true and
      exists(reservations,
        status in [:confirmed, :pending] and
        not (reserved_until <= ^start_datetime or reserved_from >= ^end_datetime)
      )
    )
    |> Ash.Query.load([
      reservations: [
        filter: [
          status: [:confirmed, :pending],
          reserved_from: [lte: end_datetime],
          reserved_until: [gte: start_datetime]
        ]
      ]
    ])
    |> Ash.read!()
  end

  @doc """
  Get employee workload for scheduling and management.
  """
  def employee_workload(employee_id, date_range \\ nil) do
    base_filter = [employee_id: employee_id]

    date_filter = case date_range do
      {start_date, end_date} ->
        start_datetime = DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
        end_datetime = DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")
        [reserved_from: [gte: start_datetime], reserved_until: [lte: end_datetime]]
      _ -> []
    end

    filter = Keyword.merge(base_filter, date_filter)

    Reservation
    |> Ash.Query.filter(^filter)
    |> Ash.Query.aggregate(:count, :status)
    |> Ash.read!()
  end

  @doc """
  Bulk operations helper for updating multiple records efficiently.
  """
  def bulk_update_reservation_status(reservation_ids, new_status, actor) do
    Ash.bulk_update(
      Reservation,
      :update,
      %{status: new_status},
      filter: [id: [in: reservation_ids]],
      context: %{actor: actor}
    )
  end

  @doc """
  Get popular items based on reservation frequency.
  Useful for business insights and recommendations.
  """
  def popular_items_for_business(business_id, limit \\ 10) do
    # This would typically use a more complex query with aggregates
    # For now, a simplified version
    Item
    |> Ash.Query.filter(business_id == ^business_id and is_active == true)
    |> Ash.Query.load([
      reservations: [
        filter: [status: [:confirmed, :completed]]
      ]
    ])
    |> Ash.Query.sort(reservation_count: :desc)
    |> Ash.Query.limit(limit)
    |> Ash.read!()
  end

  @doc """
  Search items with full-text search capabilities.
  Optimized for user-facing search interfaces.
  """
  def search_items(business_id, search_term, opts \\ []) do
    limit = Keyword.get(opts, :limit, 20)
    include_inactive = Keyword.get(opts, :include_inactive, false)

    base_filter = [business_id: business_id]

    active_filter = if include_inactive do
      []
    else
      [is_active: true, archived_at: nil]
    end

    filter = Keyword.merge(base_filter, active_filter)

    Item
    |> Ash.Query.filter(^filter)
    |> Ash.Query.filter(contains(name, ^search_term) or contains(description, ^search_term))
    |> Ash.Query.load([:item_type, :section])
    |> Ash.Query.limit(limit)
    |> Ash.read!()
  end

  @doc """
  Get reservation calendar data for a specific month.
  Optimized for calendar views and scheduling interfaces using denormalized business_id.
  """
  def reservation_calendar_data(business_id, year, month) do
    start_date = Date.new!(year, month, 1)
    end_date = Date.end_of_month(start_date)

    start_datetime = DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
    end_datetime = DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")

    Reservation
    |> Ash.Query.filter(
      business_id == ^business_id and
      status in [:confirmed, :pending] and
      reserved_from <= ^end_datetime and
      reserved_until >= ^start_datetime
    )
    |> Ash.Query.load([:client, :item])
    |> Ash.Query.sort(reserved_from: :asc)
    |> Ash.read!(domain: RivaAsh.Domain)
  end
end
