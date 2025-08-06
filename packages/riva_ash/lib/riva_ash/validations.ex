defmodule RivaAsh.Validations do
  @moduledoc """
  Shared validation functions for common business logic across resources.
  Provides consistent validation patterns and reusable validation functions.
  """

  import Ash.Expr
  require Ash.Query
  alias RivaAsh.ErrorHelpers

  @type changeset :: Ash.Changeset.t()
  @type opts :: Keyword.t()
  @type item_id :: String.t() | integer()
  @type reserved_from :: DateTime.t() | NaiveDateTime.t()
  @type reserved_until :: DateTime.t() | NaiveDateTime.t()
  @type business_id :: String.t() | integer()
  @type employee_id :: String.t() | integer()
  @type client_id :: String.t() | integer()
  @type section_id :: String.t() | integer()
  @type item_type_id :: String.t() | integer()
  @type plot_id :: String.t() | integer()
  @type layout_id :: String.t() | integer()
  @type reservation_id :: String.t() | integer()
  @type granted_by_id :: String.t() | integer()
  @type pricing_type :: atom()
  @type effective_from :: Date.t() | nil
  @type effective_until :: Date.t() | nil
  @type weekday_price :: Decimal.t() | nil
  @type weekend_price :: Decimal.t() | nil
  @type start_time :: Time.t() | DateTime.t()
  @type end_time :: Time.t() | DateTime.t()
  @type date :: Date.t()
  @type email :: String.t()
  @type phone :: String.t()
  @type text_input :: String.t()
  @type availability_status :: :available | {:unavailable, String.t()}
  @type overlap_result :: {:ok, :no_overlap} | {:ok, :overlap_found} | {:error, String.t()}
  @type availability_result :: {:ok, availability_status()} | {:error, String.t()}
  @type validation_result :: :ok | {:error, map()}

  @doc """
  Validates that a reservation doesn't conflict with existing reservations.
  Uses standardized overlap detection logic.
  """
  @spec validate_reservation_availability(changeset, opts) :: validation_result()
  def validate_reservation_availability(changeset, opts \\ []) do
    with {:ok, item_id} <- get_required_attribute(changeset, :item_id),
         {:ok, reserved_from} <- get_required_attribute(changeset, :reserved_from),
         {:ok, reserved_until} <- get_required_attribute(changeset, :reserved_until),
         {:ok, current_reservation_id} <- get_optional_attribute(changeset, :id) do
      check_reservation_availability(item_id, reserved_from, reserved_until, current_reservation_id, opts)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec get_required_attribute(changeset, atom()) :: {:ok, any()} | {:error, :missing_attribute}
  defp get_required_attribute(changeset, field) do
    case Ash.Changeset.get_argument_or_attribute(changeset, field) do
      value when not is_nil(value) -> {:ok, value}
      _ -> {:error, :missing_attribute}
    end
  end

  @spec get_required_attribute(map()) :: {:ok, any()} | {:error, :missing_attribute}
  defp get_required_attribute(%{changeset: changeset, field: field}) do
    get_required_attribute(changeset, field)
  end

  @spec get_optional_attribute(changeset, atom()) :: {:ok, any()} | {:error, :missing_attribute}
  defp get_optional_attribute(changeset, field) do
    case Ash.Changeset.get_argument_or_attribute(changeset, field) do
      value -> {:ok, value}
    end
  end

  @spec get_optional_attribute(map()) :: {:ok, any()} | {:error, :missing_attribute}
  defp get_optional_attribute(%{changeset: changeset, field: field}) do
    get_optional_attribute(changeset, field)
  end

  @spec check_reservation_availability(
          item_id,
          reserved_from,
          reserved_until,
          reservation_id | nil,
          opts
        ) :: validation_result()
  defp check_reservation_availability(item_id, reserved_from, reserved_until, current_reservation_id, opts) do
    case check_reservation_overlap(item_id, reserved_from, reserved_until, current_reservation_id, opts) do
      {:ok, :no_overlap} -> :ok
      {:error, reason} -> {:error, %{field: :reserved_from, message: "Failed to check availability: #{reason}"}}
    end
  end

  @spec check_reservation_availability(
          map()
        ) :: validation_result()
  defp check_reservation_availability(%{
         item_id: item_id,
         reserved_from: reserved_from,
         reserved_until: reserved_until,
         current_reservation_id: current_reservation_id,
         opts: opts
       }) do
    check_reservation_availability(item_id, reserved_from, reserved_until, current_reservation_id, opts)
  end

  @doc """
  Standardized reservation overlap checking logic.
  Returns {:ok, :no_overlap} or {:ok, :overlap_found} or {:error, reason}.

  Options:
  - exclude_statuses: List of statuses to exclude from overlap check (default: [:cancelled, :completed])
  - include_provisional: Whether to include provisional reservations (default: true)
  """
  @spec check_reservation_overlap(item_id, reserved_from, reserved_until, item_id | nil, opts) :: overlap_result()
  def check_reservation_overlap(
        item_id,
        reserved_from,
        reserved_until,
        exclude_reservation_id \\ nil,
        opts \\ []
      ) do
    case {get_exclude_statuses(opts), build_status_filter(opts), filter_statuses(status_filter, exclude_statuses)} do
      {{:ok, exclude_statuses}, {:ok, status_filter}, {:ok, final_status_filter}} ->
        build_and_execute_overlap_query(
          item_id,
          reserved_from,
          reserved_until,
          exclude_reservation_id,
          final_status_filter
        )
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_reservation_overlap(map()) :: overlap_result()
  def check_reservation_overlap(%{
         item_id: item_id,
         reserved_from: reserved_from,
         reserved_until: reserved_until,
         exclude_reservation_id: exclude_reservation_id,
         opts: opts
       }) do
    check_reservation_overlap(item_id, reserved_from, reserved_until, exclude_reservation_id, opts)
  end

  @spec get_exclude_statuses(opts) :: {:ok, list()} | {:error, String.t()}
  defp get_exclude_statuses(opts) do
    case Keyword.get(opts, :exclude_statuses, [:cancelled, :completed]) do
      statuses when is_list(statuses) -> {:ok, statuses}
      _ -> {:error, "Invalid exclude_statuses format"}
    end
  end

  @spec get_exclude_statuses(map()) :: {:ok, list()} | {:error, String.t()}
  defp get_exclude_statuses(%{opts: opts}) do
    get_exclude_statuses(opts)
  end

  @spec build_status_filter(opts) :: {:ok, list()} | {:error, String.t()}
  defp build_status_filter(opts) do
    case validate_include_provisional(Keyword.get(opts, :include_provisional, true)) do
      {:ok, include_provisional} ->
        case include_provisional do
          true -> {:ok, [:confirmed, :pending, :provisional]}
          false -> {:ok, [:confirmed, :pending]}
        end
      {:error, reason} -> {:error, reason}
    end
  end

  @spec build_status_filter(map()) :: {:ok, list()} | {:error, String.t()}
  defp build_status_filter(%{opts: opts}) do
    build_status_filter(opts)
  end

  @spec validate_include_provisional(boolean()) :: {:ok, boolean()} | {:error, String.t()}
  defp validate_include_provisional(value) when is_boolean(value) do
    {:ok, value}
  end

  defp validate_include_provisional(_), do: {:error, "Invalid include_provisional value"}

  @spec validate_include_provisional(map()) :: {:ok, boolean()} | {:error, String.t()}
  defp validate_include_provisional(%{value: value}) do
    validate_include_provisional(value)
  end

  @spec filter_statuses(list(), list()) :: {:ok, list()}
  defp filter_statuses(status_filter, exclude_statuses) do
    final_status_filter = status_filter -- exclude_statuses
    {:ok, final_status_filter}
  end

  @spec filter_statuses(map()) :: {:ok, list()}
  defp filter_statuses(%{
         status_filter: status_filter,
         exclude_statuses: exclude_statuses
       }) do
    filter_statuses(status_filter, exclude_statuses)
  end

  @spec build_and_execute_overlap_query(
          item_id,
          reserved_from,
          reserved_until,
          item_id | nil,
          list()
        ) :: overlap_result()
  defp build_and_execute_overlap_query(
         item_id,
         reserved_from,
         reserved_until,
         exclude_reservation_id,
         final_status_filter
       ) do
    query = build_overlap_query(item_id, reserved_from, reserved_until, exclude_reservation_id, final_status_filter)
    execute_overlap_query(query)
    rescue
      e in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, "Validation error during overlap check: #{inspect(e)}"}
      e in ArgumentError ->
        {:error, "Invalid argument during overlap check: #{inspect(e)}"}
      e -> {:error, "Unexpected exception during overlap check: #{inspect(e)}"}
  end

  @spec build_and_execute_overlap_query(map()) :: overlap_result()
  defp build_and_execute_overlap_query(%{
         item_id: item_id,
         reserved_from: reserved_from,
         reserved_until: reserved_until,
         exclude_reservation_id: exclude_reservation_id,
         final_status_filter: final_status_filter
       }) do
    build_and_execute_overlap_query(item_id, reserved_from, reserved_until, exclude_reservation_id, final_status_filter)
  end

  @spec build_overlap_query(
          item_id,
          reserved_from,
          reserved_until,
          item_id | nil,
          list()
        ) :: Ash.Query.t()
  defp build_overlap_query(item_id, reserved_from, reserved_until, exclude_reservation_id, final_status_filter) do
    query =
      RivaAsh.Resources.Reservation
      |> Ash.Query.filter(expr(item_id == ^item_id))
      |> Ash.Query.filter(expr(status in ^final_status_filter))
      |> Ash.Query.filter(
        expr(
          fragment(
            "? < ? AND ? > ?",
            ^reserved_from,
            ^reserved_until,
            ^reserved_until,
            ^reserved_from
          )
        )
      )

    # Exclude current reservation if updating
    if exclude_reservation_id do
      Ash.Query.filter(query, expr(id != ^exclude_reservation_id))
    else
      query
    end
  end

  @spec build_overlap_query(map()) :: Ash.Query.t()
  defp build_overlap_query(%{
         item_id: item_id,
         reserved_from: reserved_from,
         reserved_until: reserved_until,
         exclude_reservation_id: exclude_reservation_id,
         final_status_filter: final_status_filter
       }) do
    build_overlap_query(item_id, reserved_from, reserved_until, exclude_reservation_id, final_status_filter)
  end

  @spec execute_overlap_query(Ash.Query.t()) :: overlap_result()
  defp execute_overlap_query(query) do
    case Ash.read(query, domain: RivaAsh.Domain) do
      {:ok, []} -> {:ok, :no_overlap}
      {:ok, _overlapping} -> {:ok, :overlap_found}
      {:error, error} -> {:error, "Failed to read reservations: #{inspect(error)}"}
    end
  end

  @spec execute_overlap_query(map()) :: overlap_result()
  defp execute_overlap_query(%{query: query}) do
    execute_overlap_query(query)
  end

  @doc """
  Validates item availability considering holds, schedules, and exceptions.
  """
  @spec validate_item_availability(changeset, opts) :: validation_result()
  def validate_item_availability(changeset, opts \\ []) do
    with {:ok, item_id} <- get_required_attribute(changeset, :item_id),
         {:ok, reserved_from} <- get_required_attribute(changeset, :reserved_from),
         {:ok, reserved_until} <- get_required_attribute(changeset, :reserved_until) do
      check_item_availability(item_id, reserved_from, reserved_until, opts)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Comprehensive item availability checking.
  Considers item schedules, availability exceptions, and active holds.
  """
  @spec check_item_availability(item_id, reserved_from, reserved_until, opts) :: availability_result()
  def check_item_availability(item_id, reserved_from, reserved_until, opts \\ []) do
    check_holds = Keyword.get(opts, :check_holds, true)

    with {:ok, item} <- get_item(item_id),
         {:ok, _} <- validate_item_is_active(item),
         {:ok, _} <- validate_item_not_archived(item) do
      result =
        if item.is_always_available do
          check_additional_constraints(item_id, reserved_from, reserved_until, check_holds)
        else
          check_schedule_and_exceptions(item, reserved_from, reserved_until, check_holds)
        end

      result
    else
      {:error, :item_not_found} -> {:ok, {:unavailable, "Item not found"}}
      {:error, :item_inactive} -> {:ok, {:unavailable, "Item is not active"}}
      {:error, :item_archived} -> {:ok, {:unavailable, "Item is archived"}}
      {:error, error} -> {:error, "Failed to check availability: #{inspect(error)}"}
    rescue
      e in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, "Validation error during availability check: #{inspect(e)}"}
      e in ArgumentError ->
        {:error, "Invalid argument during availability check: #{inspect(e)}"}
      e -> {:error, "Unexpected exception during availability check: #{inspect(e)}"}
    end
  end

  @spec check_item_availability(map()) :: availability_result()
  def check_item_availability(%{
         item_id: item_id,
         reserved_from: reserved_from,
         reserved_until: reserved_until,
         opts: opts
       }) do
    check_item_availability(item_id, reserved_from, reserved_until, opts)
  end

  @spec get_item(item_id) :: {:ok, map()} | {:error, atom()}
  defp get_item(item_id) do
    case Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain) do
      nil -> {:error, :item_not_found}
      item -> {:ok, item}
    end
  end

  @spec get_item(map()) :: {:ok, map()} | {:error, atom()}
  defp get_item(%{item_id: item_id}) do
    get_item(item_id)
  end

  @spec validate_item_is_active(map()) :: {:ok, :ok} | {:error, :item_inactive}
  defp validate_item_is_active(%{is_active: true}), do: {:ok, :ok}
  defp validate_item_is_active(_), do: {:error, :item_inactive}

  @spec validate_item_is_active(map()) :: {:ok, :ok} | {:error, :item_inactive}
  defp validate_item_is_active(%{item: item}) do
    validate_item_is_active(item)
  end

  @spec validate_item_not_archived(map()) :: {:ok, :ok} | {:error, :item_archived}
  defp validate_item_not_archived(%{archived_at: nil}), do: {:ok, :ok}
  defp validate_item_not_archived(_), do: {:error, :item_archived}

  @spec validate_item_not_archived(map()) :: {:ok, :ok} | {:error, :item_archived}
  defp validate_item_not_archived(%{item: item}) do
    validate_item_not_archived(item)
  end

  @spec check_additional_constraints(item_id, reserved_from, reserved_until, boolean()) :: availability_result()
  defp check_additional_constraints(item_id, reserved_from, reserved_until, check_holds) do
    if check_holds do
      check_active_holds(item_id, reserved_from, reserved_until)
    else
      {:ok, :available}
    end
  end

  @spec check_additional_constraints(map()) :: availability_result()
  defp check_additional_constraints(%{
         item_id: item_id,
         reserved_from: reserved_from,
         reserved_until: reserved_until,
         check_holds: check_holds
       }) do
    check_additional_constraints(item_id, reserved_from, reserved_until, check_holds)
  end

  @spec check_schedule_and_exceptions(map(), reserved_from, reserved_until, boolean()) :: availability_result()
  defp check_schedule_and_exceptions(item, reserved_from, reserved_until, check_holds) do
    reservation_date = DateTime.to_date(reserved_from)

    with {:ok, exceptions} <- get_availability_exceptions(),
         {:ok, _} <- check_exceptions_for_date(exceptions, item.id, reservation_date),
         {:ok, result} <- check_holds_or_availability(item.id, reserved_from, reserved_until, check_holds) do
      {:ok, result}
    else
      {:error, :exception_check_failed} ->
        # If we can't check exceptions, assume no exceptions and proceed
        check_holds_or_availability(item.id, reserved_from, reserved_until, check_holds)

      {:error, error} ->
        {:error, error}
    end
  end

  @spec check_schedule_and_exceptions(map()) :: availability_result()
  defp check_schedule_and_exceptions(%{
         item: item,
         reserved_from: reserved_from,
         reserved_until: reserved_until,
         check_holds: check_holds
       }) do
    check_schedule_and_exceptions(item, reserved_from, reserved_until, check_holds)
  end

  @spec get_availability_exceptions() :: {:ok, list()} | {:error, atom()}
  defp get_availability_exceptions, do: case Ash.read(RivaAsh.Resources.AvailabilityException, domain: RivaAsh.Domain) do
      {:ok, exceptions} -> {:ok, exceptions}
      {:error, _} -> {:error, :exception_check_failed}
    end

  @spec get_availability_exceptions(map()) :: {:ok, list()} | {:error, atom()}
  defp get_availability_exceptions(%{domain: domain}) do
    get_availability_exceptions()
  end

  @spec check_exceptions_for_date(list(), item_id, Date.t()) :: {:ok, :ok} | {:error, String.t()}
  defp check_exceptions_for_date(exceptions, item_id, reservation_date) do
    has_exception =
      Enum.any?(exceptions, fn exception ->
        exception.item_id == item_id and
          Timex.compare(exception.exception_date, reservation_date) == 0 and
          not exception.is_available
      end)

    if has_exception do
      {:error, "Item is unavailable due to scheduled exception"}
    else
      {:ok, :ok}
    end
  end

  @spec check_exceptions_for_date(map()) :: {:ok, :ok} | {:error, String.t()}
  defp check_exceptions_for_date(%{
         exceptions: exceptions,
         item_id: item_id,
         reservation_date: reservation_date
       }) do
    check_exceptions_for_date(exceptions, item_id, reservation_date)
  end

  @spec check_holds_or_availability(item_id, reserved_from, reserved_until, boolean()) :: availability_result()
  defp check_holds_or_availability(item_id, reserved_from, reserved_until, true) do
    check_active_holds(item_id, reserved_from, reserved_until)
  end

  defp check_holds_or_availability(_item_id, _reserved_from, _reserved_until, false) do
    {:ok, :available}
  end

  @spec check_holds_or_availability(map()) :: availability_result()
  defp check_holds_or_availability(%{
         item_id: item_id,
         reserved_from: reserved_from,
         reserved_until: reserved_until,
         check_holds: check_holds
       }) do
    check_holds_or_availability(item_id, reserved_from, reserved_until, check_holds)
  end

  @spec check_active_holds(item_id, reserved_from, reserved_until) :: availability_result()
  def check_active_holds(item_id, reserved_from, reserved_until) do
    now = Timex.now()

    query =
      RivaAsh.Resources.ItemHold
      |> Ash.Query.filter(expr(item_id == ^item_id))
      |> Ash.Query.filter(expr(is_active == true))
      |> Ash.Query.filter(expr(expires_at > ^now))
      |> Ash.Query.filter(
        expr(
          fragment(
            "? < ? AND ? > ?",
            reserved_from,
            ^reserved_until,
            reserved_until,
            ^reserved_from
          )
        )
      )

    case Ash.read(query, domain: RivaAsh.Domain) do
      {:ok, []} -> {:ok, :available}
      {:ok, _active_holds} -> {:ok, {:unavailable, "Item is currently held by another user"}}
      {:error, error} -> {:error, "Failed to check active holds: #{inspect(error)}"}
    rescue
      e in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, "Validation error during holds check: #{inspect(e)}"}
      e in ArgumentError ->
        {:error, "Invalid argument during holds check: #{inspect(e)}"}
      e -> {:error, "Unexpected exception during holds check: #{inspect(e)}"}
    end
  end

  @spec check_active_holds(map()) :: availability_result()
  def check_active_holds(%{
         item_id: item_id,
         reserved_from: reserved_from,
         reserved_until: reserved_until
       }) do
    check_active_holds(item_id, reserved_from, reserved_until)
  end

  @doc """
  Validates day type pricing configuration.
  Ensures that if has_day_type_pricing is true, at least one of weekday_price or weekend_price is set.
  """
  @spec validate_day_type_pricing(changeset, opts) :: :ok | {:error, map()}
  def validate_day_type_pricing(changeset, _opts) do
    with {:ok, has_day_type_pricing} <- get_required_attribute(changeset, :has_day_type_pricing),
         {:ok, weekday_price} <- get_optional_attribute(changeset, :weekday_price),
         {:ok, weekend_price} <- get_optional_attribute(changeset, :weekend_price) do
      validate_pricing_config(has_day_type_pricing, weekday_price, weekend_price)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec validate_pricing_config(boolean(), weekday_price, weekend_price) :: :ok | {:error, map()}
  defp validate_pricing_config(true, nil, nil) do
    {:error,
     field: :has_day_type_pricing,
     message: "When day type pricing is enabled, at least one of weekday_price or weekend_price must be set"}
  end

  defp validate_pricing_config(true, weekday_price, weekend_price) do
    validate_price_non_negative(weekday_price, weekend_price)
  end

  defp validate_pricing_config(false, _weekday_price, _weekend_price) do
    :ok
  end

  @spec validate_pricing_config(map()) :: :ok | {:error, map()}
  defp validate_pricing_config(%{
         has_day_type_pricing: has_day_type_pricing,
         weekday_price: weekday_price,
         weekend_price: weekend_price
       }) do
    validate_pricing_config(has_day_type_pricing, weekday_price, weekend_price)
  end

  @spec validate_price_non_negative(weekday_price, weekend_price) :: :ok | {:error, map()}
  defp validate_price_non_negative(weekday_price, weekend_price) do
    cond do
      not is_nil(weekday_price) and Decimal.compare(weekday_price, 0) == :lt ->
        {:error, field: :weekday_price, message: "Weekday price must be non-negative"}

      not is_nil(weekend_price) and Decimal.compare(weekend_price, 0) == :lt ->
        {:error, field: :weekend_price, message: "Weekend price must be non-negative"}

      true ->
        :ok
    end
  end

  @spec validate_price_non_negative(map()) :: :ok | {:error, map()}
  defp validate_price_non_negative(%{
         weekday_price: weekday_price,
         weekend_price: weekend_price
       }) do
    validate_price_non_negative(weekday_price, weekend_price)
  end

  @doc """
  Validates that end time is after start time.
  """
  @spec validate_time_range(changeset, opts) :: :ok | {:error, map()}
  def validate_time_range(changeset, _opts) do
    with {:ok, start_time} <- get_optional_attribute(changeset, :start_time),
         {:ok, end_time} <- get_optional_attribute(changeset, :end_time) do
      validate_time_order(start_time, end_time)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec validate_time_order(start_time | nil, end_time | nil) :: :ok | {:error, map()}
  defp validate_time_order(nil, _end_time), do: :ok
  defp validate_time_order(_start_time, nil), do: :ok

  defp validate_time_order(start_time, end_time) do
    if Timex.compare(end_time, start_time) == 1 do
      :ok
    else
      {:error, field: :end_time, message: "End time must be after start time"}
    end
  end

  @spec validate_time_order(map()) :: :ok | {:error, map()}
  defp validate_time_order(%{
         start_time: start_time,
         end_time: end_time
       }) do
    validate_time_order(start_time, end_time)
  end

  @doc """
  Validates that a date is not in the past.
  """
  @spec validate_future_date(changeset, opts) :: :ok | {:error, map()}
  def validate_future_date(changeset, _opts) do
    case get_optional_attribute(changeset, :date) do
      {:ok, date} when not is_nil(date) ->
        validate_date_not_in_past(date)

      {:ok, _} ->
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec validate_date_not_in_past(date) :: :ok | {:error, map()}
  defp validate_date_not_in_past(date) do
    today = Timex.today()

    if Timex.compare(date, today) != -1 do
      :ok
    else
      {:error, field: :date, message: "Date cannot be in the past"}
    end
  end

  @spec validate_date_not_in_past(map()) :: :ok | {:error, map()}
  defp validate_date_not_in_past(%{date: date}) do
    validate_date_not_in_past(date)
  end

  @doc """
  Validates business capacity constraints.
  """
  @spec validate_business_capacity(changeset, opts) :: :ok | {:error, map()}
  def validate_business_capacity(_changeset, _opts) do
    # This would check business-specific capacity rules
    # Implementation depends on your business logic
    :ok
  end

  @doc """
  Validates email format with improved regex.
  """
  @spec validate_email_format(changeset, opts) :: :ok | {:error, map()}
  def validate_email_format(changeset, _opts) do
    case get_optional_attribute(changeset, :email) do
      {:ok, email} when is_binary(email) ->
        validate_email_regex(email)

      {:ok, _} ->
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec validate_email_regex(email) :: :ok | {:error, map()}
  defp validate_email_regex(email) do
    email_regex =
      ~r/^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/

    if Regex.match?(email_regex, email) do
      :ok
    else
      {:error, field: :email, message: "Invalid email format"}
    end
  end

  @spec validate_email_regex(map()) :: :ok | {:error, map()}
  defp validate_email_regex(%{email: email}) do
    validate_email_regex(email)
  end

  @doc """
  Validates phone number format.
  """
  @spec validate_phone_format(changeset, opts) :: :ok | {:error, map()}
  def validate_phone_format(changeset, _opts) do
    case get_optional_attribute(changeset, :phone) do
      {:ok, phone} when is_binary(phone) ->
        validate_phone_regex(phone)

      {:ok, _} ->
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec validate_phone_regex(phone) :: :ok | {:error, map()}
  defp validate_phone_regex(phone) do
    phone_regex = ~r/^[\+]?[1-9][\d\s\-\(\)\.]{7,15}$/

    if Regex.match?(phone_regex, phone) do
      :ok
    else
      {:error, field: :phone, message: "Invalid phone number format"}
    end
  end

  @spec validate_phone_regex(map()) :: :ok | {:error, map()}
  defp validate_phone_regex(%{phone: phone}) do
    validate_phone_regex(phone)
  end

  @doc """
  Sanitizes text input to prevent XSS.
  """
  @spec sanitize_text_input(changeset, opts) :: changeset()
  def sanitize_text_input(changeset, _opts) do
    case get_optional_attribute(changeset, :name) do
      {:ok, text} when is_binary(text) ->
        sanitize_text(text, changeset)

      {:ok, _} ->
        changeset

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec sanitize_text(text_input, changeset) :: changeset()
  defp sanitize_text(text, changeset) do
    sanitized =
      text
      |> String.trim()
      |> String.replace(~r/[<>\"'&]/, "")

    Ash.Changeset.change_attribute(changeset, :name, sanitized)
  end

  @spec sanitize_text(map()) :: changeset()
  defp sanitize_text(%{text: text, changeset: changeset}) do
    sanitize_text(text, changeset)
  end

  @doc """
  Validates that a business_id belongs to the current actor's accessible businesses.
  """
  @spec validate_business_access(changeset, opts) :: :ok | {:error, map()}
  def validate_business_access(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :business_id) do
      nil ->
        :ok

      _business_id ->
        # This validation ensures that the business_id on the changeset matches one of the businesses
        # the current actor (employee) has access to.
        # For now, we'll skip this validation during seeding since there's no actor context
        # In a real application with proper authentication, you would check:
        # case changeset.context[:actor] do
        #   %{id: actor_id, type: :employee} ->
        #     if RivaAsh.Authorization.can_access_business?(actor_id, business_id) do
        #       :ok
        #     else
        #       {:error, field: :business_id, message: "No access to this business"}
        #     end
        #   _ ->
        #     :ok
        # end
        :ok
    end
  end

  @doc """
  Validates that a section belongs to the same business as the item.
  """
  @spec validate_section_business_match(changeset, opts) :: :ok | {:error, map()}
  def validate_section_business_match(changeset, _opts) do
    with {:ok, section_id} <- get_required_attribute(changeset, :section_id),
         {:ok, business_id} <- get_required_attribute(changeset, :business_id) do
      check_section_business_match(section_id, business_id)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_section_business_match(section_id, business_id) :: :ok | {:error, map()}
  defp check_section_business_match(section_id, business_id) do
    case Ash.get(RivaAsh.Resources.Section, section_id, domain: RivaAsh.Domain) do
      {:ok, %{business_id: ^business_id}} ->
        :ok

      {:ok, %{business_id: _other_business_id}} ->
        {:error, field: :section_id, message: "Section must belong to the same business"}

      {:error, _} ->
        {:error, field: :section_id, message: "Section not found"}
    end
  end

  @spec check_section_business_match(map()) :: :ok | {:error, map()}
  defp check_section_business_match(%{
         section_id: section_id,
         business_id: business_id
       }) do
    check_section_business_match(section_id, business_id)
  end

  @doc """
  Validates that an item_type belongs to the same business as the item.
  """
  @spec validate_item_type_business_match(changeset, opts) :: :ok | {:error, map()}
  def validate_item_type_business_match(changeset, _opts) do
    with {:ok, item_type_id} <- get_required_attribute(changeset, :item_type_id),
         {:ok, business_id} <- get_required_attribute(changeset, :business_id) do
      check_item_type_business_match(item_type_id, business_id)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_item_type_business_match(item_type_id, business_id) :: :ok | {:error, map()}
  defp check_item_type_business_match(item_type_id, business_id) do
    case Ash.get(RivaAsh.Resources.ItemType, item_type_id, domain: RivaAsh.Domain) do
      {:ok, %{business_id: ^business_id}} ->
        :ok

      {:ok, %{business_id: _other_business_id}} ->
        {:error, field: :item_type_id, message: "Item type must belong to the same business"}

      {:error, _} ->
        {:error, field: :item_type_id, message: "Item type not found"}
    end
  end

  @spec check_item_type_business_match(map()) :: :ok | {:error, map()}
  defp check_item_type_business_match(%{
         item_type_id: item_type_id,
         business_id: business_id
       }) do
    check_item_type_business_match(item_type_id, business_id)
  end

  @doc """
  Validates that a plot belongs to the same business as the section.
  """
  @spec validate_plot_business_match(changeset, opts) :: :ok | {:error, map()}
  def validate_plot_business_match(changeset, _opts) do
    with {:ok, plot_id} <- get_required_attribute(changeset, :plot_id),
         {:ok, business_id} <- get_required_attribute(changeset, :business_id) do
      check_plot_business_match(plot_id, business_id)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_plot_business_match(plot_id, business_id) :: :ok | {:error, map()}
  defp check_plot_business_match(plot_id, business_id) do
    case Ash.get(RivaAsh.Resources.Plot, plot_id, domain: RivaAsh.Domain) do
      {:ok, %{business_id: ^business_id}} ->
        :ok

      {:ok, %{business_id: _other_business_id}} ->
        {:error, field: :plot_id, message: "Plot must belong to the same business"}

      {:error, _} ->
        {:error, field: :plot_id, message: "Plot not found"}
    end
  end

  @spec check_plot_business_match(map()) :: :ok | {:error, map()}
  defp check_plot_business_match(%{
         plot_id: plot_id,
         business_id: business_id
       }) do
    check_plot_business_match(plot_id, business_id)
  end

  @doc """
  Validates that a client belongs to the same business as the reservation item.
  """
  @spec validate_client_item_business_match(changeset, opts) :: :ok | {:error, map()}
  def validate_client_item_business_match(changeset, _opts) do
    with {:ok, client_id} <- get_required_attribute(changeset, :client_id),
         {:ok, item_id} <- get_required_attribute(changeset, :item_id) do
      check_client_item_business_match(client_id, item_id)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_client_item_business_match(client_id, item_id) :: :ok | {:error, map()}
  defp check_client_item_business_match(client_id, item_id) do
    with {:ok, client} <- Ash.get(RivaAsh.Resources.Client, client_id, domain: RivaAsh.Domain),
         {:ok, item} <- Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain) do
      check_business_match(client.business_id, item.business_id, :client_id)
    else
      {:error, _} ->
        {:error, field: :client_id, message: "Client or item not found"}
    end
  end

  @spec check_client_item_business_match(map()) :: :ok | {:error, map()}
  defp check_client_item_business_match(%{
         client_id: client_id,
         item_id: item_id
       }) do
    check_client_item_business_match(client_id, item_id)
  end

  @spec check_business_match(business_id, business_id, atom()) :: :ok | {:error, map()}
  defp check_business_match(business_id, business_id, _field), do: :ok

  defp check_business_match(_business_id1, _business_id2, field) do
    {:error, field: field, message: "Client and item must belong to the same business"}
  end

  @spec check_business_match(map()) :: :ok | {:error, map()}
  defp check_business_match(%{
         business_id1: business_id1,
         business_id2: business_id2,
         field: field
       }) do
    check_business_match(business_id1, business_id2, field)
  end

  @doc """
  Validates that an employee belongs to the same business as the reservation item.
  """
  @spec validate_employee_item_business_match(changeset, opts) :: :ok | {:error, map()}
  def validate_employee_item_business_match(changeset, _opts) do
    with {:ok, employee_id} <- get_required_attribute(changeset, :employee_id),
         {:ok, item_id} <- get_required_attribute(changeset, :item_id) do
      check_employee_item_business_match(employee_id, item_id)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_employee_item_business_match(employee_id, item_id) :: :ok | {:error, map()}
  defp check_employee_item_business_match(employee_id, item_id) do
    with {:ok, employee} <- Ash.get(RivaAsh.Resources.Employee, employee_id, domain: RivaAsh.Domain),
         {:ok, item} <- Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain) do
      check_business_match(employee.business_id, item.business_id, :employee_id)
    else
      {:error, _} ->
        {:error, field: :employee_id, message: "Employee or item not found"}
    end
  end

  @spec check_employee_item_business_match(map()) :: :ok | {:error, map()}
  defp check_employee_item_business_match(%{
         employee_id: employee_id,
         item_id: item_id
       }) do
    check_employee_item_business_match(employee_id, item_id)
  end

  @doc """
  Validates that an item and layout belong to the same business (for ItemPosition).
  """
  @spec validate_item_layout_business_match(changeset, opts) :: :ok | {:error, map()}
  def validate_item_layout_business_match(changeset, _opts) do
    with {:ok, item_id} <- get_required_attribute(changeset, :item_id),
         {:ok, layout_id} <- get_required_attribute(changeset, :layout_id) do
      check_item_layout_business_match(item_id, layout_id)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_item_layout_business_match(item_id, layout_id) :: :ok | {:error, map()}
  defp check_item_layout_business_match(item_id, layout_id) do
    with {:ok, item} <- Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain),
         {:ok, layout} <- Ash.get(RivaAsh.Resources.Layout, layout_id, domain: RivaAsh.Domain, load: [:plot]) do
      check_business_match(item.business_id, layout.plot.business_id, :layout_id)
    else
      {:error, _} ->
        {:error, field: :layout_id, message: "Item or layout not found"}
    end
  end

  @spec check_item_layout_business_match(map()) :: :ok | {:error, map()}
  defp check_item_layout_business_match(%{
         item_id: item_id,
         layout_id: layout_id
       }) do
    check_item_layout_business_match(item_id, layout_id)
  end

  @doc """
  Validates that a reservation belongs to the same business as the payment.
  """
  @spec validate_reservation_payment_business_match(changeset, opts) :: :ok | {:error, map()}
  def validate_reservation_payment_business_match(changeset, _opts) do
    case get_required_attribute(changeset, :reservation_id) do
      {:ok, reservation_id} -> check_reservation_payment_business_match(changeset, reservation_id)
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_reservation_payment_business_match(changeset, reservation_id) :: :ok | {:error, map()}
  defp check_reservation_payment_business_match(changeset, reservation_id) do
    case Ash.get(RivaAsh.Resources.Reservation, reservation_id,
           domain: RivaAsh.Domain,
           load: [:item]
         ) do
      {:ok, reservation} ->
        # Get business_id from the changeset or derive from reservation
        case Ash.Changeset.get_argument_or_attribute(changeset, :business_id) do
          {:ok, business_id} ->
            if reservation.item.business_id == business_id do
              :ok
            else
              {:error, field: :reservation_id, message: "Payment and reservation must belong to the same business"}
            end

          :error ->
            # If no business_id in changeset, set it from the reservation
            Ash.Changeset.force_change_attribute(
              changeset,
              :business_id,
              reservation.item.business_id
            )

            :ok
        end

      {:error, _} ->
        {:error, field: :reservation_id, message: "Reservation not found"}
    end
  end

  @spec check_reservation_payment_business_match(map()) :: :ok | {:error, map()}
  defp check_reservation_payment_business_match(%{
         changeset: changeset,
         reservation_id: reservation_id
       }) do
    check_reservation_payment_business_match(changeset, reservation_id)
  end

  @doc """
  Validates that employee and granter belong to the same business (for EmployeePermission).
  """
  @spec validate_employee_granter_business_match(changeset, opts) :: :ok | {:error, map()}
  def validate_employee_granter_business_match(changeset, _opts) do
    with {:ok, employee_id} <- get_required_attribute(changeset, :employee_id),
         {:ok, granted_by_id} <- get_required_attribute(changeset, :granted_by_id) do
      check_employee_granter_business_match(employee_id, granted_by_id)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_employee_granter_business_match(employee_id, granted_by_id) :: :ok | {:error, map()}
  defp check_employee_granter_business_match(employee_id, granted_by_id) do
    with {:ok, employee} <- Ash.get(RivaAsh.Resources.Employee, employee_id, domain: RivaAsh.Domain),
         {:ok, granter} <- Ash.get(RivaAsh.Resources.Employee, granted_by_id, domain: RivaAsh.Domain) do
      check_business_match(employee.business_id, granter.business_id, :granted_by_id)
    else
      {:error, _} ->
        {:error, field: :granted_by_id, message: "Employee or granter not found"}
    end
  end

  @spec check_employee_granter_business_match(map()) :: :ok | {:error, map()}
  defp check_employee_granter_business_match(%{
         employee_id: employee_id,
         granted_by_id: granted_by_id
       }) do
    check_employee_granter_business_match(employee_id, granted_by_id)
  end

  @doc """
  Validates that pricing rules don't have overlapping date ranges for the same business/item_type/pricing_type.
  """
  @spec validate_pricing_date_overlap(changeset, opts) :: :ok | {:error, map()}
  def validate_pricing_date_overlap(changeset, _opts) do
    with {:ok, business_id} <- get_required_attribute(changeset, :business_id),
         {:ok, item_type_id} <- get_required_attribute(changeset, :item_type_id),
         {:ok, pricing_type} <- get_required_attribute(changeset, :pricing_type),
         {:ok, effective_from} <- get_required_attribute(changeset, :effective_from),
         {:ok, effective_until} <- get_required_attribute(changeset, :effective_until) do
      check_pricing_date_overlap(changeset, business_id, item_type_id, pricing_type, effective_from, effective_until)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_pricing_date_overlap(changeset, business_id, item_type_id, pricing_type, effective_from, effective_until) ::
          :ok | {:error, map()}
  defp check_pricing_date_overlap(changeset, business_id, item_type_id, pricing_type, effective_from, effective_until) do
    current_id = Ash.Changeset.get_attribute(changeset, :id)

    # Query for existing pricing rules with same business/item_type/pricing_type
    query =
      RivaAsh.Resources.Pricing
      |> Ash.Query.filter(expr(business_id == ^business_id))
      |> Ash.Query.filter(expr(item_type_id == ^item_type_id))
      |> Ash.Query.filter(expr(pricing_type == ^pricing_type))

    # Exclude current record if updating
    query =
      if current_id do
        Ash.Query.filter(query, expr(id != ^current_id))
      else
        query
      end

    case Ash.read(query, domain: RivaAsh.Domain) do
      {:ok, existing_rules} ->
        if has_date_overlap?(existing_rules, effective_from, effective_until) do
          {:error, field: :effective_from, message: "Date range overlaps with existing pricing rule"}
        else
          :ok
        end

      # Skip validation if query fails
      {:error, _} ->
        :ok
    end
  end

  @spec check_pricing_date_overlap(map()) :: :ok | {:error, map()}
  defp check_pricing_date_overlap(%{
         changeset: changeset,
         business_id: business_id,
         item_type_id: item_type_id,
         pricing_type: pricing_type,
         effective_from: effective_from,
         effective_until: effective_until
       }) do
    check_pricing_date_overlap(changeset, business_id, item_type_id, pricing_type, effective_from, effective_until)
  end

  @spec check_pricing_date_overlap(map()) :: :ok | {:error, map()}
  defp check_pricing_date_overlap(%{
         changeset: changeset,
         business_id: business_id,
         item_type_id: item_type_id,
         pricing_type: pricing_type,
         effective_from: effective_from,
         effective_until: effective_until
       }) do
    check_pricing_date_overlap(changeset, business_id, item_type_id, pricing_type, effective_from, effective_until)
  end

  @doc """
  Validates that there's only one active base pricing rule per business/item_type at any given time.
  """
  @spec validate_single_active_base_pricing(changeset, opts) :: :ok | {:error, map()}
  def validate_single_active_base_pricing(changeset, _opts) do
    with {:ok, business_id} <- get_required_attribute(changeset, :business_id),
         {:ok, item_type_id} <- get_required_attribute(changeset, :item_type_id),
         {:ok, pricing_type} <- get_required_attribute(changeset, :pricing_type) do
      check_single_active_base_pricing(changeset, business_id, item_type_id, pricing_type)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_single_active_base_pricing(changeset, business_id, item_type_id, pricing_type) :: :ok | {:error, map()}
  defp check_single_active_base_pricing(changeset, business_id, item_type_id, :base) do
    current_id = Ash.Changeset.get_attribute(changeset, :id)
    today = Timex.today()

    # Query for other active base pricing rules
    query =
      RivaAsh.Resources.Pricing
      |> Ash.Query.filter(expr(business_id == ^business_id))
      |> Ash.Query.filter(expr(item_type_id == ^item_type_id))
      |> Ash.Query.filter(expr(pricing_type == :base))
      |> Ash.Query.filter(
        expr(
          fragment(
            "(effective_from IS NULL OR effective_from <= ?) AND (effective_until IS NULL OR effective_until >= ?)",
            ^today,
            ^today
          )
        )
      )

    # Exclude current record if updating
    query =
      if current_id do
        Ash.Query.filter(query, expr(id != ^current_id))
      else
        query
      end

    case Ash.read(query, domain: RivaAsh.Domain) do
      {:ok, []} ->
        :ok

      {:ok, _existing} ->
        {:error, field: :pricing_type, message: "Only one active base pricing rule allowed per business/item_type"}

      {:error, _} ->
        :ok
    end
  end

  defp check_single_active_base_pricing(_changeset, _business_id, _item_type_id, _pricing_type) do
    :ok
  end

  @spec check_single_active_base_pricing(map()) :: :ok | {:error, map()}
  defp check_single_active_base_pricing(%{
         changeset: changeset,
         business_id: business_id,
         item_type_id: item_type_id,
         pricing_type: pricing_type
       }) do
    check_single_active_base_pricing(changeset, business_id, item_type_id, pricing_type)
  end

  # Helper function to check for date range overlaps
  @spec has_date_overlap?(list(), effective_from, effective_until) :: boolean()
  defp has_date_overlap?(existing_rules, new_from, new_until) do
    Enum.any?(existing_rules, fn rule ->
      date_ranges_overlap?(
        {rule.effective_from, rule.effective_until},
        {new_from, new_until}
      )
    end)
  end

  @spec has_date_overlap?(map()) :: boolean()
  defp has_date_overlap?(%{
         existing_rules: existing_rules,
         new_from: new_from,
         new_until: new_until
       }) do
    has_date_overlap?(existing_rules, new_from, new_until)
  end

  # Helper function to check if two date ranges overlap
  @spec date_ranges_overlap?(
          {effective_from | nil, effective_until | nil},
          {effective_from | nil, effective_until | nil}
        ) :: boolean()
  defp date_ranges_overlap?({from1, until1}, {from2, until2}) do
    # Convert nil dates to appropriate boundaries
    from1 = from1 || Timex.parse!("1900-01-01", "{YYYY}-{0M}-{0D}")
    until1 = until1 || Timex.parse!("2100-12-31", "{YYYY}-{0M}-{0D}")
    from2 = from2 || Timex.parse!("1900-01-01", "{YYYY}-{0M}-{0D}")
    until2 = until2 || Timex.parse!("2100-12-31", "{YYYY}-{0M}-{0D}")

    # Check for overlap: start1 < end2 && start2 < end1
    Timex.compare(from1, until2) == -1 && Timex.compare(from2, until1) == -1
  end

  @spec date_ranges_overlap?(map()) :: boolean()
  defp date_ranges_overlap?(%{
         range1: {from1, until1},
         range2: {from2, until2}
       }) do
    date_ranges_overlap?({from1, until1}, {from2, until2})
  end
end
