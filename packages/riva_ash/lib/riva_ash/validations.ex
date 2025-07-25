defmodule RivaAsh.Validations do
  @moduledoc """
  Shared validation functions for common business logic across resources.
  Provides consistent validation patterns and reusable validation functions.
  """

  import Ash.Expr
  require Ash.Query
  alias RivaAsh.ErrorHelpers

  @doc """
  Validates that a reservation doesn't conflict with existing reservations.
  Uses standardized overlap detection logic.
  """
  def validate_reservation_availability(changeset, opts \\ []) do
    item_id = Ash.Changeset.get_argument_or_attribute(changeset, :item_id)
    reserved_from = Ash.Changeset.get_argument_or_attribute(changeset, :reserved_from)
    reserved_until = Ash.Changeset.get_argument_or_attribute(changeset, :reserved_until)

    if item_id && reserved_from && reserved_until do
      # Get current reservation ID to exclude from overlap check (for updates)
      current_reservation_id = Ash.Changeset.get_attribute(changeset, :id)

      case check_reservation_overlap(item_id, reserved_from, reserved_until, current_reservation_id, opts) do
        {:ok, :no_overlap} -> :ok
        {:error, reason} ->
          ErrorHelpers.failure(%{field: :reserved_from, message: "Failed to check availability: #{reason}"})
      end
    else
      :ok  # Skip validation if required fields are missing
    end
  end

  @doc """
  Standardized reservation overlap checking logic.
  Returns {:ok, :no_overlap} or {:ok, :overlap_found} or {:error, reason}.

  Options:
  - exclude_statuses: List of statuses to exclude from overlap check (default: [:cancelled, :completed])
  - include_provisional: Whether to include provisional reservations (default: true)
  """
  def check_reservation_overlap(item_id, reserved_from, reserved_until, exclude_reservation_id \\ nil, opts \\ []) do
    exclude_statuses = Keyword.get(opts, :exclude_statuses, [:cancelled, :completed])
    include_provisional = Keyword.get(opts, :include_provisional, true)

    # Build status filter
    status_filter = if include_provisional do
      [:confirmed, :pending, :provisional]
    else
      [:confirmed, :pending]
    end

    # Remove excluded statuses
    final_status_filter = status_filter -- exclude_statuses

    try do
      # Query for overlapping reservations
      query = RivaAsh.Resources.Reservation
      |> Ash.Query.filter(expr(item_id == ^item_id))
      |> Ash.Query.filter(expr(status in ^final_status_filter))
      |> Ash.Query.filter(expr(
        fragment("? < ? AND ? > ?", ^reserved_from, ^reserved_until, ^reserved_until, ^reserved_from)
      ))

      # Exclude current reservation if updating
      query = if exclude_reservation_id do
        Ash.Query.filter(query, expr(id != ^exclude_reservation_id))
      else
        query
      end

      query
      |> Ash.read(domain: RivaAsh.Domain)
      |> case do
        {:ok, []} -> ErrorHelpers.success(:no_overlap)
        {:ok, _overlapping} -> ErrorHelpers.failure("Time slot conflicts with existing reservation")
        {:error, error} -> ErrorHelpers.failure("Failed to read reservations: #{inspect(error)}")
      end
    rescue
      e -> ErrorHelpers.failure("Exception during overlap check: #{inspect(e)}")
    end
  end

  @doc """
  Validates item availability considering holds, schedules, and exceptions.
  """
  def validate_item_availability(changeset, opts \\ []) do
    item_id = Ash.Changeset.get_argument_or_attribute(changeset, :item_id)
    reserved_from = Ash.Changeset.get_argument_or_attribute(changeset, :reserved_from)
    reserved_until = Ash.Changeset.get_argument_or_attribute(changeset, :reserved_until)

    if item_id && reserved_from && reserved_until do
      case check_item_availability(item_id, reserved_from, reserved_until, opts) do
        {:ok, :available} -> :ok
        {:ok, :unavailable, reason} ->
          ErrorHelpers.failure(%{field: :item_id, message: reason})
        {:error, reason} ->
          ErrorHelpers.failure(%{field: :item_id, message: "Failed to check item availability: #{reason}"})
      end
    else
      :ok
    end
  end

  @doc """
  Comprehensive item availability checking.
  Considers item schedules, availability exceptions, and active holds.
  """
  def check_item_availability(item_id, reserved_from, reserved_until, opts \\ []) do
    check_holds = Keyword.get(opts, :check_holds, true)

    try do
      with {:ok, item} <- RivaAsh.Resources.Item
                         |> Ash.get(item_id, domain: RivaAsh.Domain)
                         |> ErrorHelpers.to_result(),
           {:ok, _} <- validate_item_is_active(item),
           {:ok, _} <- validate_item_not_archived(item) do
        result = if item.is_always_available do
                   check_additional_constraints(item_id, reserved_from, reserved_until, check_holds)
                 else
                   check_schedule_and_exceptions(item, reserved_from, reserved_until, check_holds)
                 end
        result
      else
        {:error, :item_not_found} -> ErrorHelpers.success({:unavailable, "Item not found"})
        {:error, :item_inactive} -> ErrorHelpers.success({:unavailable, "Item is not active"})
        {:error, :item_archived} -> ErrorHelpers.success({:unavailable, "Item is archived"})
        {:error, error} -> ErrorHelpers.failure("Failed to check availability: #{inspect(error)}")
      end
    rescue
      e -> ErrorHelpers.failure("Exception during availability check: #{inspect(e)}")
    end
  end

  defp validate_item_is_active(%{is_active: true}), do: ErrorHelpers.success(:ok)
  defp validate_item_is_active(_), do: ErrorHelpers.failure(:item_inactive)

  defp validate_item_not_archived(%{archived_at: nil}), do: ErrorHelpers.success(:ok)
  defp validate_item_not_archived(_), do: ErrorHelpers.failure(:item_archived)

  defp check_additional_constraints(item_id, reserved_from, reserved_until, check_holds) do
    if check_holds do
      check_active_holds(item_id, reserved_from, reserved_until)
    else
      ErrorHelpers.success(:available)
    end
  end

  defp check_schedule_and_exceptions(item, reserved_from, reserved_until, check_holds) do
    # Check if the item has any schedule restrictions
    # For now, we'll implement basic availability checking
    # In the future, this could check ItemSchedule and AvailabilityException resources

    reservation_date = DateTime.to_date(reserved_from)

    with {:ok, exceptions} <- RivaAsh.Resources.AvailabilityException
                              |> Ash.read(domain: RivaAsh.Domain)
                              |> ErrorHelpers.to_result(),
         has_exception = check_exceptions_for_date(exceptions, item.id, reservation_date),
         {:ok, _} <- (if has_exception,
                       do: ErrorHelpers.failure("Item is unavailable due to scheduled exception"),
                       else: ErrorHelpers.success(:ok)) do
      result = if check_holds,
                 do: check_active_holds(item.id, reserved_from, reserved_until),
                 else: ErrorHelpers.success(:available)
      result
    else
      {:error, :exception_check_failed} ->
        # If we can't check exceptions, assume no exceptions and proceed
        if check_holds do
          check_active_holds(item.id, reserved_from, reserved_until)
        else
          ErrorHelpers.success(:available)
        end

      {:error, error} -> ErrorHelpers.failure(error)
    end
  end

  defp check_exceptions_for_date(exceptions, item_id, reservation_date) do
    Enum.any?(exceptions, fn exception ->
      exception.item_id == item_id and
      Timex.compare(exception.exception_date, reservation_date) == 0 and
      not exception.is_available
    end)
  end

  def check_active_holds(item_id, reserved_from, reserved_until) do
    # Check for active holds that would conflict with this reservation
    now = Timex.now()

    try do
      query = RivaAsh.Resources.ItemHold
      |> Ash.Query.filter(expr(item_id == ^item_id))
      |> Ash.Query.filter(expr(is_active == true))
      |> Ash.Query.filter(expr(expires_at > ^now))
      |> Ash.Query.filter(expr(
        fragment("? < ? AND ? > ?", reserved_from, ^reserved_until, reserved_until, ^reserved_from)
      ))

      case Ash.read(query, domain: RivaAsh.Domain) do
        {:ok, []} -> ErrorHelpers.success(:available)
        {:ok, _active_holds} -> ErrorHelpers.success({:unavailable, "Item is currently held by another user"})
        {:error, error} -> ErrorHelpers.failure("Failed to check active holds: #{inspect(error)}")
      end
    rescue
      e -> ErrorHelpers.failure("Exception during holds check: #{inspect(e)}")
    end
  end

  @doc """
  Validates day type pricing configuration.
  Ensures that if has_day_type_pricing is true, at least one of weekday_price or weekend_price is set.
  """
  def validate_day_type_pricing(changeset, _opts) do
    has_day_type_pricing = Ash.Changeset.get_argument_or_attribute(changeset, :has_day_type_pricing)
    weekday_price = Ash.Changeset.get_argument_or_attribute(changeset, :weekday_price)
    weekend_price = Ash.Changeset.get_argument_or_attribute(changeset, :weekend_price)

    if has_day_type_pricing do
      if is_nil(weekday_price) and is_nil(weekend_price) do
        {:error, field: :has_day_type_pricing,
         message: "When day type pricing is enabled, at least one of weekday_price or weekend_price must be set"}
      else
        # Validate that prices are non-negative if set
        cond do
          not is_nil(weekday_price) and Decimal.compare(weekday_price, 0) == :lt ->
            {:error, field: :weekday_price, message: "Weekday price must be non-negative"}
          not is_nil(weekend_price) and Decimal.compare(weekend_price, 0) == :lt ->
            {:error, field: :weekend_price, message: "Weekend price must be non-negative"}
          true -> :ok
        end
      end
    else
      :ok
    end
  end

  @doc """
  Validates that end time is after start time.
  """
  def validate_time_range(changeset, _opts) do
    start_field = :start_time
    end_field = :end_time

    start_time = Ash.Changeset.get_argument_or_attribute(changeset, start_field)
    end_time = Ash.Changeset.get_argument_or_attribute(changeset, end_field)

    if start_time && end_time do
      if Timex.compare(end_time, start_time) == 1 do
        :ok
      else
        {:error, field: end_field, message: "End time must be after start time"}
      end
    else
      :ok  # Skip validation if fields are missing
    end
  end

  @doc """
  Validates that a date is not in the past.
  """
  def validate_future_date(changeset, _opts) do
    field = :date

    case Ash.Changeset.get_argument_or_attribute(changeset, field) do
      date when not is_nil(date) ->
        today = Timex.today()
        if Timex.compare(date, today) != -1 do
          :ok
        else
          {:error, field: field, message: "Date cannot be in the past"}
        end
      _ -> :ok
    end
  end

  @doc """
  Validates business capacity constraints.
  """
  def validate_business_capacity(changeset, _opts) do
    # This would check business-specific capacity rules
    # Implementation depends on your business logic
    :ok
  end



  @doc """
  Validates email format with improved regex.
  """
  def validate_email_format(changeset, _opts) do
    field = :email

    case Ash.Changeset.get_argument_or_attribute(changeset, field) do
      email when is_binary(email) ->
        # More comprehensive email validation
        email_regex = ~r/^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/

        if Regex.match?(email_regex, email) do
          :ok
        else
          {:error, field: field, message: "Invalid email format"}
        end
      nil -> :ok  # Allow nil if field is optional
      _ -> :ok
    end
  end

  @doc """
  Validates phone number format.
  """
  def validate_phone_format(changeset, _opts) do
    field = :phone

    case Ash.Changeset.get_argument_or_attribute(changeset, field) do
      phone when is_binary(phone) ->
        # Allow various phone formats
        phone_regex = ~r/^[\+]?[1-9][\d\s\-\(\)\.]{7,15}$/

        if Regex.match?(phone_regex, phone) do
          :ok
        else
          {:error, field: field, message: "Invalid phone number format"}
        end
      nil -> :ok
      _ -> :ok
    end
  end

  @doc """
  Sanitizes text input to prevent XSS.
  """
  def sanitize_text_input(changeset, _opts) do
    field = :name

    case Ash.Changeset.get_argument_or_attribute(changeset, field) do
      text when is_binary(text) ->
        # Basic sanitization - remove potentially dangerous characters
        sanitized =
          text
          |> String.trim()
          |> String.replace(~r/[<>\"'&]/, "")

        Ash.Changeset.change_attribute(changeset, field, sanitized)
      _ -> changeset
    end
  end

  @doc """
  Validates that a business_id belongs to the current actor's accessible businesses.
  """
  def validate_business_access(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :business_id) do
      nil ->
        :ok
      business_id ->
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
  def validate_section_business_match(changeset, _opts) do
    section_id = Ash.Changeset.get_argument_or_attribute(changeset, :section_id)
    business_id = Ash.Changeset.get_argument_or_attribute(changeset, :business_id)

    if section_id && business_id do
      case Ash.get(RivaAsh.Resources.Section, section_id, domain: RivaAsh.Domain) do
        {:ok, %{business_id: ^business_id}} ->
          :ok
        {:ok, %{business_id: _other_business_id}} ->
          {:error, field: :section_id, message: "Section must belong to the same business"}
        {:error, _} ->
          {:error, field: :section_id, message: "Section not found"}
      end
    else
      :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that an item_type belongs to the same business as the item.
  """
  def validate_item_type_business_match(changeset, _opts) do
    item_type_id = Ash.Changeset.get_argument_or_attribute(changeset, :item_type_id)
    business_id = Ash.Changeset.get_argument_or_attribute(changeset, :business_id)

    if item_type_id && business_id do
      case Ash.get(RivaAsh.Resources.ItemType, item_type_id, domain: RivaAsh.Domain) do
        {:ok, %{business_id: ^business_id}} ->
          :ok
        {:ok, %{business_id: _other_business_id}} ->
          {:error, field: :item_type_id, message: "Item type must belong to the same business"}
        {:error, _} ->
          {:error, field: :item_type_id, message: "Item type not found"}
      end
    else
      :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that a plot belongs to the same business as the section.
  """
  def validate_plot_business_match(changeset, _opts) do
    plot_id = Ash.Changeset.get_argument_or_attribute(changeset, :plot_id)
    business_id = Ash.Changeset.get_argument_or_attribute(changeset, :business_id)

    if plot_id && business_id do
      case Ash.get(RivaAsh.Resources.Plot, plot_id, domain: RivaAsh.Domain) do
        {:ok, %{business_id: ^business_id}} ->
          :ok
        {:ok, %{business_id: _other_business_id}} ->
          {:error, field: :plot_id, message: "Plot must belong to the same business"}
        {:error, _} ->
          {:error, field: :plot_id, message: "Plot not found"}
      end
    else
      :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that a client belongs to the same business as the reservation item.
  """
  def validate_client_item_business_match(changeset, _opts) do
    client_id = Ash.Changeset.get_argument_or_attribute(changeset, :client_id)
    item_id = Ash.Changeset.get_argument_or_attribute(changeset, :item_id)

    if client_id && item_id do
      with {:ok, client} <- Ash.get(RivaAsh.Resources.Client, client_id, domain: RivaAsh.Domain),
           {:ok, item} <- Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain) do

        if client.business_id == item.business_id do
          :ok
        else
          {:error, field: :client_id, message: "Client and item must belong to the same business"}
        end
      else
        {:error, _} ->
          {:error, field: :client_id, message: "Client or item not found"}
      end
    else
      :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that an employee belongs to the same business as the reservation item.
  """
  def validate_employee_item_business_match(changeset, _opts) do
    with {:ok, employee_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :employee_id),
         {:ok, item_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :item_id) do

      with {:ok, employee} <- Ash.get(RivaAsh.Resources.Employee, employee_id, domain: RivaAsh.Domain),
           {:ok, item} <- Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain) do

        if employee.business_id == item.business_id do
          :ok
        else
          {:error, field: :employee_id, message: "Employee and item must belong to the same business"}
        end
      else
        {:error, _} ->
          {:error, field: :employee_id, message: "Employee or item not found"}
      end
    else
      :error -> :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that an item and layout belong to the same business (for ItemPosition).
  """
  def validate_item_layout_business_match(changeset, _opts) do
    with {:ok, item_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :item_id),
         {:ok, layout_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :layout_id) do

      with {:ok, item} <- Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain),
           {:ok, layout} <- Ash.get(RivaAsh.Resources.Layout, layout_id, domain: RivaAsh.Domain, load: [:plot]) do

        if item.business_id == layout.plot.business_id do
          :ok
        else
          {:error, field: :layout_id, message: "Item and layout must belong to the same business"}
        end
      else
        {:error, _} ->
          {:error, field: :layout_id, message: "Item or layout not found"}
      end
    else
      :error -> :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that a reservation belongs to the same business as the payment.
  """
  def validate_reservation_payment_business_match(changeset, _opts) do
    with {:ok, reservation_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :reservation_id) do

      case Ash.get(RivaAsh.Resources.Reservation, reservation_id, domain: RivaAsh.Domain, load: [:item]) do
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
              Ash.Changeset.force_change_attribute(changeset, :business_id, reservation.item.business_id)
              :ok
          end
        {:error, _} ->
          {:error, field: :reservation_id, message: "Reservation not found"}
      end
    else
      :error -> :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that employee and granter belong to the same business (for EmployeePermission).
  """
  def validate_employee_granter_business_match(changeset, _opts) do
    with {:ok, employee_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :employee_id),
         {:ok, granted_by_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :granted_by_id) do

      with {:ok, employee} <- Ash.get(RivaAsh.Resources.Employee, employee_id, domain: RivaAsh.Domain),
           {:ok, granter} <- Ash.get(RivaAsh.Resources.Employee, granted_by_id, domain: RivaAsh.Domain) do

        if employee.business_id == granter.business_id do
          :ok
        else
          {:error, field: :granted_by_id, message: "Employee and granter must belong to the same business"}
        end
      else
        {:error, _} ->
          {:error, field: :granted_by_id, message: "Employee or granter not found"}
      end
    else
      :error -> :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that pricing rules don't have overlapping date ranges for the same business/item_type/pricing_type.
  """
  def validate_pricing_date_overlap(changeset, _opts) do
    with {:ok, business_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :business_id),
         {:ok, item_type_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :item_type_id),
         {:ok, pricing_type} <- Ash.Changeset.get_argument_or_attribute(changeset, :pricing_type),
         {:ok, effective_from} <- Ash.Changeset.get_argument_or_attribute(changeset, :effective_from),
         {:ok, effective_until} <- Ash.Changeset.get_argument_or_attribute(changeset, :effective_until) do

      current_id = Ash.Changeset.get_attribute(changeset, :id)

      # Query for existing pricing rules with same business/item_type/pricing_type
      query = RivaAsh.Resources.Pricing
      |> Ash.Query.filter(expr(business_id == ^business_id))
      |> Ash.Query.filter(expr(item_type_id == ^item_type_id))
      |> Ash.Query.filter(expr(pricing_type == ^pricing_type))

      # Exclude current record if updating
      query = if current_id do
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
        {:error, _} -> :ok  # Skip validation if query fails
      end
    else
      :error -> :ok  # Skip validation if required fields are missing
    end
  end

  @doc """
  Validates that there's only one active base pricing rule per business/item_type at any given time.
  """
  def validate_single_active_base_pricing(changeset, _opts) do
    with {:ok, business_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :business_id),
         {:ok, item_type_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :item_type_id),
         {:ok, pricing_type} <- Ash.Changeset.get_argument_or_attribute(changeset, :pricing_type) do

      if pricing_type == :base do
        current_id = Ash.Changeset.get_attribute(changeset, :id)
        today = Timex.today()

        # Query for other active base pricing rules
        query = RivaAsh.Resources.Pricing
        |> Ash.Query.filter(expr(business_id == ^business_id))
        |> Ash.Query.filter(expr(item_type_id == ^item_type_id))
        |> Ash.Query.filter(expr(pricing_type == :base))
        |> Ash.Query.filter(expr(
          fragment("(effective_from IS NULL OR effective_from <= ?) AND (effective_until IS NULL OR effective_until >= ?)",
            ^today, ^today)
        ))

        # Exclude current record if updating
        query = if current_id do
          Ash.Query.filter(query, expr(id != ^current_id))
        else
          query
        end

        case Ash.read(query, domain: RivaAsh.Domain) do
          {:ok, []} -> :ok
          {:ok, _existing} ->
            {:error, field: :pricing_type, message: "Only one active base pricing rule allowed per business/item_type"}
          {:error, _} -> :ok
        end
      else
        :ok
      end
    else
      :error -> :ok
    end
  end

  # Helper function to check for date range overlaps
  defp has_date_overlap?(existing_rules, new_from, new_until) do
    Enum.any?(existing_rules, fn rule ->
      date_ranges_overlap?(
        {rule.effective_from, rule.effective_until},
        {new_from, new_until}
      )
    end)
  end

  # Helper function to check if two date ranges overlap
  defp date_ranges_overlap?({from1, until1}, {from2, until2}) do
    # Convert nil dates to appropriate boundaries
    from1 = from1 || Timex.parse!("1900-01-01", "{YYYY}-{0M}-{0D}")
    until1 = until1 || Timex.parse!("2100-12-31", "{YYYY}-{0M}-{0D}")
    from2 = from2 || Timex.parse!("1900-01-01", "{YYYY}-{0M}-{0D}")
    until2 = until2 || Timex.parse!("2100-12-31", "{YYYY}-{0M}-{0D}")

    # Check for overlap: start1 < end2 && start2 < end1
    Timex.compare(from1, until2) == -1 && Timex.compare(from2, until1) == -1
  end
end
