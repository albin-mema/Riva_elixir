defmodule RivaAsh.Validations do
  @moduledoc """
  Shared validation functions for common business logic across resources.
  Provides consistent validation patterns and reusable validation functions.
  """

  @doc """
  Validates that a reservation doesn't conflict with existing reservations.
  """
  def validate_reservation_availability(changeset, _opts) do
    with {:ok, item_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :item_id),
         {:ok, reserved_from} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :reserved_from),
         {:ok, reserved_until} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :reserved_until) do

      # Check for overlapping reservations
      existing_reservations =
        RivaAsh.Resources.Reservation
        |> Ash.Query.filter(item_id == ^item_id)
        |> Ash.Query.filter(status in [:confirmed, :pending])
        |> Ash.Query.filter(
          (reserved_from < ^reserved_until and reserved_until > ^reserved_from)
        )
        |> Ash.read!()

      case existing_reservations do
        [] -> :ok
        _ -> {:error, field: :reserved_from, message: "Item is not available during the requested time"}
      end
    else
      :error -> :ok  # Skip validation if required fields are missing
    end
  end

  @doc """
  Validates that end time is after start time.
  """
  def validate_time_range(changeset, opts) do
    start_field = Keyword.get(opts, :start_field, :start_time)
    end_field = Keyword.get(opts, :end_field, :end_time)

    with {:ok, start_time} <- Ash.Changeset.fetch_argument_or_attribute(changeset, start_field),
         {:ok, end_time} <- Ash.Changeset.fetch_argument_or_attribute(changeset, end_field) do

      if DateTime.compare(end_time, start_time) == :gt do
        :ok
      else
        {:error, field: end_field, message: "End time must be after start time"}
      end
    else
      :error -> :ok  # Skip validation if fields are missing
    end
  end

  @doc """
  Validates that a date is not in the past.
  """
  def validate_future_date(changeset, opts) do
    field = Keyword.get(opts, :field, :date)

    case Ash.Changeset.fetch_argument_or_attribute(changeset, field) do
      {:ok, date} ->
        today = Date.utc_today()
        if Date.compare(date, today) != :lt do
          :ok
        else
          {:error, field: field, message: "Date cannot be in the past"}
        end
      :error -> :ok
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
  Validates that an item is active and available.
  """
  def validate_item_availability(changeset, _opts) do
    case Ash.Changeset.fetch_argument_or_attribute(changeset, :item_id) do
      {:ok, item_id} ->
        case RivaAsh.Resources.Item.by_id(item_id) do
          {:ok, item} ->
            if item.is_active do
              :ok
            else
              {:error, field: :item_id, message: "Item is not currently active"}
            end
          {:error, _} ->
            {:error, field: :item_id, message: "Item not found"}
        end
      :error -> :ok
    end
  end

  @doc """
  Validates email format with improved regex.
  """
  def validate_email_format(changeset, opts) do
    field = Keyword.get(opts, :field, :email)

    case Ash.Changeset.fetch_argument_or_attribute(changeset, field) do
      {:ok, email} when is_binary(email) ->
        # More comprehensive email validation
        email_regex = ~r/^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/

        if Regex.match?(email_regex, email) do
          :ok
        else
          {:error, field: field, message: "Invalid email format"}
        end
      {:ok, nil} -> :ok  # Allow nil if field is optional
      :error -> :ok
    end
  end

  @doc """
  Validates phone number format.
  """
  def validate_phone_format(changeset, opts) do
    field = Keyword.get(opts, :field, :phone)

    case Ash.Changeset.fetch_argument_or_attribute(changeset, field) do
      {:ok, phone} when is_binary(phone) ->
        # Allow various phone formats
        phone_regex = ~r/^[\+]?[1-9][\d\s\-\(\)\.]{7,15}$/

        if Regex.match?(phone_regex, phone) do
          :ok
        else
          {:error, field: field, message: "Invalid phone number format"}
        end
      {:ok, nil} -> :ok
      :error -> :ok
    end
  end

  @doc """
  Sanitizes text input to prevent XSS.
  """
  def sanitize_text_input(changeset, opts) do
    field = Keyword.get(opts, :field, :name)

    case Ash.Changeset.fetch_argument_or_attribute(changeset, field) do
      {:ok, text} when is_binary(text) ->
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
    case Ash.Changeset.fetch_argument_or_attribute(changeset, :business_id) do
      {:ok, business_id} ->
        actor = Ash.Changeset.get_context(changeset, :actor)

        if RivaAsh.Authorization.can_access_business?(actor, business_id) do
          :ok
        else
          {:error, field: :business_id, message: "Access denied to this business"}
        end
      :error -> :ok
    end
  end

  @doc """
  Validates that a section belongs to the same business as the item.
  """
  def validate_section_business_match(changeset, _opts) do
    with {:ok, section_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :section_id),
         {:ok, business_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :business_id) do

      case Ash.get(RivaAsh.Resources.Section, section_id, domain: RivaAsh.Domain) do
        {:ok, %{business_id: ^business_id}} ->
          :ok
        {:ok, %{business_id: _other_business_id}} ->
          {:error, field: :section_id, message: "Section must belong to the same business"}
        {:error, _} ->
          {:error, field: :section_id, message: "Section not found"}
      end
    else
      :error -> :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that an item_type belongs to the same business as the item.
  """
  def validate_item_type_business_match(changeset, _opts) do
    with {:ok, item_type_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :item_type_id),
         {:ok, business_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :business_id) do

      case Ash.get(RivaAsh.Resources.ItemType, item_type_id, domain: RivaAsh.Domain) do
        {:ok, %{business_id: ^business_id}} ->
          :ok
        {:ok, %{business_id: _other_business_id}} ->
          {:error, field: :item_type_id, message: "Item type must belong to the same business"}
        {:error, _} ->
          {:error, field: :item_type_id, message: "Item type not found"}
      end
    else
      :error -> :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that a plot belongs to the same business as the section.
  """
  def validate_plot_business_match(changeset, _opts) do
    with {:ok, plot_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :plot_id),
         {:ok, business_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :business_id) do

      case Ash.get(RivaAsh.Resources.Plot, plot_id, domain: RivaAsh.Domain) do
        {:ok, %{business_id: ^business_id}} ->
          :ok
        {:ok, %{business_id: _other_business_id}} ->
          {:error, field: :plot_id, message: "Plot must belong to the same business"}
        {:error, _} ->
          {:error, field: :plot_id, message: "Plot not found"}
      end
    else
      :error -> :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that a client belongs to the same business as the reservation item.
  """
  def validate_client_item_business_match(changeset, _opts) do
    with {:ok, client_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :client_id),
         {:ok, item_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :item_id) do

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
      :error -> :ok  # Skip validation if fields are not present
    end
  end

  @doc """
  Validates that an employee belongs to the same business as the reservation item.
  """
  def validate_employee_item_business_match(changeset, _opts) do
    with {:ok, employee_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :employee_id),
         {:ok, item_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :item_id) do

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
    with {:ok, item_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :item_id),
         {:ok, layout_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :layout_id) do

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
    with {:ok, reservation_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :reservation_id) do

      case Ash.get(RivaAsh.Resources.Reservation, reservation_id, domain: RivaAsh.Domain, load: [:item]) do
        {:ok, reservation} ->
          # Get business_id from the changeset or derive from reservation
          case Ash.Changeset.fetch_argument_or_attribute(changeset, :business_id) do
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
    with {:ok, employee_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :employee_id),
         {:ok, granted_by_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :granted_by_id) do

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
    with {:ok, business_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :business_id),
         {:ok, item_type_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :item_type_id),
         {:ok, pricing_type} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :pricing_type) do

      effective_from = Ash.Changeset.get_argument_or_attribute(changeset, :effective_from)
      effective_until = Ash.Changeset.get_argument_or_attribute(changeset, :effective_until)
      current_id = Ash.Changeset.get_data(changeset, :id)

      # Query for existing pricing rules with the same business/item_type/pricing_type
      query =
        RivaAsh.Resources.Pricing
        |> Ash.Query.filter(
          business_id == ^business_id and
          item_type_id == ^item_type_id and
          pricing_type == ^pricing_type and
          is_active == true
        )

      # Exclude current record if updating
      query = if current_id do
        Ash.Query.filter(query, id != ^current_id)
      else
        query
      end

      case Ash.read(query, domain: RivaAsh.Domain) do
        {:ok, existing_rules} ->
          if has_date_overlap?(existing_rules, effective_from, effective_until) do
            {:error, field: :effective_from, message: "Pricing rule dates overlap with existing rule"}
          else
            :ok
          end
        {:error, _} ->
          {:error, field: :business_id, message: "Error checking existing pricing rules"}
      end
    else
      :error -> :ok  # Skip validation if required fields are not present
    end
  end

  @doc """
  Validates that there's only one active base pricing rule per business/item_type at any given time.
  """
  def validate_single_active_base_pricing(changeset, _opts) do
    with {:ok, business_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :business_id),
         {:ok, item_type_id} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :item_type_id),
         {:ok, pricing_type} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :pricing_type),
         {:ok, is_active} <- Ash.Changeset.fetch_argument_or_attribute(changeset, :is_active) do

      # Only validate for base pricing rules that are being set to active
      if pricing_type == :base and is_active do
        current_id = Ash.Changeset.get_data(changeset, :id)

        query =
          RivaAsh.Resources.Pricing
          |> Ash.Query.filter(
            business_id == ^business_id and
            item_type_id == ^item_type_id and
            pricing_type == :base and
            is_active == true
          )

        # Exclude current record if updating
        query = if current_id do
          Ash.Query.filter(query, id != ^current_id)
        else
          query
        end

        case Ash.read(query, domain: RivaAsh.Domain) do
          {:ok, []} ->
            :ok  # No existing active base pricing
          {:ok, _existing_rules} ->
            {:error, field: :is_active, message: "Only one active base pricing rule allowed per business/item_type"}
          {:error, _} ->
            {:error, field: :business_id, message: "Error checking existing pricing rules"}
        end
      else
        :ok  # Not a base pricing rule or not being set to active
      end
    else
      :error -> :ok  # Skip validation if required fields are not present
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
    from1 = from1 || ~D[1900-01-01]
    until1 = until1 || ~D[2100-12-31]
    from2 = from2 || ~D[1900-01-01]
    until2 = until2 || ~D[2100-12-31]

    # Check for overlap: start1 < end2 && start2 < end1
    Date.compare(from1, until2) == :lt && Date.compare(from2, until1) == :lt
  end
end
