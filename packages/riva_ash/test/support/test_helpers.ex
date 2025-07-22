defmodule RivaAsh.TestHelpers do
  @moduledoc """
  Test helpers for RivaAsh application.
  """

  alias RivaAsh.Repo

  @doc """
  Creates a test business with default attributes.
  """
  def create_business(attrs \\ %{}) do
    default_attrs = %{
      name: "Test Business",
      description: "A test business",
      email: "test@business.com",
      phone: "123-456-7890",
      address: "123 Test St",
      timezone: "UTC"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Business{}
    |> RivaAsh.Business.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test user with default attributes.
  """
  def create_user(attrs \\ %{}) do
    default_attrs = %{
      name: "Test User",
      email: "test@user.com",
      password: "password123",
      password_confirmation: "password123"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.User{}
    |> RivaAsh.User.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test service with default attributes.
  """
  def create_service(business_id, attrs \\ %{}) do
    default_attrs = %{
      name: "Test Service",
      description: "A test service",
      duration: 60,
      price: 100.00,
      business_id: business_id
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Service{}
    |> RivaAsh.Service.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test booking with default attributes.
  """
  def create_booking(user_id, business_id, service_id, attrs \\ %{}) do
    default_attrs = %{
      user_id: user_id,
      business_id: business_id,
      service_id: service_id,
      start_time: DateTime.utc_now() |> DateTime.add(3600, :second),
      end_time: DateTime.utc_now() |> DateTime.add(7200, :second),
      status: :pending
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Booking{}
    |> RivaAsh.Booking.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test reservation with default attributes.
  """
  def create_reservation(user_id, business_id, attrs \\ %{}) do
    default_attrs = %{
      user_id: user_id,
      business_id: business_id,
      start_date: Date.utc_today(),
      end_date: Date.add(Date.utc_today(), 7),
      frequency: :weekly,
      status: :active
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.RecurringReservation{}
    |> RivaAsh.RecurringReservation.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test availability slot with default attributes.
  """
  def create_availability(business_id, attrs \\ %{}) do
    default_attrs = %{
      business_id: business_id,
      start_time: ~T[09:00:00],
      end_time: ~T[17:00:00],
      day_of_week: 1,
      is_available: true
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Availability{}
    |> RivaAsh.Availability.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test blocked time slot with default attributes.
  """
  def create_blocked_time(business_id, attrs \\ %{}) do
    start_time = DateTime.utc_now() |> DateTime.add(3600, :second)
    end_time = DateTime.utc_now() |> DateTime.add(7200, :second)

    default_attrs = %{
      business_id: business_id,
      start_time: start_time,
      end_time: end_time,
      reason: "Test block"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.BlockedTime{}
    |> RivaAsh.BlockedTime.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test change record with default attributes.
  """
  def create_change(resource_id, attrs \\ %{}) do
    default_attrs = %{
      resource_id: resource_id,
      change_type: :update,
      changes: %{name: "Updated Name"},
      user_id: "user-123"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Change{}
    |> RivaAsh.Change.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test permission with default attributes.
  """
  def create_permission(resource_id, user_id, attrs \\ %{}) do
    default_attrs = %{
      resource_id: resource_id,
      user_id: user_id,
      permission: :read
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Permission{}
    |> RivaAsh.Permission.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test role with default attributes.
  """
  def create_role(user_id, role_name \\ :business_owner) do
    %RivaAsh.Role{}
    |> RivaAsh.Role.changeset(%{
      user_id: user_id,
      role: role_name
    })
    |> Repo.insert!()
  end

  @doc """
  Creates a test validation with default attributes.
  """
  def create_validation(resource_id, attrs \\ %{}) do
    default_attrs = %{
      resource_id: resource_id,
      validation_type: :required,
      field: "name",
      value: "Test Value",
      is_valid: true
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Validation{}
    |> RivaAsh.Validation.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test database health record with default attributes.
  """
  def create_database_health(attrs \\ %{}) do
    default_attrs = %{
      check_name: "test_check",
      status: :healthy,
      message: "All systems operational",
      last_checked: DateTime.utc_now()
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.DatabaseHealth{}
    |> RivaAsh.DatabaseHealth.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test mermaid diagram with default attributes.
  """
  def create_mermaid_diagram(attrs \\ %{}) do
    default_attrs = %{
      name: "Test Diagram",
      type: "flowchart",
      content: "A-->B",
      metadata: %{}
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Mermaid{}
    |> RivaAsh.Mermaid.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test query with default attributes.
  """
  def create_query(attrs \\ %{}) do
    default_attrs = %{
      name: "Test Query",
      query: "SELECT * FROM users",
      parameters: %{},
      result_count: 0
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Query{}
    |> RivaAsh.Query.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test recurring reservation with default attributes.
  """
  def create_recurring_reservation(user_id, business_id, attrs \\ %{}) do
    default_attrs = %{
      user_id: user_id,
      business_id: business_id,
      start_date: Date.utc_today(),
      end_date: Date.add(Date.utc_today(), 30),
      frequency: :weekly,
      day_of_week: 1,
      time_slot: ~T[09:00:00],
      status: :active
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.RecurringReservation{}
    |> RivaAsh.RecurringReservation.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test error with default attributes.
  """
  def create_error(attrs \\ %{}) do
    default_attrs = %{
      error_type: "test_error",
      message: "Test error message",
      context: %{test: true},
      severity: :low
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.Error{}
    |> RivaAsh.Error.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test datetime helper record with default attributes.
  """
  def create_datetime_helper(attrs \\ %{}) do
    default_attrs = %{
      timezone: "UTC",
      format: "iso8601",
      offset: 0
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.DateTimeHelper{}
    |> RivaAsh.DateTimeHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test resource helper record with default attributes.
  """
  def create_resource_helper(attrs \\ %{}) do
    default_attrs = %{
      resource_type: "test",
      resource_id: "test-123",
      helper_type: "validation",
      data: %{}
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.ResourceHelper{}
    |> RivaAsh.ResourceHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test validation helper record with default attributes.
  """
  def create_validation_helper(attrs \\ %{}) do
    default_attrs = %{
      field: "test_field",
      validator: "required",
      options: %{},
      is_valid: true
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.ValidationHelper{}
    |> RivaAsh.ValidationHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test permission helper record with default attributes.
  """
  def create_permission_helper(attrs \\ %{}) do
    default_attrs = %{
      action: "read",
      resource: "test_resource",
      conditions: %{},
      is_allowed: true
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.PermissionHelper{}
    |> RivaAsh.PermissionHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test authorization helper record with default attributes.
  """
  def create_authorization_helper(attrs \\ %{}) do
    default_attrs = %{
      user_id: "user-123",
      resource_id: "resource-123",
      action: "read",
      is_authorized: true
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.AuthorizationHelper{}
    |> RivaAsh.AuthorizationHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test availability helper record with default attributes.
  """
  def create_availability_helper(attrs \\ %{}) do
    default_attrs = %{
      business_id: "business-123",
      date: Date.utc_today(),
      slots: [],
      is_available: true
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.AvailabilityHelper{}
    |> RivaAsh.AvailabilityHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test booking helper record with default attributes.
  """
  def create_booking_helper(attrs \\ %{}) do
    default_attrs = %{
      booking_id: "booking-123",
      status: "pending",
      price: 100.00,
      duration: 60
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.BookingHelper{}
    |> RivaAsh.BookingHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test changes helper record with default attributes.
  """
  def create_changes_helper(attrs \\ %{}) do
    default_attrs = %{
      change_type: "update",
      resource_id: "resource-123",
      changes: %{},
      user_id: "user-123"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.ChangesHelper{}
    |> RivaAsh.ChangesHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test database health helper record with default attributes.
  """
  def create_database_health_helper(attrs \\ %{}) do
    default_attrs = %{
      check_name: "test_check",
      status: "healthy",
      message: "Test message",
      last_checked: DateTime.utc_now()
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.DatabaseHealthHelper{}
    |> RivaAsh.DatabaseHealthHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test mermaid helper record with default attributes.
  """
  def create_mermaid_helper(attrs \\ %{}) do
    default_attrs = %{
      diagram_type: "flowchart",
      content: "A-->B",
      metadata: %{}
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.MermaidHelper{}
    |> RivaAsh.MermaidHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test queries helper record with default attributes.
  """
  def create_queries_helper(attrs \\ %{}) do
    default_attrs = %{
      query_type: "select",
      table: "users",
      conditions: %{},
      order_by: "id"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.QueriesHelper{}
    |> RivaAsh.QueriesHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test permissions helper record with default attributes.
  """
  def create_permissions_helper(attrs \\ %{}) do
    default_attrs = %{
      user_id: "user-123",
      resource_id: "resource-123",
      permission: "read",
      is_granted: true
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.PermissionsHelper{}
    |> RivaAsh.PermissionsHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test recurring reservations helper record with default attributes.
  """
  def create_recurring_reservations_helper(attrs \\ %{}) do
    default_attrs = %{
      user_id: "user-123",
      business_id: "business-123",
      start_date: Date.utc_today(),
      end_date: Date.add(Date.utc_today(), 30),
      frequency: "weekly"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.RecurringReservationsHelper{}
    |> RivaAsh.RecurringReservationsHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test release helper record with default attributes.
  """
  def create_release_helper(attrs \\ %{}) do
    default_attrs = %{
      version: "1.0.0",
      environment: "test",
      status: "deployed"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.ReleaseHelper{}
    |> RivaAsh.ReleaseHelper.changeset(attrs)
    |> Repo.insert!()
  end

  @doc """
  Creates a test validations helper record with default attributes.
  """
  def create_validations_helper(attrs \\ %{}) do
    default_attrs = %{
      field: "test_field",
      validator: "required",
      is_valid: true,
      message: "Field is required"
    }

    attrs = Map.merge(default_attrs, attrs)

    %RivaAsh.ValidationsHelper{}
    |> RivaAsh.ValidationsHelper.changeset(attrs)
    |> Repo.insert!()
  end
end
