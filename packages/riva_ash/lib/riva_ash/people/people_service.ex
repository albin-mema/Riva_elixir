defmodule RivaAsh.People.PeopleService do
  @moduledoc """
  Service module for managing People business logic.
  This module encapsulates all business logic related to people management,
  including clients and employees, keeping it separate from the LiveView UI concerns.
  """

  require Logger

  alias RivaAsh.Repo
  alias RivaAsh.Resources.{Business, Client, Employee}
  alias Ash.Query

  @doc """
  Get all people data for the current user.
  """
  def get_user_people(user) do
    # Get user's businesses first
    businesses = Business.read!(actor: user)
    business_ids = Enum.map(businesses, & &1.id)

    # Get clients for user's businesses
    clients = Client.read!(actor: user, filter: [business_id: [in: business_ids]])

    # Get employees for user's businesses
    employees = Employee.read!(actor: user, filter: [business_id: [in: business_ids]])

    # Get system users count (placeholder)
    system_users_count = get_system_users_count(user)

    {:ok, {businesses, clients, employees, system_users_count}}
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      Logger.error("Failed to get user people: #{inspect(error)}")
      {:error, :forbidden}

    error ->
      Logger.error("Unexpected error in get_user_people: #{inspect(error)}")
      {:error, :unexpected_error}
  end

  @doc """
  Search people by name, email, or phone.
  """
  def search_people(search_term, user) do
    # Get user's businesses first
    businesses = Business.read!(actor: user)
    business_ids = Enum.map(businesses, & &1.id)

    # Search clients
    clients_query =
      Client
      |> Query.filter(business_id: [in: business_ids])
      |> Query.or([
        [name: search_term],
        [email: search_term],
        [phone: search_term]
      ])
      |> Query.load([:business])

    clients = Client.read!(actor: user, query: clients_query)

    # Search employees
    employees_query =
      Employee
      |> Query.filter(business_id: [in: business_ids])
      |> Query.or([
        [name: search_term],
        [email: search_term],
        [phone: search_term]
      ])
      |> Query.load([:business])

    employees = Employee.read!(actor: user, query: employees_query)

    {:ok, {clients, employees}}
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      Logger.error("Failed to search people: #{inspect(error)}")
      {:error, :forbidden}

    error ->
      Logger.error("Unexpected error in search_people: #{inspect(error)}")
      {:error, :unexpected_error}
  end

  @doc """
  Filter people by business.
  """
  def filter_people_by_business(business_id, user) do
    # Validate business belongs to user
    businesses = Business.read!(actor: user)
    user_business_ids = Enum.map(businesses, & &1.id)

    unless business_id in user_business_ids do
      {:error, :forbidden}
    else
      # Get clients for the selected business
      clients = Client.read!(actor: user, filter: [business_id: business_id])

      # Get employees for the selected business
      employees = Employee.read!(actor: user, filter: [business_id: business_id])

      {:ok, {clients, employees}}
    end
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      Logger.error("Failed to filter people by business: #{inspect(error)}")
      {:error, :forbidden}

    error ->
      Logger.error("Unexpected error in filter_people_by_business: #{inspect(error)}")
      {:error, :unexpected_error}
  end

  @doc """
  Export contacts for the user's businesses.
  """
  def export_contacts(user) do
    # Get user's businesses first
    businesses = Business.read!(actor: user)
    business_ids = Enum.map(businesses, & &1.id)

    # Get all clients and employees
    clients = Client.read!(actor: user, filter: [business_id: [in: business_ids]])
    employees = Employee.read!(actor: user, filter: [business_id: [in: business_ids]])

    # Prepare export data
    export_data = %{
      businesses: businesses,
      clients: clients,
      employees: employees,
      exported_at: DateTime.utc_now(),
      exported_by: user.name
    }

    # Generate CSV or other export format
    case generate_export_file(export_data) do
      {:ok, file_path} ->
        {:ok, file_path}

      {:error, reason} ->
        Logger.error("Failed to generate export file: #{inspect(reason)}")
        {:error, reason}
    end
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      Logger.error("Failed to export contacts: #{inspect(error)}")
      {:error, :forbidden}

    error ->
      Logger.error("Unexpected error in export_contacts: #{inspect(error)}")
      {:error, :unexpected_error}
  end

  @doc """
  Create a new client.
  """
  def create_client(attrs, user) do
    try do
      case Ash.create(Client,
             attributes:
               Map.merge(attrs, %{
                 business_id: user.business_id,
                 status: :active
               }),
             authorize?: true
           ) do
        {:ok, client} ->
          {:ok, client}

        {:error, reason} ->
          Logger.error("Failed to create client: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in create_client: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Create a new employee.
  """
  def create_employee(attrs, user) do
    try do
      case Ash.create(Employee,
             attributes:
               Map.merge(attrs, %{
                 business_id: user.business_id,
                 status: :active
               }),
             authorize?: true
           ) do
        {:ok, employee} ->
          {:ok, employee}

        {:error, reason} ->
          Logger.error("Failed to create employee: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in create_employee: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Update a client.
  """
  def update_client(id, attrs, user) do
    try do
      case Ash.get(Client, id, authorize?: true) do
        {:ok, client} ->
          if client.business_id == user.business_id do
            case Ash.update(client, attributes: attrs, authorize?: true) do
              {:ok, updated_client} ->
                {:ok, updated_client}

              {:error, reason} ->
                Logger.error("Failed to update client: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get client for update: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in update_client: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Update an employee.
  """
  def update_employee(id, attrs, user) do
    try do
      case Ash.get(Employee, id, authorize?: true) do
        {:ok, employee} ->
          if employee.business_id == user.business_id do
            case Ash.update(employee, attributes: attrs, authorize?: true) do
              {:ok, updated_employee} ->
                {:ok, updated_employee}

              {:error, reason} ->
                Logger.error("Failed to update employee: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get employee for update: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in update_employee: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Delete a person (client or employee).
  """
  def delete_person(id, type, user) do
    try do
      with {:ok, record} <- get_person_record(id, type, user),
           true <- record.business_id == user.business_id || {:error, :forbidden},
           {:ok, deleted_record} <- Ash.destroy(record, authorize?: true) do
        {:ok, deleted_record}
      else
        {:error, reason} ->
          Logger.error("Failed to get person for deletion: #{inspect(reason)}")
          {:error, reason}

        {:error, :forbidden} ->
          {:error, :forbidden}

        _ ->
          {:error, :invalid_person_type}
      end
    rescue
      error ->
        Logger.error("Unexpected error in delete_person: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  # Private helper functions
  defp get_person_record(id, "client", user) do
    case Ash.get(Client, id, authorize?: true) do
      {:ok, client} -> {:ok, client}
      {:error, reason} -> {:error, reason}
    end
  end

  defp get_person_record(id, "employee", user) do
    case Ash.get(Employee, id, authorize?: true) do
      {:ok, employee} -> {:ok, employee}
      {:error, reason} -> {:error, reason}
    end
  end

  defp get_person_record(_, _, _) do
    {:error, :invalid_person_type}
  end

  @doc """
  Get a single person by ID and type.
  """
  def get_person(id, type, user) do
    try do
      case type do
        "client" ->
          case Ash.get(Client, id, authorize?: true) do
            {:ok, client} ->
              if client.business_id == user.business_id do
                {:ok, Map.put(client, :type, "client")}
              else
                {:error, :forbidden}
              end

            {:error, reason} ->
              Logger.error("Failed to get client: #{inspect(reason)}")
              {:error, reason}
          end

        "employee" ->
          case Ash.get(Employee, id, authorize?: true) do
            {:ok, employee} ->
              if employee.business_id == user.business_id do
                {:ok, Map.put(employee, :type, "employee")}
              else
                {:error, :forbidden}
              end

            {:error, reason} ->
              Logger.error("Failed to get employee: #{inspect(reason)}")
              {:error, reason}
          end

        _ ->
          {:error, :invalid_person_type}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_person: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Validate client attributes.
  """
  def validate_client_attrs(attrs) do
    attrs
    |> validate_required_field(:name, "Name is required")
    |> validate_required_field(:business_id, "Business ID is required")
    |> validate_optional_email()
    |> validate_optional_phone()
    |> case do
      [] -> {:ok, attrs}
      errors -> {:error, errors}
    end
  end

  @doc """
  Validate employee attributes.
  """
  def validate_employee_attrs(attrs) do
    attrs
    |> validate_required_field(:name, "Name is required")
    |> validate_required_field(:business_id, "Business ID is required")
    |> validate_required_field(:role, "Role is required")
    |> validate_optional_email()
    |> validate_optional_phone()
    |> case do
      [] -> {:ok, attrs}
      errors -> {:error, errors}
    end
  end

  # Private helper functions for validation
  defp validate_required_field(attrs, field, error_message) do
    case Map.get(attrs, field) do
      value when is_binary(value) and byte_size(value) > 0 ->
        []

      _ ->
        [error_message]
    end
  end

  defp validate_optional_email(attrs) do
    case Map.get(attrs, :email) do
      email when is_binary(email) and byte_size(email) > 0 ->
        if String.contains?(email, "@") do
          attrs
        else
          attrs ++ ["Email must contain @ symbol"]
        end

      _ ->
        attrs
    end
  end

  defp validate_optional_phone(attrs) do
    case Map.get(attrs, :phone) do
      phone when is_binary(phone) and byte_size(phone) > 0 ->
        attrs

      _ ->
        attrs
    end
  end

  @doc """
  Get people statistics for a business.
  """
  def get_people_stats(business_id) do
    # Get client counts
    total_clients_query =
      Client
      |> Query.filter(business_id: business_id)
      |> Query.aggregate(:count, :id)

    active_clients_query =
      Client
      |> Query.filter(business_id: business_id, status: :active)
      |> Query.aggregate(:count, :id)

    # Get employee counts
    total_employees_query =
      Employee
      |> Query.filter(business_id: business_id)
      |> Query.aggregate(:count, :id)

    active_employees_query =
      Employee
      |> Query.filter(business_id: business_id, status: :active)
      |> Query.aggregate(:count, :id)

    try do
      with {:ok, [%{count: total_clients}]} <- Ash.read(total_clients_query),
           {:ok, [%{count: active_clients}]} <- Ash.read(active_clients_query),
           {:ok, [%{count: total_employees}]} <- Ash.read(total_employees_query),
           {:ok, [%{count: active_employees}]} <- Ash.read(active_employees_query) do
        stats = %{
          total_clients: total_clients,
          active_clients: active_clients,
          total_employees: total_employees,
          active_employees: active_employees
        }

        {:ok, stats}
      else
        {:error, reason} ->
          Logger.error("Failed to get people stats: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_people_stats: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  # Private helper functions
  defp get_system_users_count(_user) do
    # Placeholder implementation
    # In a real system, this would query the actual system users
    5
  end

  defp generate_export_file(export_data) do
    # Generate CSV content
    csv_content = generate_csv_content(export_data)

    # Write to temporary file
    file_path = "/tmp/contacts_export_#{DateTime.to_unix(DateTime.utc_now())}.csv"
    File.write(file_path, csv_content)

    {:ok, file_path}
  rescue
    error ->
      Logger.error("Failed to generate export file: #{inspect(error)}")
      {:error, :file_generation_failed}
  end

  defp generate_csv_content(export_data) do
    # Generate CSV header
    header = "Type,Name,Email,Phone,Business,Status,Role\n"

    # Generate client rows
    client_rows =
      export_data.clients
      |> Enum.map(fn client ->
        "Client,#{client.name},#{client.email || ""},#{client.phone || ""},#{client.business.name},#{client.status},\n"
      end)
      |> Enum.join("")

    # Generate employee rows
    employee_rows =
      export_data.employees
      |> Enum.map(fn employee ->
        "Employee,#{employee.name},#{employee.email || ""},#{employee.phone || ""},#{employee.business.name},#{employee.status},#{employee.role}\n"
      end)
      |> Enum.join("")

    # Combine all parts
    header <> client_rows <> employee_rows
  end
end
