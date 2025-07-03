defmodule RivaAsh.Reactors.ReservationReactor do
  @moduledoc """
  Reactor for handling complex reservation creation with validation and rollback.
  
  This reactor demonstrates:
  1. Reading existing resources
  2. Validating availability
  3. Creating reservations within transactions
  4. Handling failures with proper cleanup
  """

  use Ash.Reactor

  ash do
    default_domain RivaAsh.Domain
  end

  # Define inputs for the reactor
  input :client_id
  input :employee_id
  input :item_id
  input :start_datetime
  input :end_datetime
  input :notes

  # Step 1: Validate that the client exists
  read_one :get_client, RivaAsh.Resources.Client do
    inputs %{id: input(:client_id)}
    fail_on_not_found? true
  end

  # Step 2: Validate that the employee exists
  read_one :get_employee, RivaAsh.Resources.Employee do
    inputs %{id: input(:employee_id)}
    fail_on_not_found? true
  end

  # Step 3: Validate that the item exists
  read_one :get_item, RivaAsh.Resources.Item do
    inputs %{id: input(:item_id)}
    fail_on_not_found? true
  end

  # Step 4: Check availability (this would use your availability module)
  step :check_availability do
    argument :item_id, input(:item_id)
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)
    
    run fn %{item_id: item_id, start_datetime: start_dt, end_datetime: end_dt}, _context ->
      case RivaAsh.Availability.check_availability(item_id, start_dt, end_dt) do
        {:ok, :available} -> {:ok, :available}
        {:ok, :partial, capacity} -> {:ok, {:partial, capacity}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  # Step 5: Create the reservation within a transaction
  transaction :create_reservation_transaction, [RivaAsh.Resources.Reservation] do
    create :create_reservation, RivaAsh.Resources.Reservation do
      inputs %{
        client_id: input(:client_id),
        employee_id: input(:employee_id),
        item_id: input(:item_id),
        start_datetime: input(:start_datetime),
        end_datetime: input(:end_datetime),
        notes: input(:notes),
        status: "confirmed"
      }
      
      # Wait for availability check to complete
      wait_for [:check_availability]
      
      # Enable undo for this step
      undo :outside_transaction
      undo_action :destroy
    end

    # Return the created reservation from the transaction
    return :create_reservation
  end

  # Return the final reservation
  return :create_reservation_transaction
end
