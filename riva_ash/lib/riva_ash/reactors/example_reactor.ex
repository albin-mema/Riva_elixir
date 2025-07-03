defmodule RivaAsh.Reactors.ExampleReactor do
  @moduledoc """
  Example reactor demonstrating Ash.Reactor usage.
  
  This reactor shows how to:
  1. Create a business
  2. Create a section within that business
  3. Create an item within that section
  4. Handle failures with undo actions
  """

  use Ash.Reactor

  ash do
    default_domain RivaAsh.Domain
  end

  # Define inputs for the reactor
  input :business_name
  input :business_description
  input :section_name
  input :section_description
  input :item_name
  input :item_description
  input :item_capacity

  # Step 1: Create a business
  create :create_business, RivaAsh.Resources.Business do
    inputs %{
      name: input(:business_name),
      description: input(:business_description)
    }
    
    # Enable undo for this step
    undo :outside_transaction
    undo_action :destroy
  end

  # Step 2: Create a section within the business
  create :create_section, RivaAsh.Resources.Section do
    inputs %{
      name: input(:section_name),
      description: input(:section_description),
      business_id: result(:create_business, [:id])
    }
    
    # Enable undo for this step
    undo :outside_transaction
    undo_action :destroy
  end

  # Step 3: Create an item within the section
  create :create_item, RivaAsh.Resources.Item do
    inputs %{
      name: input(:item_name),
      description: input(:item_description),
      capacity: input(:item_capacity),
      section_id: result(:create_section, [:id])
    }
    
    # Enable undo for this step
    undo :outside_transaction
    undo_action :destroy
  end

  # Return the created item as the final result
  return :create_item
end
