defmodule RivaAsh.Reactors.SimpleTestReactor do
  @moduledoc """
  A simple test reactor to demonstrate diagram generation.
  This reactor shows a basic workflow for creating a business with sections and items.
  """

  use Ash.Reactor

  ash do
    default_domain(RivaAsh.Domain)
  end

  # Define inputs for the reactor
  input(:business_name)
  input(:section_name)
  input(:item_name)

  # Step 1: Create a business
  create :create_business, RivaAsh.Resources.Business do
    inputs(
      template: [
        name: input(:business_name)
      ]
    )
  end

  # Step 2: Create a section within the business
  create :create_section, RivaAsh.Resources.Section do
    inputs(
      template: [
        name: input(:section_name),
        business_id: result(:create_business, [:id])
      ]
    )
  end

  # Step 3: Create an item within the section
  create :create_item, RivaAsh.Resources.Item do
    inputs(
      template: [
        name: input(:item_name),
        section_id: result(:create_section, [:id])
      ]
    )
  end

  # Return the created item as the final result
  return(:create_item)
end
