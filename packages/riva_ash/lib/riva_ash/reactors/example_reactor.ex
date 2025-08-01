defmodule RivaAsh.Reactors.ExampleReactor do
  @moduledoc """
  Example reactor that demonstrates creating a complete business setup
  with business, section, and item in a transactional manner.

  This reactor creates:
  1. A business with the provided name and description
  2. A section within that business
  3. An item within that section

  If any step fails, all previous steps are rolled back.
  """

  use Reactor

  alias RivaAsh.Resources.{Business, Section, Item, ItemType}

  # Define the reactor inputs
  input(:business_name)
  input(:business_description)
  input(:section_name)
  input(:section_description)
  input(:item_name)
  input(:item_description)
  input(:item_capacity)

  # Step 1: Create the business
  step :create_business do
    argument(:name, input(:business_name))
    argument(:description, input(:business_description))

    run(fn %{name: name, description: description}, _context ->
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: name,
        description: description
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end)

    compensate(fn business, _context ->
      Business.destroy!(business, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Step 2: Create a default item type for the business
  step :create_item_type do
    argument(:business_id, result(:create_business, [:id]))

    run(fn %{business_id: business_id}, _context ->
      ItemType
      |> Ash.Changeset.for_create(:create, %{
        name: "Default Type",
        description: "Default item type for reactor demo",
        business_id: business_id
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end)

    compensate(fn item_type, _context ->
      ItemType.destroy!(item_type, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Step 3: Create the section
  step :create_section do
    argument(:business_id, result(:create_business, [:id]))
    argument(:name, input(:section_name))
    argument(:description, input(:section_description))

    run(fn %{business_id: business_id, name: name, description: description}, _context ->
      Section
      |> Ash.Changeset.for_create(:create, %{
        name: name,
        description: description,
        business_id: business_id
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end)

    compensate(fn section, _context ->
      Section.destroy!(section, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Step 4: Create the item
  step :create_item do
    argument(:section_id, result(:create_section, [:id]))
    argument(:item_type_id, result(:create_item_type, [:id]))
    argument(:name, input(:item_name))
    argument(:description, input(:item_description))
    argument(:capacity, input(:item_capacity))

    run(fn %{
             section_id: section_id,
             item_type_id: item_type_id,
             name: name,
             description: description,
             capacity: capacity
           },
           _context ->
      Item
      |> Ash.Changeset.for_create(:create, %{
        name: name,
        description: description,
        capacity: capacity,
        section_id: section_id,
        item_type_id: item_type_id
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end)

    compensate(fn item, _context ->
      Item.destroy!(item, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Return the created item as the final result
  return(:create_item)
end
