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

  require Logger
  alias RivaAsh.Resources.{Business, Section, Item, ItemType}
  alias RivaAsh.Config.AppConfig

  @type business_attrs :: %{
          required(:name) => String.t(),
          optional(:description) => String.t()
        }

  @type section_attrs :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          required(:business_id) => integer()
        }

  @type item_attrs :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          required(:capacity) => integer(),
          required(:section_id) => integer(),
          required(:item_type_id) => integer()
        }

  @type result :: Item.t()

  @spec create_business(business_attrs()) :: {:ok, Business.t()} | {:error, String.t()}
  @spec create_item_type(integer()) :: {:ok, ItemType.t()} | {:error, String.t()}
  @spec create_section(section_attrs()) :: {:ok, Section.t()} | {:error, String.t()}
  @spec create_item(item_attrs()) :: {:ok, Item.t()} | {:error, String.t()}

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
      Logger.info("Starting business creation with name: #{name}")
      
      with :ok <- validate_business_name(name),
           {:ok, business} <- do_create_business(name, description) do
        Logger.info("Business created successfully: #{business.id}")
        {:ok, business}
      else
        {:error, reason} ->
          Logger.error("Business creation failed: #{inspect(reason)}")
          {:error, reason}
      end
    end)

    compensate(fn business, _context ->
      Logger.warning("Compensating business creation for business: #{business.id}")
      Business.destroy!(business, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Helper functions for business creation
  defp validate_business_name(name) when is_binary(name) and byte_size(name) > 0 do
    Logger.debug("Business name validation passed: #{name}")
    :ok
  end

  defp validate_business_name(_name) do
    Logger.debug("Business name validation failed: missing or invalid name")
    {:error, "Business name is required and must be a non-empty string"}
  end

  defp do_create_business(name, description) do
    Logger.debug("Creating business with name: #{name}")
    
    business_attrs = %{
      name: name,
      description: description || ""
    }

    result =
      Business
      |> Ash.Changeset.for_create(:create, business_attrs)
      |> Ash.create(domain: RivaAsh.Domain)

    case result do
      {:ok, business} ->
        Logger.debug("Business created successfully with ID: #{business.id}")
        {:ok, business}
      {:error, changeset} ->
        Logger.error("Business creation failed: #{inspect(changeset)}")
        {:error, "Failed to create business: #{format_changeset_errors(changeset)}"}
    end
  end

  # Helper function to format changeset errors
  defp format_changeset_errors(changeset) do
    changeset
    |> Ash.Changeset.traverse_errors(fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
    |> Enum.map(fn {field, errors} -> "#{field}: #{Enum.join(errors, ", ")}" end)
    |> Enum.join("; ")
  end

  # Step 2: Create a default item type for the business
  step :create_item_type do
    argument(:business_id, result(:create_business, [:id]))

    run(fn %{business_id: business_id}, _context ->
      Logger.info("Creating default item type for business: #{business_id}")
      
      result =
        ItemType
        |> Ash.Changeset.for_create(:create, %{
          name: "Default Type",
          description: "Default item type for reactor demo",
          business_id: business_id
        })
        |> Ash.create(domain: RivaAsh.Domain)

      case result do
        {:ok, item_type} ->
          Logger.info("Item type created successfully: #{item_type.id}")
          {:ok, item_type}
        {:error, changeset} ->
          Logger.error("Item type creation failed: #{inspect(changeset)}")
          {:error, "Failed to create item type: #{format_changeset_errors(changeset)}"}
      end
    end)

    compensate(fn item_type, _context ->
      Logger.warning("Compensating item type creation for item type: #{item_type.id}")
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
      Logger.info("Creating section for business: #{business_id}")
      
      result =
        Section
        |> Ash.Changeset.for_create(:create, %{
          name: name,
          description: description,
          business_id: business_id
        })
        |> Ash.create(domain: RivaAsh.Domain)

      case result do
        {:ok, section} ->
          Logger.info("Section created successfully: #{section.id}")
          {:ok, section}
        {:error, changeset} ->
          Logger.error("Section creation failed: #{inspect(changeset)}")
          {:error, "Failed to create section: #{format_changeset_errors(changeset)}"}
      end
    end)

    compensate(fn section, _context ->
      Logger.warning("Compensating section creation for section: #{section.id}")
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
      Logger.info("Creating item with name: #{name}")
      
      result =
        Item
        |> Ash.Changeset.for_create(:create, %{
          name: name,
          description: description,
          capacity: capacity,
          section_id: section_id,
          item_type_id: item_type_id
        })
        |> Ash.create(domain: RivaAsh.Domain)

      case result do
        {:ok, item} ->
          Logger.info("Item created successfully: #{item.id}")
          {:ok, item}
        {:error, changeset} ->
          Logger.error("Item creation failed: #{inspect(changeset)}")
          {:error, "Failed to create item: #{format_changeset_errors(changeset)}"}
      end
    end)

    compensate(fn item, _context ->
      Logger.warning("Compensating item creation for item: #{item.id}")
      Item.destroy!(item, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Return the created item as the final result
  return(:create_item)
end
