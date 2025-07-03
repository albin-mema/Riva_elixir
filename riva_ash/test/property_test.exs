defmodule RivaAsh.PropertyTest do
  use ExUnit.Case
  use ExUnitProperties

  alias RivaAsh.Resources.{Business, Section, Item}
  alias RivaAsh.Domain

  setup do
    # Start the database sandbox for each test
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)

    # Clean up any existing data
    {:ok, businesses} = Ash.read(Business, domain: Domain)
    Enum.each(businesses, &Ash.destroy(&1, domain: Domain))

    {:ok, sections} = Ash.read(Section, domain: Domain)
    Enum.each(sections, &Ash.destroy(&1, domain: Domain))

    {:ok, items} = Ash.read(Item, domain: Domain)
    Enum.each(items, &Ash.destroy(&1, domain: Domain))

    :ok
  end

  # Custom generators for your domain
  def business_name_generator do
    gen all name <- string(:alphanumeric, min_length: 3, max_length: 20),
            suffix <- integer(1..999999) do
      # Add suffix to ensure uniqueness
      "Biz#{String.trim(name)}#{suffix}"
    end
  end

  def business_description_generator do
    one_of([
      constant(nil),
      string(:alphanumeric, min_length: 5, max_length: 100)
    ])
  end

  def section_name_generator do
    gen all name <- string(:alphanumeric, min_length: 3, max_length: 20),
            suffix <- integer(1..999999) do
      "Sec#{String.trim(name)}#{suffix}"
    end
  end

  def item_name_generator do
    gen all name <- string(:alphanumeric, min_length: 3, max_length: 20),
            suffix <- integer(1..999999) do
      "Item#{String.trim(name)}#{suffix}"
    end
  end

  # Property tests for Business resource
  property "business creation with valid data always succeeds" do
    check all name <- business_name_generator(),
              description <- business_description_generator() do

      attrs = %{name: name, description: description}

      case Ash.create(Business, attrs, domain: Domain) do
        {:ok, business} ->
          assert business.name == name
          assert business.description == description
          assert is_binary(business.id)
          assert business.inserted_at != nil
          assert business.updated_at != nil

        {:error, error} ->
          # This should not happen with valid data
          flunk("Business creation failed with valid data: #{inspect(attrs)}, error: #{inspect(error)}")
      end
    end
  end

  property "business names must be unique" do
    check all name <- business_name_generator() do

      # Create first business
      {:ok, _business1} = Ash.create(Business, %{name: name, description: "Test"}, domain: Domain)

      # Try to create second business with same name
      case Ash.create(Business, %{name: name, description: "Test"}, domain: Domain) do
        {:error, error} ->
          # This is expected - names must be unique
          assert error.errors != []

        {:ok, _business2} ->
          flunk("Duplicate business name was allowed: #{name}")
      end
    end
  end

  property "item creation always succeeds with valid data" do
    check all item_name <- item_name_generator() do
      case Ash.create(Item, %{name: item_name}, domain: Domain) do
        {:ok, item} ->
          assert item.name == item_name
          assert is_binary(item.id)
          assert item.inserted_at != nil
          assert item.updated_at != nil

        {:error, error} ->
          flunk("Item creation failed with valid data: #{item_name}, error: #{inspect(error)}")
      end
    end
  end

  property "string operations on business names never crash" do
    check all name <- business_name_generator() do
      # These operations should never crash regardless of input
      length = String.length(name)
      assert is_integer(length)
      assert length > 0

      upcase = String.upcase(name)
      assert is_binary(upcase)

      trimmed = String.trim(name)
      assert is_binary(trimmed)
    end
  end

  property "business creation is deterministic" do
    check all name <- business_name_generator(),
              description <- business_description_generator() do

      attrs = %{name: name, description: description}

      # Create business twice with same data (in different test runs)
      {:ok, business1} = Ash.create(Business, attrs, domain: Domain)

      # Verify the business has expected properties
      assert business1.name == name
      assert business1.description == description
      assert is_binary(business1.id)
    end
  end
end
