defmodule RivaAsh.WorkingPropertyTest do
  use ExUnit.Case
  use ExUnitProperties

  # This demonstrates StreamData working properly without database dependencies

  # Custom generators for your business domain
  def business_name_generator do
    gen all name <- string(:alphanumeric, min_length: 3, max_length: 10),
            suffix <- integer(1..99999) do
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
    gen all name <- string(:alphanumeric, min_length: 3, max_length: 10),
            suffix <- integer(1..99999) do
      "Sec#{String.trim(name)}#{suffix}"
    end
  end

  def item_name_generator do
    gen all name <- string(:alphanumeric, min_length: 3, max_length: 10),
            suffix <- integer(1..99999) do
      "Item#{String.trim(name)}#{suffix}"
    end
  end

  # Property tests for business logic validation
  property "business names are always valid strings" do
    check all name <- business_name_generator() do
      # Properties that should always be true
      assert is_binary(name)
      assert String.length(name) > 5  # "Biz" + at least 3 chars + number
      assert String.starts_with?(name, "Biz")
      assert String.match?(name, ~r/^Biz.*\d+$/)  # Starts with Biz, ends with digits
    end
  end

  property "business descriptions are either nil or valid strings" do
    check all description <- business_description_generator() do
      case description do
        nil ->
          assert is_nil(description)

        desc when is_binary(desc) ->
          assert String.length(desc) >= 5
          assert String.length(desc) <= 100

        _ ->
          flunk("Description should be nil or string, got: #{inspect(description)}")
      end
    end
  end

  property "string operations never crash on generated names" do
    check all name <- business_name_generator() do
      # These operations should never crash regardless of input
      length = String.length(name)
      assert is_integer(length)
      assert length > 0

      upcase = String.upcase(name)
      assert is_binary(upcase)
      assert String.length(upcase) == length

      downcase = String.downcase(name)
      assert is_binary(downcase)
      assert String.length(downcase) == length

      trimmed = String.trim(name)
      assert is_binary(trimmed)

      # Reversing twice should give original
      reversed_twice = name |> String.reverse() |> String.reverse()
      assert reversed_twice == name
    end
  end

  property "business name validation logic" do
    check all name <- business_name_generator() do
      # Simulate your business validation logic
      is_valid = validate_business_name(name)

      # All generated names should be valid according to our rules
      assert is_valid == true
    end
  end

  property "section names follow expected pattern" do
    check all section_name <- section_name_generator() do
      assert is_binary(section_name)
      assert String.starts_with?(section_name, "Sec")
      assert String.match?(section_name, ~r/^Sec.*\d+$/)
      assert String.length(section_name) > 5
    end
  end

  property "item names are unique when generated" do
    check all items <- list_of(item_name_generator(), min_length: 1, max_length: 10) do
      # Since we include random suffixes, all generated names should be unique
      unique_items = Enum.uniq(items)
      assert length(unique_items) == length(items)
    end
  end

  property "business data structure creation" do
    check all name <- business_name_generator(),
              description <- business_description_generator() do

      # Create a business-like data structure
      business = %{
        name: name,
        description: description,
        id: Ecto.UUID.generate(),
        inserted_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now()
      }

      # Verify the structure
      assert Map.has_key?(business, :name)
      assert Map.has_key?(business, :description)
      assert Map.has_key?(business, :id)
      assert business.name == name
      assert business.description == description
      assert is_binary(business.id)
    end
  end

  property "list operations preserve properties" do
    check all names <- list_of(business_name_generator(), min_length: 0, max_length: 5) do
      # Length is preserved when sorting
      sorted = Enum.sort(names)
      assert length(sorted) == length(names)

      # All elements are still valid business names
      Enum.each(sorted, fn name ->
        assert String.starts_with?(name, "Biz")
        assert is_binary(name)
      end)

      # Reversing twice gives original order
      reversed_twice = names |> Enum.reverse() |> Enum.reverse()
      assert reversed_twice == names
    end
  end

  property "mathematical properties hold" do
    check all a <- integer(1..1000),
              b <- integer(1..1000) do

      # Addition is commutative
      assert a + b == b + a

      # Multiplication by 1 is identity
      assert a * 1 == a
      assert 1 * a == a

      # Multiplication by 0 gives 0
      assert a * 0 == 0
      assert 0 * a == 0
    end
  end

  # Helper function to simulate business validation
  defp validate_business_name(name) when is_binary(name) do
    String.length(name) > 5 and
    String.starts_with?(name, "Biz") and
    String.match?(name, ~r/^Biz.*\d+$/)
  end

  defp validate_business_name(_), do: false
end
