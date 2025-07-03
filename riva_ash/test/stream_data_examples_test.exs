defmodule RivaAsh.StreamDataExamplesTest do
  use ExUnit.Case
  use ExUnitProperties

  # Basic StreamData generators demonstration
  
  property "strings are always strings" do
    check all str <- string(:alphanumeric) do
      assert is_binary(str)
    end
  end

  property "integers in range behave correctly" do
    check all num <- integer(1..100) do
      assert num >= 1
      assert num <= 100
      assert is_integer(num)
    end
  end

  property "lists have expected properties" do
    check all list <- list_of(integer(), min_length: 0, max_length: 10) do
      assert is_list(list)
      assert length(list) <= 10
      assert Enum.all?(list, &is_integer/1)
    end
  end

  # Custom generators
  def email_generator do
    gen all username <- string(:alphanumeric, min_length: 1, max_length: 20),
            domain <- string(:alphanumeric, min_length: 1, max_length: 15),
            tld <- member_of(["com", "org", "net", "edu"]) do
      "#{username}@#{domain}.#{tld}"
    end
  end

  property "custom email generator creates valid-looking emails" do
    check all email <- email_generator() do
      assert String.contains?(email, "@")
      assert String.contains?(email, ".")
      assert String.length(email) > 5
    end
  end

  # Complex data structures
  def user_generator do
    gen all name <- string(:alphanumeric, min_length: 1, max_length: 50),
            age <- integer(18..100),
            email <- email_generator(),
            active <- boolean() do
      %{
        name: name,
        age: age,
        email: email,
        active: active
      }
    end
  end

  property "user generator creates valid user maps" do
    check all user <- user_generator() do
      assert is_map(user)
      assert Map.has_key?(user, :name)
      assert Map.has_key?(user, :age)
      assert Map.has_key?(user, :email)
      assert Map.has_key?(user, :active)
      
      assert is_binary(user.name)
      assert is_integer(user.age)
      assert user.age >= 18 and user.age <= 100
      assert is_binary(user.email)
      assert is_boolean(user.active)
    end
  end

  # Testing edge cases
  property "string operations handle edge cases" do
    check all str <- string(:printable) do
      # String.length should never crash
      length = String.length(str)
      assert is_integer(length)
      assert length >= 0
      
      # String.trim should never crash
      trimmed = String.trim(str)
      assert is_binary(trimmed)
      
      # String.upcase should never crash
      upcased = String.upcase(str)
      assert is_binary(upcased)
    end
  end

  # Testing mathematical properties
  property "addition is commutative" do
    check all a <- integer(),
              b <- integer() do
      assert a + b == b + a
    end
  end

  property "multiplication by zero always gives zero" do
    check all num <- integer() do
      assert num * 0 == 0
      assert 0 * num == 0
    end
  end

  # Testing list operations
  property "reversing a list twice gives original list" do
    check all list <- list_of(integer()) do
      assert list |> Enum.reverse() |> Enum.reverse() == list
    end
  end

  property "length is preserved when sorting" do
    check all list <- list_of(integer()) do
      sorted = Enum.sort(list)
      assert length(list) == length(sorted)
    end
  end

  property "sorted lists are actually sorted" do
    check all list <- list_of(integer()) do
      sorted = Enum.sort(list)
      
      # Check if sorted (each element <= next element)
      is_sorted = sorted
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.all?(fn [a, b] -> a <= b end)
      
      assert is_sorted
    end
  end

  # Testing with filters
  property "filtered generators work correctly" do
    positive_integers = integer() |> filter(&(&1 > 0))
    
    check all num <- positive_integers do
      assert num > 0
    end
  end

  # Testing with frequency (weighted choices)
  property "frequency generator respects weights" do
    # This will generate mostly small numbers, occasionally large ones
    weighted_generator = frequency([
      {8, integer(1..10)},      # 80% chance of 1-10
      {2, integer(100..1000)}   # 20% chance of 100-1000
    ])
    
    check all num <- weighted_generator do
      assert (num >= 1 and num <= 10) or (num >= 100 and num <= 1000)
    end
  end

  # Testing shrinking behavior
  property "demonstrates shrinking on failure" do
    check all list <- list_of(integer(), min_length: 1) do
      # This will fail and show how StreamData shrinks to find minimal failing case
      # Uncomment the next line to see shrinking in action:
      # assert length(list) < 1  # This will always fail
      
      # For now, let's make it pass
      assert length(list) >= 1
    end
  end
end
