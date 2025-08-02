defmodule RivaAsh.StorybookTesting.PropertyGeneratorsTest do
  @moduledoc """
  Tests for the property-based testing generators.
  
  This ensures our property generators work correctly and produce
  valid data for component testing.
  """
  
  use ExUnit.Case, async: true
  use ExUnitProperties
  
  import RivaAsh.StorybookTesting.PropertyGenerators
  
  describe "Button Property Generator" do
    property "generates valid button props" do
      check all(
        props <- button_props(),
        max_runs: 50
      ) do
        # Verify all required fields are present
        assert Map.has_key?(props, :variant)
        assert Map.has_key?(props, :size)
        assert Map.has_key?(props, :disabled)
        assert Map.has_key?(props, :loading)
        assert Map.has_key?(props, :text)
        assert Map.has_key?(props, :type)
        assert Map.has_key?(props, :class)
        
        # Verify field types and constraints
        assert is_binary(props.variant)
        assert props.variant in ~w(primary secondary destructive outline ghost link)
        
        assert is_binary(props.size)
        assert props.size in ~w(sm md lg xl)
        
        assert is_boolean(props.disabled)
        assert is_boolean(props.loading)
        
        assert is_binary(props.text)
        assert String.length(props.text) >= 1
        assert String.length(props.text) <= 50
        
        assert is_binary(props.type)
        assert props.type in ~w(button submit reset)
        
        assert is_binary(props.class)
      end
    end
  end
  
  describe "Input Property Generator" do
    property "generates valid input props" do
      check all(
        props <- input_props(),
        max_runs: 50
      ) do
        # Verify all required fields are present
        assert Map.has_key?(props, :type)
        assert Map.has_key?(props, :placeholder)
        assert Map.has_key?(props, :value)
        assert Map.has_key?(props, :disabled)
        assert Map.has_key?(props, :readonly)
        assert Map.has_key?(props, :required)
        assert Map.has_key?(props, :size)
        assert Map.has_key?(props, :variant)
        assert Map.has_key?(props, :class)
        
        # Verify field types and constraints
        assert is_binary(props.type)
        assert props.type in ~w(text email password number tel url search date time)
        
        assert is_binary(props.placeholder)
        assert String.length(props.placeholder) <= 100
        
        assert props.value == nil or is_binary(props.value)
        if props.value, do: assert(String.length(props.value) <= 200)
        
        assert is_boolean(props.disabled)
        assert is_boolean(props.readonly)
        assert is_boolean(props.required)
        
        assert is_binary(props.size)
        assert props.size in ~w(sm md lg)
        
        assert is_binary(props.variant)
        assert props.variant in ~w(default error success)
        
        assert is_binary(props.class)
      end
    end
  end
  
  describe "Text Property Generator" do
    property "generates valid text props" do
      check all(
        props <- text_props(),
        max_runs: 50
      ) do
        # Verify all required fields are present
        assert Map.has_key?(props, :variant)
        assert Map.has_key?(props, :text)
        assert Map.has_key?(props, :class)
        
        # Verify field types and constraints
        assert is_binary(props.variant)
        assert props.variant in ~w(h1 h2 h3 h4 h5 h6 p span small lead muted)
        
        assert is_binary(props.text)
        assert String.length(props.text) >= 1
        assert String.length(props.text) <= 200
        
        assert is_binary(props.class)
      end
    end
  end
  
  describe "Badge Property Generator" do
    property "generates valid badge props" do
      check all(
        props <- badge_props(),
        max_runs: 50
      ) do
        # Verify all required fields are present
        assert Map.has_key?(props, :variant)
        assert Map.has_key?(props, :text)
        assert Map.has_key?(props, :class)
        
        # Verify field types and constraints
        assert is_binary(props.variant)
        assert props.variant in ~w(default secondary destructive outline)
        
        assert is_binary(props.text)
        assert String.length(props.text) >= 1
        assert String.length(props.text) <= 20
        
        assert is_binary(props.class)
      end
    end
  end
  
  describe "Icon Property Generator" do
    property "generates valid icon props" do
      check all(
        props <- icon_props(),
        max_runs: 50
      ) do
        # Verify all required fields are present
        assert Map.has_key?(props, :name)
        assert Map.has_key?(props, :variant)
        assert Map.has_key?(props, :size)
        assert Map.has_key?(props, :class)
        
        # Verify field types and constraints
        assert is_atom(props.name)
        
        assert is_binary(props.variant)
        assert props.variant in ~w(outline solid mini micro)
        
        assert is_binary(props.size)
        assert props.size in ~w(xs sm md lg xl)
        
        assert is_binary(props.class)
      end
    end
  end
  
  describe "Edge Case Generator" do
    property "generates valid edge case props for buttons" do
      check all(
        props <- edge_case_props(:button),
        max_runs: 20
      ) do
        # Should have text and variant fields
        assert Map.has_key?(props, :text)
        assert Map.has_key?(props, :variant)
        
        # Text can be any string (including empty, very long, or special chars)
        assert is_binary(props.text)
        
        # Variant can be valid or invalid
        assert is_binary(props.variant)
      end
    end
    
    property "generates valid edge case props for inputs" do
      check all(
        props <- edge_case_props(:input),
        max_runs: 20
      ) do
        # Should have value and type fields
        assert Map.has_key?(props, :value)
        assert Map.has_key?(props, :type)
        
        # Value can be any string (including potentially malicious content)
        assert is_binary(props.value)
        
        # Type can be valid or invalid
        assert is_binary(props.type)
      end
    end
  end
  
  describe "CSS Class Generator" do
    property "generates valid CSS classes" do
      check all(
        css_class <- css_class_generator(),
        max_runs: 30
      ) do
        assert is_binary(css_class)
        
        # Should not contain dangerous characters
        refute css_class =~ ~r/[<>'"]/
        
        # Should be reasonable length
        assert String.length(css_class) <= 200
      end
    end
  end
  
  describe "Component Test Case Generator" do
    test "generates test cases for all component types" do
      component_types = [:button, :input, :text, :badge, :icon, :card, :tooltip]
      
      Enum.each(component_types, fn component_type ->
        # Generate a single test case
        test_case = component_test_case(component_type) |> Enum.take(1) |> hd()
        
        # Should be a map
        assert is_map(test_case)
        
        # Should have at least one key
        assert map_size(test_case) > 0
        
        IO.puts("✅ Generated #{component_type} test case: #{inspect(Map.keys(test_case))}")
      end)
    end
    
    test "generates test batches" do
      batch = component_test_batch(:button, 5) |> Enum.take(1) |> hd()
      
      # Should be a list of 5 test cases
      assert is_list(batch)
      assert length(batch) == 5
      
      # Each item should be a map
      Enum.each(batch, fn test_case ->
        assert is_map(test_case)
        assert map_size(test_case) > 0
      end)
      
      IO.puts("✅ Generated batch of #{length(batch)} button test cases")
    end
  end
  
  describe "Property Testing Statistics" do
    test "property generators produce diverse outputs" do
      # Generate multiple button props and check for diversity
      button_variants = 
        button_props()
        |> Enum.take(20)
        |> Enum.map(& &1.variant)
        |> Enum.uniq()
      
      # Should generate multiple different variants
      assert length(button_variants) > 1
      
      IO.puts("✅ Generated #{length(button_variants)} unique button variants: #{inspect(button_variants)}")
      
      # Generate multiple input types and check for diversity
      input_types = 
        input_props()
        |> Enum.take(20)
        |> Enum.map(& &1.type)
        |> Enum.uniq()
      
      # Should generate multiple different types
      assert length(input_types) > 1
      
      IO.puts("✅ Generated #{length(input_types)} unique input types: #{inspect(input_types)}")
    end
  end
end
