#!/usr/bin/env elixir

# test_business_form.exs
# A script to test the business form functionality

# Add the application code to the code path
Code.prepend_path("_build/dev/lib/riva_ash/ebin")
Code.prepend_path("_build/dev/lib/ash/ebin")
Code.prepend_path("_build/dev/lib/ash_phoenix/ebin")

# Start required applications
Application.ensure_all_started(:riva_ash)

alias RivaAsh.Resources.Business
alias RivaAsh.Accounts.User
alias AshPhoenix.Form

# Create a mock user for testing
mock_user = %User{
  id: "11111111-1111-1111-1111-111111111111",
  name: "Test User",
  email: "test@example.com",
  role: :user
}

IO.puts("=== BUSINESS FORM TEST ===")
IO.puts("Testing form creation with and without actor")

# Test 1: Create form without actor
IO.puts("\n--- Test 1: Form without actor ---")
try do
  form = Business
         |> AshPhoenix.Form.for_create(:create)
         |> Map.put(:action, :validate)
  
  IO.puts("Form created successfully without actor")
  IO.puts("Form valid? #{form.valid?}")
  
  # Try to submit the form
  params = %{"name" => "Test Business", "description" => ""}
  case AshPhoenix.Form.submit(form, params: params) do
    {:ok, business} ->
      IO.puts("Form submitted successfully without actor")
      IO.inspect(business, label: "Created business")
    {:error, form_with_errors} ->
      IO.puts("Form submission failed without actor")
      IO.inspect(form_with_errors.errors, label: "Errors")
  end
rescue
  e ->
    IO.puts("Error creating form without actor: #{inspect(e)}")
end

# Test 2: Create form with actor
IO.puts("\n--- Test 2: Form with actor ---")
try do
  form = Business
         |> AshPhoenix.Form.for_create(:create, actor: mock_user)
         |> Map.put(:action, :validate)
  
  IO.puts("Form created successfully with actor")
  IO.puts("Form valid? #{form.valid?}")
  
  # Try to submit the form
  params = %{"name" => "Test Business", "description" => ""}
  case AshPhoenix.Form.submit(form, params: params, actor: mock_user) do
    {:ok, business} ->
      IO.puts("Form submitted successfully with actor")
      IO.inspect(business, label: "Created business")
    {:error, form_with_errors} ->
      IO.puts("Form submission failed with actor")
      IO.inspect(form_with_errors.errors, label: "Errors")
  end
rescue
  e ->
    IO.puts("Error creating form with actor: #{inspect(e)}")
end

# Test 3: Check if to_form() causes any issues
IO.puts("\n--- Test 3: Form with to_form() ---")
try do
  form = Business
         |> AshPhoenix.Form.for_create(:create, actor: mock_user)
         |> Map.put(:action, :validate)
         |> Phoenix.Component.to_form()
  
  IO.puts("Form with to_form() created successfully")
  IO.inspect(form.source.submit_errors, label: "Initial errors")
rescue
  e ->
    IO.puts("Error creating form with to_form(): #{inspect(e)}")
end

IO.puts("\n=== TEST COMPLETE ===")